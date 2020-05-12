/*
Assumptions:
  1. Each bank stores output window of one channel.
  2. Every bank stores output window corresponding to different channels.
  3. A store request will require the module to store data from bank 0 to nCol-1.
     Note that, an offset (rg_buffer_base) is specified which indicates the starting
     accumindex.
  4. Parameter output_window_col_size < 256. If not, then multiple burst req will have to
     be generated for a single output row.

TODO_List:
1. --Done-- Current assumption is that BRAM read req and AXI write req can never stall. Fix that.
2. --Done-- Reduce number of multipliers used by reusing the same hardware.
3. --Done-- Change multipliers from single cycle to multicycle, and hide the latencies.
4. --Done-- Reduce the number of adders required.


Interface Parameters:
  1. addr_width     : Width of AXI4 Addr bus
  2. data_width     : Width of AXI4 Data bus
  3. nCol           : Num of systolic array columns (which is equal to no. output buffer banks)
  4. mulWidth2      : Size of one element in the output buffer
  5. nRow           : Num of rows in the systolic array
  6. accumindexbits : Log of depth of each output buffer
  7. convColbits    : Log of number of columns (or rows, whichever is higher) that would be present
                      in the convolution output

Module parameters:
  1. dram_base : Base address from where the store instruction should begin storing
  2. output_window_col_size_minus1 : No. of columns in the output window - 1

Tuning:
  1. Fix the size of the multiplier (currently set based on assumption 5). Note that the order in 
     which you send inputs can also be exchanged so as to declare the most optimally sized multiplier.
*/

package col2im_stride;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import Semi_FIFOF::*;
  import ConfigReg::*;
  import UniqueWrappers::*;
  import pipe_mul::*;
  `define Buffer_wreq_id 4

  typedef enum {Compute_conv_output_size, Compute_awlen, Pre_active, Active_new_burst, Active_burst_data, Done}
  Col2im_state deriving(Bits, Eq, FShow);

  interface Ifc_col2im#(numeric type addr_width, numeric type data_width, numeric type nCol,
  numeric type mulWidth2, numeric type nRow, numeric type accumindexbits, numeric type convColbits);
    method Tuple3#(Bool,Bit#(TLog#(nCol)),Bit#(accumindexbits)) outp_buffer_addr;
    method Action buffer_val(Bit#(mulWidth2) data);
    method Action init(Bit#(accumindexbits) row_index, Bit#(convColbits) conv_output_col_size,
                       Bit#(convColbits) conv_output_row_size, Bool is_output_size_changed);
    method Bool store_done;
    interface AXI4_Master_IFC#(addr_width, data_width, accumindexbits) axi_buffer_wreq;
  endinterface

  function Bit#(a) incr1(Bit#(a) inp);
    return inp+1;
  endfunction

  function Bit#(m) adjust_size(Bit#(n) val);
    Bit#(TAdd#(m,n)) temp_val= zeroExtend(val);
    return truncate(temp_val);
  endfunction

  module mkcol2im#(Bit#(addr_width) dram_base, Bit#(accumindexbits) output_window_col_size,
  Bit#(accumindexbits) output_window_row_size)
  (Ifc_col2im#(addr_width, data_width, nCol, mulWidth2, nRow, accumindexbits, convColbits))
  provisos(Log#(nRow,nRowbits),
           Log#(TDiv#(data_width,8), awsz),
           Log#(nCol, nColbits),
           Max#(accumindexbits, convColbits, local_mul_width),
           Add#(a__, mulWidth2, data_width),
           Add#(b__, nColbits, 8),
           Add#(c__, TAdd#(convColbits, convColbits), addr_width)
           );
    let v_awsize= valueOf(awsz);
    let v_accumbanks= valueOf(nCol);

		AXI4_Master_Xactor_IFC #(addr_width, data_width, accumindexbits) memory_xactor <- mkAXI4_Master_Xactor;

    Reg#(Bit#(nColbits)) rg_bank_index[2] <- mkCReg(2,0);
    Reg#(Bit#(8)) rg_burst_counter[2] <- mkCReg(2,0);
    Reg#(Bit#(accumindexbits)) rg_output_partial_row <- mkReg(0);
    Reg#(Bit#(accumindexbits)) rg_buffer_base <- mkConfigReg(0);
    Reg#(Bit#(convColbits)) rg_conv_output_col_size <- mkReg(0);
    Reg#(Bit#(TAdd#(convColbits, convColbits))) rg_conv_output_total_size <- mkReg(0);
    Reg#(Col2im_state) rg_state <- mkConfigReg(Done);
    Reg#(Bool) rg_mul_done <- mkReg(True);
    Reg#(Bool) rg_mul_compute_channel_offset <- mkRegU();

    Wire#(Bit#(mulWidth2)) wr_buffer_val <- mkWire();

    Ifc_pipe_mul#(local_mul_width, local_mul_width, 1) mul <- mkpipe_mul();
		Wrapper#(Bit#(8), Bit#(8)) wrapper_incr1 <- mkUniqueWrapper(incr1);

    Reg#(Bit#(addr_width)) rg_store_addr <- mkReg(0);
    Reg#(Bit#(8)) rg_awlen <- mkReg(0);

    //This rule waits for the multiplier output to be ready.
    rule rl_compute_conv_output_size(rg_state==Compute_conv_output_size);
      let res= mul.outp;
      rg_conv_output_total_size<= adjust_size(mul.outp);
      rg_state<= Compute_awlen;
      mul.inp(adjust_size(output_window_row_size), adjust_size(output_window_col_size));
    endrule

    rule rl_compute_awlen(rg_state==Compute_awlen);
      rg_awlen<= adjust_size(mul.outp)-1;
      rg_state<= Pre_active;
    endrule

    //One wait cycle to send a BRAM read request, whose output would be required in the next cycle
    rule rl_pre_start(rg_state==Pre_active);
      rg_state<= Active_new_burst;
    endrule

    rule rl_process_buffer_val(rg_state==Active_new_burst);
		  AXI4_Wr_Addr#(addr_width, accumindexbits) aw = AXI4_Wr_Addr { awaddr : rg_store_addr,
      awlen : rg_awlen, awsize : fromInteger(v_awsize), awburst : 'b01, awuser: output_window_row_size,
      awid : `Buffer_wreq_id, awprot:? }; // arburst : 00 - FIXED 01 - INCR 10 - WRAP

      $display($time,"\tNew AXI req addr: %h awlen: %d awuser: %d", rg_store_addr, rg_awlen, output_window_row_size);
  	  let w  = AXI4_Wr_Data {wdata : zeroExtend(wr_buffer_val), wstrb : '1,
      wlast : rg_burst_counter[0]==rg_awlen, wid : `Buffer_wreq_id };
	    memory_xactor.i_wr_addr.enq(aw);
		  memory_xactor.i_wr_data.enq(w);

      rg_burst_counter[0]<= 1;
      rg_state<= Active_burst_data;
    endrule


    rule rl_send_burst_data(rg_state==Active_burst_data);
  	  let w  = AXI4_Wr_Data {wdata : zeroExtend(wr_buffer_val), wstrb : '1,
                             wlast : rg_burst_counter[0]==rg_awlen, 
                             wid : `Buffer_wreq_id };
		  memory_xactor.i_wr_data.enq(w);

      if(rg_burst_counter[0]==rg_awlen) begin   //Burst transfer complete
        rg_burst_counter[0]<= 0;
        if(rg_bank_index[0] == fromInteger(v_accumbanks-1)) begin
          rg_bank_index[0]<= 0;
          rg_state<= Done;
        end
        else begin
          let lv2 <- wrapper_incr1.func(zeroExtend(rg_bank_index[0]));
          rg_bank_index[0]<= truncate(lv2);
          rg_store_addr<= rg_store_addr + zeroExtend(rg_conv_output_total_size);
          rg_state<= Active_new_burst;
        end
      end
      else begin
        let lv2 <- wrapper_incr1.func(zeroExtend(rg_burst_counter[0]));
        rg_burst_counter[0]<= truncate(lv2);
      end
    endrule

    rule rl_get_write_response;
			let x<- pop_o(memory_xactor.o_wr_resp) ;
    endrule

    method Tuple3#(Bool,Bit#(nColbits),Bit#(accumindexbits)) outp_buffer_addr;
      Bool valid_req= False;
      if(rg_state==Pre_active || rg_state==Active_new_burst || rg_state==Active_burst_data)
        valid_req= True;

      return tuple3(valid_req, rg_bank_index[1], (rg_buffer_base + adjust_size(rg_burst_counter[1])));
    endmethod

    method Action buffer_val(Bit#(mulWidth2) data);
      wr_buffer_val<= data;
    endmethod

    method Action init(Bit#(accumindexbits) row_index, Bit#(convColbits) conv_output_row_size,
                       Bit#(convColbits) conv_output_col_size, Bool is_output_size_changed)
                       if(rg_state==Done);
      rg_buffer_base<= row_index;
      rg_conv_output_col_size<= conv_output_col_size;
      if(is_output_size_changed) begin
        mul.inp(adjust_size(conv_output_col_size), adjust_size(conv_output_row_size));
        rg_state<= Compute_conv_output_size;
      end
      else
        rg_state<= Pre_active;
      rg_bank_index[0]<= 'd0;
      rg_burst_counter[0]<= 'd0;
      rg_output_partial_row<= 'd0;
      rg_store_addr<= dram_base;
    endmethod

    method Bool store_done;
      return (rg_state==Done);
    endmethod

    interface axi_buffer_wreq= memory_xactor.axi_side;

  endmodule


  (*synthesize*)
  module mkinst_col2im(Ifc_col2im#(32, 64, 98, 56, 150, 9, 6));
    let ifc();
    mkcol2im#('h80000000, 5, 5) _temp(ifc);
    return ifc;
  endmodule

endpackage
