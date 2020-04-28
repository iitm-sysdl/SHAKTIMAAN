/*
Assumptions:
  1. Each bank stores output window of one channel.
  2. Every bank stores output window corresponding to different channels.
  3. A store request will require the module to store data from bank 0 to nCol-1.
     Note that, an offset (rg_buffer_base) is specified which indicates the starting
     accumindex.
  4. Parameter output_window_col_size_minus1 < 256. If not, then multiple burst req will have to
     be generated for a single output row.
  5. accumindexbits >= nColbits (Can be changed easily)

TODO_List:
1. --Done-- Current assumption is that BRAM read req and AXI write req can never stall. Fix that.
2. --Done-- Reduce number of multipliers used by reusing the same hardware.
3. --Done-- Change multipliers from single cycle to multicycle, and hide the latencies.
4. Reduce the number of adders required.


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

package col2im;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import Semi_FIFOF::*;
  import ConfigReg::*;
  import UniqueWrappers::*;
  import pipe_mul::*;
  `define Buffer_wreq_id 4

  typedef enum {Compute_conv_output_size, Pre_active, Active_new_burst, Active_burst_data, Done}
  Col2im_state deriving(Bits, Eq, FShow);

  interface Ifc_col2im#(numeric type addr_width, numeric type data_width, numeric type nCol,
  numeric type mulWidth2, numeric type nRow, numeric type accumindexbits, numeric type convColbits);
    method Tuple3#(Bool,Bit#(TLog#(nCol)),Bit#(accumindexbits)) outp_buffer_addr;
    method Action buffer_val(Bit#(mulWidth2) data);
    method Action init(Bit#(accumindexbits) row_index, Bit#(convColbits) conv_output_col_size,
                       Bit#(convColbits) conv_output_row_size, Bool is_output_size_changed);
    method Bool is_store_done;
    interface AXI4_Master_IFC#(addr_width, data_width, 0) axi_buffer_wreq;
  endinterface

  function Bit#(a) incr1(Bit#(a) inp);
    return inp+1;
  endfunction

  module mkcol2im#(Bit#(addr_width) dram_base, Bit#(8) output_window_col_size_minus1,
  Bit#(accumindexbits) output_window_row_size_minus1)
  (Ifc_col2im#(addr_width, data_width, nCol, mulWidth2, nRow, accumindexbits, convColbits))
  provisos(Log#(nRow,nRowbits),
           Log#(TDiv#(data_width,8), awsz),
           Log#(nCol, nColbits),
           Add#(a__, mulWidth2, data_width),
           Add#(b__, TAdd#(TAdd#(convColbits, convColbits), nColbits), addr_width),
           Add#(c__, TAdd#(convColbits, accumindexbits), addr_width),
           Add#(d__, convColbits, accumindexbits),    //assumption 5.
           Add#(e__, nColbits, accumindexbits), //assumption 5.
           Max#(accumindexbits, 8, incr_width), //assumption 5. Max(accumindexbits,8,nColbits)= incr_width
           Add#(f__, 8, incr_width),
           Add#(g__, nColbits, incr_width),
           Add#(h__, accumindexbits, incr_width)
           );
    let v_awsize= valueOf(awsz);
    let v_accumbanks= valueOf(nCol);

		AXI4_Master_Xactor_IFC #(addr_width, data_width, 0) memory_xactor <- mkAXI4_Master_Xactor;

    Reg#(Bit#(nColbits)) rg_bank_index <- mkReg(0);
    Reg#(Bit#(accumindexbits)) rg_accum_row_index <- mkReg(0);
    Reg#(Bit#(8)) rg_burst_counter <- mkReg(0);
    Reg#(Bit#(accumindexbits)) rg_output_partial_row <- mkReg(0);
    Reg#(Bit#(accumindexbits)) rg_buffer_base <- mkReg(0);
    Reg#(Bit#(convColbits)) rg_conv_output_col_size <- mkReg(0);
    Reg#(Bit#(TAdd#(convColbits, convColbits))) rg_conv_output_total_size <- mkReg(0);
    Reg#(Bit#(TAdd#(TAdd#(convColbits, convColbits), nColbits))) rg_channel_offset <- mkReg(0);
    Reg#(Bit#(TAdd#(convColbits, accumindexbits))) rg_row_offset <- mkReg(0);
    Reg#(Col2im_state) rg_state <- mkReg(Done);
    Reg#(Bool) rg_mul_done <- mkReg(True);
    Reg#(Bool) rg_mul_compute_channel_offset <- mkRegU();

    Wire#(Bit#(mulWidth2)) wr_buffer_val <- mkWire();

    Ifc_pipe_mul#(TAdd#(convColbits, convColbits), accumindexbits, 2) mul <- mkpipe_mul();
		Wrapper#(Bit#(accumindexbits), Bit#(accumindexbits)) wrapper1_incr1 <- mkUniqueWrapper(incr1);
		Wrapper#(Bit#(incr_width), Bit#(incr_width)) wrapper2_incr1 <- mkUniqueWrapper(incr1);

//Bit#(TAdd#(TAdd#(convColbits, convColbits), nColbits)) channel_offset=
//zeroExtend(rg_conv_output_total_size) * zeroExtend(rg_bank_index);
//Bit#(TAdd#(convColbits, accumindexbits)) row_offset= zeroExtend(rg_conv_output_col_size) *
//                                                     zeroExtend(rg_output_partial_row);

    //This rule waits for the multiplier output to be ready.
    rule rl_compute_conv_output_size(rg_state==Compute_conv_output_size);
      let res= mul.outp;
      rg_conv_output_total_size<= truncate(res);
      rg_state<= Pre_active;
    endrule

    //One wait cycle to send a BRAM read request, whose output would be required in the next cycle
    rule rl_pre_start(rg_state==Pre_active);
      rg_state<= Active_new_burst;
    endrule

    //If the multiplier takes more number of clock cycles than the burst_len, new req will not begin
    //Untill mul is done. This is indicated using the rg_mul_done signal
    rule rl_process_buffer_val(rg_state==Active_new_burst && rg_mul_done);
      Bit#(addr_width) store_addr= dram_base + ((zeroExtend(rg_channel_offset) + zeroExtend(rg_row_offset)) << v_awsize);

		  AXI4_Wr_Addr#(addr_width, 0) aw = AXI4_Wr_Addr {awaddr : store_addr, awuser : 0,
      awlen : output_window_col_size_minus1, awsize : fromInteger(v_awsize), awburst : 'b01,
      awid : `Buffer_wreq_id, awprot:? }; // arburst : 00 - FIXED 01 - INCR 10 - WRAP

  	  let w  = AXI4_Wr_Data {wdata : zeroExtend(wr_buffer_val), wstrb : '1,
                             wlast : rg_burst_counter==output_window_col_size_minus1, 
                             wid : `Buffer_wreq_id };
	    memory_xactor.i_wr_addr.enq(aw);
		  memory_xactor.i_wr_data.enq(w);

      //$display("AXI4 New burst write addr: ", fshow(aw));
      //If partial output is not done, then new rg_row_offset has to be calculated
      if(rg_output_partial_row < output_window_row_size_minus1) begin
        let lv2 <- wrapper2_incr1.func(zeroExtend(rg_output_partial_row));
        rg_output_partial_row<= truncate(lv2);
        mul.inp(zeroExtend(rg_conv_output_col_size), rg_output_partial_row); 
        rg_mul_compute_channel_offset<= False;
      end
      else begin
        mul.inp(rg_conv_output_total_size, zeroExtend(rg_bank_index));
        rg_mul_compute_channel_offset<= True;
        rg_output_partial_row<= 0;
        rg_row_offset<= 0;
      end
      rg_mul_done<= False;
      let lv1 <- wrapper1_incr1.func(rg_accum_row_index);
      rg_accum_row_index<= lv1;
      rg_state<= Active_burst_data;
    endrule

    rule rl_send_burst_data(rg_state==Active_burst_data);
  	  let w  = AXI4_Wr_Data {wdata : zeroExtend(wr_buffer_val), wstrb : '1,
                             wlast : rg_burst_counter==output_window_col_size_minus1, 
                             wid : `Buffer_wreq_id };
		  memory_xactor.i_wr_data.enq(w);
      //$display("AXI4 burst write data: %h", wr_buffer_val);
      //Keep track of how many bursts are done
      if(rg_burst_counter==output_window_col_size_minus1) begin   //Burst transfer complete
        rg_burst_counter<= 0;
        //This block computes the SRAM indices (rg_accum_row_index and rg_bank_index)
        if(rg_mul_compute_channel_offset) begin //Not the last row of output window for one channel
          let lv1 <- wrapper1_incr1.func(rg_accum_row_index);
          rg_accum_row_index<= lv1;
          rg_state<= Active_new_burst;
        end
        else begin  //Last row of output window for one channel
          rg_accum_row_index<= 0;
          //TODO Assuming a store instruction would end at the last bank
          let lv2 <- wrapper2_incr1.func(zeroExtend(rg_bank_index));
          rg_bank_index<= truncate(lv2);
          if(rg_bank_index == fromInteger(v_accumbanks-1))  //Last bank's last row's last burst data
            rg_state<= Done;
          else
            rg_state<= Active_new_burst;
        end
      end
      else begin
        let lv1 <- wrapper1_incr1.func(rg_accum_row_index);
        rg_accum_row_index<= lv1;
        let lv2 <- wrapper2_incr1.func(zeroExtend(rg_burst_counter));
        rg_burst_counter<= truncate(lv2);
      end
    endrule

    rule rl_get_write_response;
			let x<- pop_o(memory_xactor.o_wr_resp) ;
    endrule

    rule rl_update_burst_counter(rg_state==Active_burst_data);
      let res= mul.outp;
      if(rg_mul_compute_channel_offset)
        rg_channel_offset<= truncate(res);
      else
        rg_row_offset<= truncate(res);
      rg_mul_done<= True;
    endrule


    method Tuple3#(Bool,Bit#(nColbits),Bit#(accumindexbits)) outp_buffer_addr;
      Bool valid_req= False;
      if(rg_state==Pre_active || rg_state==Active_new_burst || rg_state==Active_burst_data)
        valid_req= True;

      return tuple3(valid_req, rg_bank_index, (rg_buffer_base + zeroExtend(rg_accum_row_index)));
    endmethod

    method Action buffer_val(Bit#(mulWidth2) data);
      wr_buffer_val<= data;
    endmethod

    method Action init(Bit#(accumindexbits) row_index, Bit#(convColbits) conv_output_row_size,
                       Bit#(convColbits) conv_output_col_size, Bool is_output_size_changed)
                       if(rg_state==Done);
      rg_accum_row_index<= row_index;
      rg_buffer_base<= row_index;
      rg_conv_output_col_size<= conv_output_col_size;
      if(is_output_size_changed) begin
        mul.inp(zeroExtend(conv_output_col_size), zeroExtend(conv_output_row_size));
        rg_state<= Compute_conv_output_size;
      end
      else
        rg_state<= Pre_active;
      rg_bank_index<= 'd0;
      rg_burst_counter<= 'd0;
      rg_output_partial_row<= 'd0;
      rg_channel_offset<= 0;
    endmethod

    method Bool is_store_done;
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
