/*
Assumptions: 1. Each bank stores output window of one channel.
             2. Every bank stores output window corresponding to different channels.
             3. A store request will require the module to store data from bank 0 to accumbanks-1.
                Note that, an offset (rg_buffer_base) is specified which indicates the starting
                accumindex.
             4. Parameter window_output_col_size < 256. If not, then multiple burst req will have to
                be generated for a single output row.

//TODO
1. Current assumption is that BRAM read req and AXI write req can never stall. Fix that.
2. Reduce number of multipliers used by reusing the same hardware.
3. Change multipliers from single cycle to multicycle, and hide the latencies.

*/

package col2im;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import Semi_FIFOF::*;
  import ConfigReg::*;
  `define Buffer_wreq_id 4

  interface Ifc_col2im#(numeric type addr_width, numeric type data_width, numeric type accumbanks,
                        numeric type mulWidth2, numeric type nRow, numeric type accumindexbits,
                        numeric type convColbits);
    method Tuple3#(Bool,Bit#(TLog#(accumbanks)),Bit#(accumindexbits)) outp_buffer_addr;
    method Action buffer_val(Bit#(mulWidth2) data);
    method Action init(Bit#(accumindexbits) row_index, Bit#(convColbits) conv_output_col_size);
    interface AXI4_Master_IFC#(addr_width, data_width, 0) axi_buffer_wreq;
  endinterface

  module mkcol2im#(Bit#(addr_width) dram_base, parameter Integer window_output_col_size)
  (Ifc_col2im#(addr_width, data_width, accumbanks, mulWidth2, nRow, accumindexbits, convColbits))
  provisos(Log#(nRow,nRowbits),
           Log#(TDiv#(data_width,8), awsz),
           Log#(accumbanks, accumbanksbits),
           Add#(a__, mulWidth2, data_width),
           Add#(b__, TAdd#(TAdd#(convColbits, convColbits), accumbanksbits), addr_width),
           Add#(c__, TAdd#(convColbits, accumindexbits), addr_width)
           );
    let v_awsize= valueOf(awsz);
    let v_accumbanks= valueOf(accumbanks);
    Bit#(accumindexbits) window_output_total_size= fromInteger(window_output_col_size *
                                                               window_output_col_size);

		AXI4_Master_Xactor_IFC #(addr_width, data_width, 0) memory_xactor <- mkAXI4_Master_Xactor;

    Reg#(Bit#(accumbanksbits)) rg_bank_index <- mkReg(0);  //accumbanksbits= Log(no. of Cols)
    Reg#(Bit#(accumindexbits)) rg_row_index <- mkReg(0);
    Reg#(Bool) rg_start <- mkConfigReg(False);
    Reg#(Bool) rg_done <- mkReg(True);
    Reg#(Bit#(8)) rg_burst_counter <- mkReg(0);
    Reg#(Bit#(accumindexbits)) rg_output_partial_row <- mkReg(0);
    Reg#(Bit#(accumindexbits)) rg_buffer_base <- mkReg(0);
    Reg#(Bit#(convColbits)) rg_conv_output_col_size <- mkReg(0);
    Reg#(Bit#(TAdd#(convColbits, convColbits))) rg_conv_output_total_size <- mkReg(0);

    Wire#(Bit#(mulWidth2)) wr_buffer_val <- mkWire();

    rule rl_one_cycle_delay;
      rg_start<= !rg_done;
    endrule

    rule rl_process_buffer_val(rg_burst_counter==0 && !rg_done && rg_start);
      Bit#(TAdd#(TAdd#(convColbits, convColbits), accumbanksbits)) channel_offset=
      zeroExtend(rg_conv_output_total_size) * zeroExtend(rg_bank_index);
      Bit#(TAdd#(convColbits, accumindexbits)) row_offset= zeroExtend(rg_conv_output_col_size) *
                                                           zeroExtend(rg_output_partial_row);
      Bit#(addr_width) store_addr= dram_base + zeroExtend(channel_offset) + zeroExtend(row_offset);

      if(rg_output_partial_row < fromInteger(window_output_col_size)) begin
        rg_output_partial_row<= rg_output_partial_row + 1;
      end
      else begin
        rg_output_partial_row<= 0;
      end

		  AXI4_Wr_Addr#(addr_width, 0) aw = AXI4_Wr_Addr {awaddr : store_addr, awuser : 0,
      awlen : fromInteger(window_output_col_size), awsize : fromInteger(v_awsize), awburst : 'b10,
      awid : `Buffer_wreq_id, awprot:? }; // arburst : 00 - FIXED 01 - INCR 10 - WRAP

  	  let w  = AXI4_Wr_Data {wdata : zeroExtend(wr_buffer_val), wstrb : '1,
                             wlast : rg_burst_counter==fromInteger(window_output_col_size-1), 
                             wid : `Buffer_wreq_id };
	    memory_xactor.i_wr_addr.enq(aw);
		  memory_xactor.i_wr_data.enq(w);
      rg_burst_counter<= rg_burst_counter+1;
      rg_row_index<= rg_row_index+1;
    endrule

    rule rl_send_burst_data(rg_burst_counter!=0 && !rg_done && rg_start);
  	  let w  = AXI4_Wr_Data {wdata : zeroExtend(wr_buffer_val), wstrb : '1,
                             wlast : rg_burst_counter==fromInteger(window_output_col_size-1), 
                             wid : `Buffer_wreq_id };
		  memory_xactor.i_wr_data.enq(w);

      if(rg_burst_counter==fromInteger(window_output_col_size-1))
        rg_burst_counter<= 0;
      else
        rg_burst_counter<= rg_burst_counter+1;

      if(rg_row_index < rg_buffer_base + window_output_total_size)
        rg_row_index<= rg_row_index + 1;
      else begin
        rg_row_index<= rg_buffer_base;
        rg_bank_index<= rg_bank_index+1;      //TODO Assuming a store instruction would end at the
                                              //last bank.
        if(rg_bank_index == fromInteger(v_accumbanks))
          rg_done<= True;
      end
    endrule

    method Tuple3#(Bool,Bit#(accumbanksbits),Bit#(accumindexbits)) outp_buffer_addr;
      return tuple3(!rg_done, rg_bank_index, rg_row_index);
    endmethod

    method Action buffer_val(Bit#(mulWidth2) data);
      wr_buffer_val<= data;
    endmethod

    method Action init(Bit#(accumindexbits) row_index, Bit#(convColbits) conv_output_col_size)
    if(rg_done);
      rg_row_index<= row_index;
      rg_buffer_base<= row_index;
      rg_conv_output_col_size<= conv_output_col_size;
      rg_conv_output_total_size<= zeroExtend(conv_output_col_size) *
                                  zeroExtend(conv_output_col_size);
      rg_bank_index<= 'd0;
      rg_done<= False;
      rg_burst_counter<= 'd0;
      rg_output_partial_row<= 'd0;
    endmethod

    interface axi_buffer_wreq= memory_xactor.axi_side;

  endmodule


  (*synthesize*)
  module mkinst_col2im(Ifc_col2im#(32, 64, 98, 56, 150, 6, 7));
    let ifc();
    mkcol2im#('h80000000, 5) _temp(ifc);
    return ifc;
  endmodule

endpackage
