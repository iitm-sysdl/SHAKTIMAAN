/* 
Author: Mohan Prasath G R
Email id: mohanprasathr@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/
/*Assumptions:
	1. precision of the input/output will be a multiple of 8 (in bits)

  TODO:
  	1. write Code for loading into buffers once finalized
  	2. 

*/

package load_Module;

import GetPut::*;
import FIFOF::*;

`define rd_req_id 5



interface Ifc_load_Module#(numeric type addr_width, numeric type data_width, numeric type param_width);

	interface AXI4_Master_IFC#(addr_width, data_width,0) master;
	interface Put#(Bit#(ILEN)) subifc_from_depResolver;

endinterface


module mk_load_Module(Ifc_load_module#(addr_width, data_width, param_width));

	AXI4_Master_Xactor_IFC #(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;

	FIFOF#(Bit#(addr_width)) ff_dest_addr <- mkSizedFIFOF(3) ;
	FIFOF#(Tuple2#(Bit#(addr_width), Bit#(data_width))) ff_temp_sram <- mkSizedFIFOF(1024); //temporary fifo instead of buffer for now
	
	//parameters registers
	Reg#(Bit#(addr_width)) rg_dram_base <- mkReg(0);
	Reg#(Bit#(addr_width)) rg_sram_base <- mkReg(0);
	Reg#(Bit#(param_width)) rg_z_size <- mkReg(0);
	Reg#(Bit#(param_width)) rg_y_size <- mkReg(0);
	Reg#(Bit#(param_width)) rg_x_size <- mkReg(0);
	Reg#(Bit#(param_width)) rg_z_stride <- mkReg(0);
	Reg#(Bit#(param_width)) rg_y_stride <- mkReg(0);
	Reg#(Bit#(param_width)) rg_bitwidth <- mkReg(0);
	Reg#(Bit#(param_width)) rg_zy_size <- mkReg(0); //zsize * ysize


	Reg#(Bit#(param_width)) rg_dram_addr_a <- mkReg(0);
	Reg#(Bit#(param_width)) rg_dram_addr_b <- mkReg(0);
	Reg#(Bit#(param_width)) rg_sram_addr_a <- mkReg(0);
	Reg#(Bit#(param_width)) rg_sram_addr_b <- mkReg(0);
	Reg#(Bit#(param_width)) rg_y_cntr <- mkReg(0);

	Reg#(Bit#(8)) rg_burst_len <- mkReg(0); //truncate(((rg_z_size * (rg_bitwidth >> 3)) >> 3)-1); // dividing by 8 again since burstsize = 8bytes
	Reg#(Bit#(8)) rg_burst_cntr <- mkReg(0);
	Reg#(Bool) rg_finish_load <- mkReg(True);


	rule rl_start_dram_Read(rg_x_size==0 && rg_y_cntr==0 );

		if(rg_y_cntr == (rg_y_size-1)) begin //----- end of a 2D slice -----
			
			rg_dram_addr_a <= rg_dram_addr_a + rg_y_stride * (rg_bitwidth >> 3);
			rg_dram_addr_b <= rg_dram_base;
			rg_sram_addr_a <= rg_sram_addr_a + rg_zy_size * (rg_bitwidth >> 3);
			rg_sram_addr_b <= rg_sram_base;
			rg_y_cntr <= 0;
			rg_x_size <= rg_x_size - 1;
		end
		else begin
			rg_dram_addr_b <= rg_dram_addr_b + rg_z_stride * (rg_bitwidth >> 3);
			rg_sram_addr_b <= rg_sram_addr_b + rg_z_stride * (rg_bitwidth >> 3);
			rg_y_cntr <= rg_y_cntr + 1;
		end

		Bit#(addr_width) lv_dram_addr = rg_dram_addr_a + rg_dram_addr_b;
		Bit#(addr_width) lv_sram_addr = rg_sram_addr_a + rg_sram_addr_b;

		ff_dest_addr.enq(lv_sram_addr);

		let read_request = AXI4_Rd_Addr {araddr: lv_dram_addr, 
                         arid: {`rd_req_id}, arlen: rg_burst_len,
                         arsize: (data_width >> 3), arburst: 'b01, //arburst: 00-FIXED 01-INCR 10-WRAP
                         aruser: 0 };  

        m_xactor.i_rd_addr.enq(read_request);

	endrule

	rule rl_start_write(m_xactor.o_rd_data.first.rid == `rd_req_id && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                         && rg_burst_cntr == 0);

		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		let lv_sram_addr = ff_dest_addr.first;

		ff_temp_sram.enq(tuple2(lv_sram_addr, lv_data)); //loading in a FIFO instead of buffer for now
		
		/* ----code for loading into buffer----*/

		if(rg_burst_len > 0) begin
			rg_burst_cntr <= rg_burst_cntr + 1;
			let lv_burst_addr = lv_addr + (data_width>>3); // or (data_width)/(bitwidth)?
			rg_burst_addr <= lv_burst_addr;
		end

		ff_dest_addr.deq;  //dequeing this FIFO will cause start_Mem_Read to fire.


	endrule

	rule rl_burst_load(m_xactor.o_rd_data.first.rid == `rd_req_id && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                        && rg_burst_cntr != 0);


		Bool lv_last = (rg_burst_cntr == rg_burst_len);
		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		let lv_sram_addr = rg_burst_addr;

		ff_temp_sram.enq(tuple2(lv_sram_addr, lv_data)); //loading in a FIFO instead of buffer for now
		
		/*  ----code for loading in buffer------  */

		let lv_burst_addr = lv_addr + (data_width>>3); // or (data_width)/(bitwidth)?
		rg_burst_addr <= lv_burst_addr;

		if(lv_last) begin // last beat of current burst
			rg_burst_cntr <= 0;
			if(rg_x_size==0 && rg_y_cntr==0) begin // end of current load instruction
				rg_finish_load <= True;
			end
		end
		else rg_burst_cntr <= rg_burst_cntr + 1;

	endrule

	interface master = m_xactor.axi_side;

	interface Put subifc_from_depResolver;
		method Action put(Bit#(ILEN) loadinst) if(rg_finish_load);
			rg_finish_load <= False;
			/* copy the parameters to 
			rg_z_size
			rg_x_size
			rg_y_size
			rg_y_stride
			rg_z_stride
			rg_dram_base
			rg_sram_base
			rg_bitwidth
			based on the instruction encoding &
			intialize
			rg_sram_addr_b
			rg_dram_addr_b
			rg_zy_size
			rg_burst_len
			*/
		endmethod
	endinterface

endmodule


	