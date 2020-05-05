/* 
Author: Mohan Prasath G R
Email id: mohanprasathr@gmail.com
Details:
This module performs the load and load_param operations. Recieves load and load_param instruction from the
dependency resolver module.
Load_param : Loads the parameters for given N number of instructions from DRAM and stores it in the paramter buffer.
Load: Generates required number of AXI requests to load a 3D matrix to either of input/weight/output buffer and 
for load immediate, stores a constant value 
--------------------------------------------------------------------------------------------------

  Interface Parameters:
  	1. addr_width (AXI)
  	2. data_width (AXI)


  	subinterface:
  	1. AXI4_Master_IFC
  	2. Put interface to get the instruction from dependency resolver

  Assumptions:
	1. Start address of parambuffer to read the parameters should be multiple of 8
	2. loadimmediate only for input buffer

  TODO:
  	1. write Code for loading into buffers once finalized -- Done
  	2. logic to load parameters into param_buffer -- Done
  	3. Finalize sram address space
  	4. Finalize instruction encoding for load param
  	5.

*/

package load_Module;

import GetPut::*;
import FIFOF::*;
import Semi_FIFOF:: *;
import AXI4_Types:: *;
import AXI4_Fabric:: *;
import Connectable::*;
import isa::*;
import FIFOF::*;
import onchip_buffers::*;
import BRAM::*;
import BRAMCore::*;
import Vector::*;
`include "systolic.defines"

`define INS_WIDTH 64
`define rd_req_id 5
`define DIM_WIDTH3 2*`DIM_WIDTH1


interface Ifc_load_Module#(numeric type addr_width, numeric type data_width);

	interface AXI4_Master_IFC#(addr_width, data_width,0) master;
	interface Put#(Bit#(`INS_WIDTH)) subifc_from_depResolver;

endinterface

module mk_load_Module(Ifc_load_Module#(addr_width, data_width))
		provisos(IsModule#(_m__, _c__),
				Add#(a__, 26, addr_width),
				Add#(b__, data_width, 64),
				Add#(b__, 32, addr_width));

	let axidata_width = valueOf(data_width);
 	let numWords_Ibuf = axidata_width/`INWIDTH;
 	let numWords_Obuf = axidata_width/`OUTWIDTH;

	AXI4_Master_Xactor_IFC #(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;

	Ifc_onchip_buffers buffers <- mkbuffers;

	FIFOF#(Bit#(addr_width)) ff_dest_addr <- mkSizedFIFOF(3) ;

	//parameters registers

	Reg#(Bit#(3)) rg_bitwidth <- mkReg(0); // in bytes
	Reg#(Dim1) rg_x_size <- mkReg(0); 
	Reg#(Dim1) rg_y_size <- mkReg(0);
	Reg#(Dim1) rg_z_size <- mkReg(0);
	Reg#(Bool) rg_isreset <- mkReg(False);
	Reg#(Dim1) rg_z_stride <- mkReg(0);
	Reg#(Dim1) rg_y_stride <- mkReg(0);
	Reg#(DRAM_address) rg_dramaddr <- mkReg(0);
	Reg#(SRAM_address) rg_sramaddr <- mkReg(0);	
	Reg#(SRAM_address) rg_zy_size <- mkReg(0); //zsize * ysize
	Reg#(SRAM_address) rg_xyz_size <- mkReg(0); 


	//local registers
	Reg#(DRAM_address) rg_dram_addr_a <- mkReg(0);
	Reg#(DRAM_address) rg_dram_addr_b <- mkReg(0);
	Reg#(SRAM_address) rg_sram_addr_a <- mkReg(0);
	Reg#(SRAM_address) rg_sram_addr_b <- mkReg(0);
	Reg#(Dim1) rg_y_cntr <- mkReg(0);
	Reg#(Dim1) rg_z_cntr <- mkReg(0);

	Reg#(Bit#(8)) rg_burst_len <- mkReg(0); // (total no.of bytes per request) / (axidatawidth) - 1
	Reg#(Bool) rg_burst <- mkReg(False);
	Reg#(Bit#(addr_width)) rg_burst_addr <- mkReg(0);
	Reg#(Bool) rg_finish_load <- mkReg(True);
	Reg#(SRAM_address) rg_paramaddr <- mkReg(0);


	//Get load parameters from param_Buffer
	rule rl_get_parambuffer; //yet to define the conditions for this rule 

		let {param_index, bufferbank} = split_address_PBUF(rg_paramaddr);	
		Bit#(256) val = 0;
		let temp = 0;

		for(Integer i=0; i < `PBUF_BANKS; i=i+1) begin
			 Bit#(TAdd#(`PBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
             Bit#(`PBUF_INDEX) index = param_index;
             Bit#(`PBUF_Bankbits) bank = truncate(m);
             if(bank < bufferbank)begin 
               index = index + 1;
             end
			let temp1 <- buffers.param_buffer[bank].portB.response.get();
			val = (val << `PBUF_WIDTH) | zeroExtend(temp1);
		end

		let lv_upperlimit = 255 - rg_paramaddr[2:0];
		let lv_lowerlimit = 0;
		// let lv_lowerlimit = lv_upperlimit - sizeof(load_parameters) + 1
		let lv_param = val[lv_upperlimit: lv_lowerlimit]; //parameters for load inst.

		// rg_bitwidth <= if (loading inpweight) ? `INBYTES : `OUTBYTES; //yet to be defined based on sram address
		rg_sram_addr_b <= temp[86:61]; //sram base address
		rg_dram_addr_b <= temp[118:87]; //dram base address
		// rg_burst_len <= if (loading inp/weight) ? truncate(((temp[36:25] *(`INBYTES)) >> 3)-1) : truncate(((temp[36:25] *(`OUTBYTES)) >> 3)-1); //temp[24:17] - z_size 
		rg_zy_size <= zeroExtend(temp[36:25] * temp[48:37]); //z_size * y_size
		rg_x_size <= temp[60:49];
		rg_y_size <= temp[48:37];
		rg_z_size <= temp[36:25];
		rg_z_cntr <= temp[36:25];
		rg_xyz_size <= zeroExtend(temp[36:25] * temp[48:37] * temp[60:49]);
		rg_isreset <= temp[0]==1'b1;
		rg_sramaddr <= temp[86:61];
		rg_dramaddr <= temp[118:87];
		rg_z_stride <= temp[24:13];
		rg_y_stride <= temp[12:1];

	endrule


	// LOAD immediate into SRAM
	rule rl_load_immediate(rg_isreset && rg_xyz_size !=0 ); 

		// load immediate into input buffer -- have to do similarly for weight and output if necessary
		let {inp_index, inp_bufferbank} = split_address_IBUF(rg_sram_addr_b);

		if(rg_xyz_size >= `IBUF_BANKS) begin
			for(Integer i=0; i<`IBUF_BANKS; i=i+1)begin
	             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(inp_bufferbank) + fromInteger(i);
	             Bit#(`IBUF_INDEX) index = inp_index;
	             Bit#(`IBUF_Bankbits) bank = truncate(m);
	             if(bank < inp_bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`INWIDTH) data = truncate(rg_dram_addr_b);
	             buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data)); 
	             $display($time,"writing data index %h bank %d rg_xyz_size %d\n",index,bank,rg_xyz_size);
	        end
	        rg_xyz_size <= rg_xyz_size - (`IBUF_BANKS);
	    end
	    else begin 
	    	for(Integer i=0; i<`IBUF_BANKS; i=i+1)begin
	             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(inp_bufferbank) + fromInteger(i);
	             Bit#(`IBUF_INDEX) index = inp_index;
	             Bit#(`IBUF_Bankbits) bank = truncate(m);
	             if(bank < inp_bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`IBUF_Bankbits,1)) n = zeroExtend(inp_bufferbank) + truncate(rg_xyz_size);
	             Bit#(`INWIDTH) data = truncate(rg_dram_addr_b);
	             if(m <= n)	begin
	             	buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	             	$display($time,"writing data index %h bank %d rg_xyz_size %d\n",index,bank,rg_xyz_size);
	             end
	        end
	        rg_xyz_size <= 0;
	        rg_finish_load <= True; //end of load immmediate
	    end

        rg_sram_addr_b <= rg_sram_addr_b + truncate(rg_xyz_size * `INBYTES);

	endrule

	//LOAD from DRAM to SRAM
	rule rl_start_dram_Read( !(rg_x_size ==0 && rg_y_cntr==0) && !rg_isreset);  

		if(rg_y_cntr == (rg_y_size-1)) begin //----- end of a 2D slice -----
			
			rg_dram_addr_a <= rg_dram_addr_a + zeroExtend(rg_y_stride * zeroExtend(rg_bitwidth));
			rg_dram_addr_b <= rg_dramaddr;
			rg_sram_addr_a <= rg_sram_addr_a + truncate(rg_zy_size * zeroExtend(rg_bitwidth));
			rg_sram_addr_b <= rg_sramaddr;
			rg_y_cntr <= 0;
			rg_x_size <= rg_x_size - 1;
		end
		else begin
			rg_dram_addr_b <= rg_dram_addr_b + zeroExtend(rg_z_stride * zeroExtend(rg_bitwidth));
			rg_sram_addr_b <= rg_sram_addr_b + zeroExtend(rg_z_size * zeroExtend(rg_bitwidth));
			rg_y_cntr <= rg_y_cntr + 1;
		end

		Bit#(addr_width) lv_dram_addr = zeroExtend(rg_dram_addr_a + rg_dram_addr_b);
		Bit#(addr_width) lv_sram_addr = zeroExtend(rg_sram_addr_a + rg_sram_addr_b);

		ff_dest_addr.enq(lv_sram_addr);

		let read_request = AXI4_Rd_Addr {araddr: lv_dram_addr, 
                         arid: {`rd_req_id}, arlen: rg_burst_len,
                         arsize: 'b011, arburst: 'b01, //arburst: 00-FIXED 01-INCR 10-WRAP
                         aruser: 0 };  

        m_xactor.i_rd_addr.enq(read_request);

	endrule

	rule rl_start_write(m_xactor.o_rd_data.first.rid == `rd_req_id && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                         && !rg_burst);

		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		let lv_sram_addr = ff_dest_addr.first;

		/* ----code for loading into buffer----*/

		if(False) begin //loadparam instruction

			let {param_index, bufferbank} = split_address_PBUF(lv_sram_addr);

			buffers.param_buffer[bufferbank].portB.request.put(makeRequest(True, 'b1 , param_index, zeroExtend(lv_data)));

		end

		else if(False) begin //loading inputs
			
			let {inp_index, bufferbank} = split_address_IBUF(lv_sram_addr);

			if(rg_z_cntr >= fromInteger(numWords_Ibuf)) begin
				for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`IBUF_INDEX) index = inp_index;
	             Bit#(`IBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end
	            rg_z_cntr <= rg_z_cntr - fromInteger(numWords_Ibuf);
	        end
	        else begin
	        	for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`IBUF_INDEX) index = inp_index;
	             Bit#(`IBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`IBUF_Bankbits,1)) n = zeroExtend(bufferbank) + truncate(rg_z_cntr);
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             if(m <= n) begin	
	             	buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	             end
	            end
	            rg_z_cntr <= rg_z_size;
	        end

		end

		else if(False) begin //loading weights

			let {wt_index, bufferbank} = split_address_WBUF(lv_sram_addr);

			if(rg_z_cntr >= fromInteger(numWords_Ibuf)) begin
				for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`WBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`WBUF_INDEX) index = wt_index;
	             Bit#(`WBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             buffers.weight_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end	
	            rg_z_cntr <= rg_z_cntr - fromInteger(numWords_Ibuf);
	        end
	        else begin
	        	for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`WBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`WBUF_INDEX) index = wt_index;
	             Bit#(`WBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`WBUF_Bankbits,1)) n = zeroExtend(bufferbank) + truncate(rg_z_cntr);
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             if(m >= n) 
	            	buffers.weight_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end
	            rg_z_cntr <= rg_z_size;
	        end
		end

		else if(False) begin //loading outputs
		
			let {out_index, bufferbank} = split_address_OBUF(lv_sram_addr);

			if(rg_z_cntr >= fromInteger(numWords_Obuf)) begin
				for(Integer i=0; i<numWords_Obuf; i=i+1)begin
	             Bit#(TAdd#(`OBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`OBUF_INDEX) index = out_index;
	             Bit#(`OBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`OUTWIDTH) data = lv_data[(i+1)*`OUTWIDTH-1:i*`OUTWIDTH];
	             buffers.output_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end
	            rg_z_cntr <= rg_z_cntr - fromInteger(numWords_Obuf);
	        end
	        else begin
	        	for(Integer i=0; i<numWords_Obuf; i=i+1)begin
	             Bit#(TAdd#(`OBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`OBUF_INDEX) index = out_index;
	             Bit#(`OBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`OBUF_Bankbits,1)) n = zeroExtend(bufferbank) + truncate(rg_z_cntr);
	             Bit#(`OUTWIDTH) data = lv_data[(i+1)*`OUTWIDTH-1:i*`OUTWIDTH];
	             if (m >= n)
	            	 buffers.output_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	             end
	            rg_z_cntr <= rg_z_size;
	        end
		end

		
		if(!lv_resp.rlast) begin
			rg_burst <= True;
			let lv_burst_addr = lv_sram_addr + (fromInteger(axidata_width) >> 3); 
			rg_burst_addr <= lv_burst_addr;
		end

		ff_dest_addr.deq;  //dequeing this FIFO will cause start_Mem_Read to fire.

	endrule

	rule rl_burst_load(m_xactor.o_rd_data.first.rid == `rd_req_id && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                        && rg_burst);

		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		let lv_sram_addr = rg_burst_addr;


		/* ----code for loading into buffer----*/

		if(False) begin //loadparam instruction

			let {param_index, bufferbank} = split_address_PBUF(lv_sram_addr);

			buffers.param_buffer[bufferbank].portB.request.put(makeRequest(True, 'b1 , param_index, zeroExtend(lv_data)));

		end

		else if(False) begin //loading inputs
			
			let {inp_index, bufferbank} = split_address_IBUF(lv_sram_addr);

			if(rg_z_cntr >= fromInteger(numWords_Ibuf)) begin
				for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`IBUF_INDEX) index = inp_index;
	             Bit#(`IBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end
	            rg_z_cntr <= rg_z_cntr - fromInteger(numWords_Ibuf);
	        end
	        else begin
	        	for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`IBUF_INDEX) index = inp_index;
	             Bit#(`IBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`IBUF_Bankbits,1)) n = zeroExtend(bufferbank) + truncate(rg_z_cntr);
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             if(m <= n) begin	
	             	buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	             end
	            end
	            rg_z_cntr <= rg_z_size;
	        end
		end

		else if(False) begin //loading weights

			let {wt_index, bufferbank} = split_address_WBUF(lv_sram_addr);

			if(rg_z_cntr >= fromInteger(numWords_Ibuf)) begin
				for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`WBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`WBUF_INDEX) index = wt_index;
	             Bit#(`WBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             buffers.weight_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end	
	            rg_z_cntr <= rg_z_cntr - fromInteger(numWords_Ibuf);
	        end
	        else begin
	        	for(Integer i=0; i<numWords_Ibuf; i=i+1)begin
	             Bit#(TAdd#(`WBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`WBUF_INDEX) index = wt_index;
	             Bit#(`WBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`WBUF_Bankbits,1)) n = zeroExtend(bufferbank) + truncate(rg_z_cntr);
	             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
	             if(m >= n) 
	            	buffers.weight_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end
	            rg_z_cntr <= rg_z_size;
	        end
		end

		else if(False) begin //loading outputs
		
			let {out_index, bufferbank} = split_address_OBUF(lv_sram_addr);

			if(rg_z_cntr >= fromInteger(numWords_Obuf)) begin
				for(Integer i=0; i<numWords_Obuf; i=i+1)begin
	             Bit#(TAdd#(`OBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`OBUF_INDEX) index = out_index;
	             Bit#(`OBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(`OUTWIDTH) data = lv_data[(i+1)*`OUTWIDTH-1:i*`OUTWIDTH];
	             buffers.output_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	            end
	            rg_z_cntr <= rg_z_cntr - fromInteger(numWords_Obuf);
	        end
	        else begin
	        	for(Integer i=0; i<numWords_Obuf; i=i+1)begin
	             Bit#(TAdd#(`OBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
	             Bit#(`OBUF_INDEX) index = out_index;
	             Bit#(`OBUF_Bankbits) bank = truncate(m);
	             if(bank < bufferbank)begin 
	               index = index + 1;
	             end
	             Bit#(TAdd#(`OBUF_Bankbits,1)) n = zeroExtend(bufferbank) + truncate(rg_z_cntr);
	             Bit#(`OUTWIDTH) data = lv_data[(i+1)*`OUTWIDTH-1:i*`OUTWIDTH];
	             if (m >= n)
	            	 buffers.output_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
	             end
	            rg_z_cntr <= rg_z_size;
	        end
		end


		let lv_burst_addr = lv_sram_addr + (fromInteger(axidata_width)>>3); 
		rg_burst_addr <= lv_burst_addr;

		if(lv_resp.rlast) begin // last beat of current burst
			rg_burst <= False;
			if(rg_x_size==0 && rg_y_cntr==0) begin // end of current load instruction
				rg_finish_load <= True;
			end
		end


	endrule

	interface master = m_xactor.axi_side;

	interface Put subifc_from_depResolver;
		
		method Action put(Bit#(`INS_WIDTH) instruction) if(rg_finish_load);			
			rg_finish_load <= False;
			if(instruction.opcode == LOAD_PARAM) begin //needs to be updated once instruction encoding is finalized
				rg_dramaddr <= dram_base + instruction.dram_offset;
				rg_sramaddr <= instruction.sram_offset;
				rg_x_size <= 'b1;
				rg_y_size <= 'b1;
				rg_z_size <= instruction.flags << 4; // flags contain no.of instructions, 16bytes per instruction
				rg_z_cntr <= instruction.flags << 4;
				rg_z_stride <= 0;
				rg_y_stride <= 0;
				rg_isreset <= False;
				rg_bitwidth <= 0;
				rg_sram_addr_b <= instruction.sram_offset;
				rg_dram_addr_b <= dram_base + instruction.dram_offset;
				rg_burst_len <= instruction.flags << 4; //zsize 
			end
			else if(instruction.opcode == LOAD) begin

				let {param_index, bufferbank} = split_address_PBUF(instruction.sram_offset);
				rg_paramaddr <= instruction.sram_offset;

				for(Integer i=0; i < `PBUF_BANKS; i=i+1) begin
					 Bit#(TAdd#(`PBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
		             Bit#(`PBUF_INDEX) index = param_index;
		             Bit#(`PBUF_Bankbits) bank = truncate(m);
		             if(bank < bufferbank)begin 
		               index = index + 1;
		             end
					buffers.param_buffer[bank].portB.request.put(makeRequest(False, 0, index, ?));
				end
			end
		endmethod
	endinterface

endmodule

(*synthesize*)
  module mkload_Tb(Ifc_load_Module#(32,64));
    let ifc();
    mk_load_Module inst1(ifc);
    return (ifc);
  endmodule

endpackage



	