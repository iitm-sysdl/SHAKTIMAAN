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
  	3. input_width (precision of input in bytes)
  	4. index bits and bank bits for all buffers once decided

  	subinterface:
  	1. AXI4_Master_IFC
  	2. Put interface to get the instruction from dependency resolver

  Assumptions:
	1. precision of the input/output will be in bytes -- input_width paramter
	2. defined numbanks and buffer indexbits variable here -- will be defined later in buffer module
	3. Entries of param_buffer - Bit#(128) - large enough to accomodate parameters of largest instruction

  TODO:
  	1. write Code for loading into buffers once finalized
  	2. logic to load parameters into param_buffer

*/

package load_Module;

import GetPut::*;
import FIFOF::*;

`define rd_req_id 5
`define numbanks 4
`define DIM_WIDTH3 2*`DIM_WIDTH1
`define inpbuf_bankbits 
`define inpbuf_indexbits


interface Ifc_load_Module#(numeric type addr_width, numeric type data_width, numeric type input_width);

	interface AXI4_Master_IFC#(addr_width, data_width,0) master;
	interface Put#(Bit#(INS_WIDTH)) subifc_from_depResolver;

endinterface


module mk_load_Module(Ifc_load_module#(addr_width, data_width, input_width))
 provisos(
 		  Div#(data_width, input_width, nwords));

 	let numWords = valueOf(nwords);

	AXI4_Master_Xactor_IFC #(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;

	FIFOF#(Bit#(addr_width)) ff_dest_addr <- mkSizedFIFOF(3) ;

	BRAM_Configure param_bufcfg = defaultValue;

	param_bufcfg.memorySize = numEntries_paramBuf;
    param_bufcfg.loadFormat = None;

    BRAM2PortBE#(SRAM_address, Bit#(128), 2) param_Buffer <- mkBRAM2ServerBE(param_bufcfg); //128 bits to accomodate a instruction's parameters 

    function BRAMRequestBE#(Bit#(a), Bit#(d), n) makeRequest (Bool write,Bit#(n) wstrb,Bit#(a) addr, Bit#(d)
        data);
            return BRAMRequestBE{
                                writeen: wstrb ,
                                responseOnWrite: False,
                                address   : addr,
                                datain : data
                              };
    endfunction
	
	//parameters registers

	Reg#(Bit#(3)) rg_bitwidth <- mkReg(0); // in bytes
	Reg#(Bit#(DIM_WIDTH3)) rg_zy_size <- mkReg(0); //zsize * ysize
	Reg#(Bit#(DIM_WIDTH3)) rg_xyz_size <- mkReg(0); 
	Reg#(Load_params) rg_load_params <- mkReg(0);


	//local registers
	Reg#(Bit#(param_width)) rg_dram_addr_a <- mkReg(0);
	Reg#(Bit#(param_width)) rg_dram_addr_b <- mkReg(0);
	Reg#(Bit#(param_width)) rg_sram_addr_a <- mkReg(0);
	Reg#(Bit#(param_width)) rg_sram_addr_b <- mkReg(0);
	Reg#(Bit#(param_width)) rg_y_cntr <- mkReg(0);

	Reg#(Bit#(8)) rg_burst_len <- mkReg(0); // (total no.of bytes per request) / (axidatawidth) - 1
	Reg#(Bit#(8)) rg_burst_cntr <- mkReg(0);
	Reg#(Bool) rg_finish_load <- mkReg(True);


	//Get load parameters from param_Buffer
	rule rl_get_parambuffer(); //yet to define the conditions for this rule 

		let temp <- param_Buffer.portB.response.get();
		rg_load_params <= truncate(temp);

		rg_bitwidth <= (if loading inp/weight) ? input_width : (input_width << 1); //yet to be defined based on sram address
		rg_sram_addr_b <= temp[66:41]; //sram base address
		rg_dram_addr_b <= temp[98:67]; //dram base address
		rg_burst_len <= (if loading inp/weight) ? truncate(((temp[24:17] *(input_width)) >> 3)-1) : truncate(((temp[24:17] *(input_width << 1)) >> 3)-1); //temp[24:17] - z_size 
		rg_zy_size <= temp[24:17] * temp[32:25]; //z_size * y_size
		rg_x_size <= temp[40:33];
		rg_xyz_size <= temp[24:17] * temp[32:25] * temp[40:33];

	endrule


	// LOAD immediate into SRAM
	rule rl_load_immediate(rg_load_params.is_reset && rg_xyz_size !=0 ); 

		let {inp_index, inp_bufferbank} = split_address_inpbuf(rg_sram_addr_b); //the function will be defined in the buffer module

		let looplimit = min(numbanks, rg_xyz_size);

		// load immeditate into input buffer -- have to do similarly for weight and output if necessary
		for(Integer i=0; i<looplimit; i=i+1)begin
             Bit#(TAdd#(inpbuf_bankbits,1)) m = zeroExtend(inp_bufferbank) + fromInteger(i);
             Bit#(inpbuf_indexbits) index = inp_index;
             Bit#(inpbuf_bankbits) bank = truncate(m);
             if(bank < inp_bufferbank)begin 
               index = index + 1;
             end
             Bit#(input_width) data = truncate(rg_dram_addr_b);
             input_buffer[bank].portB.request.put(makeRequest(True, 'b1 , index, data));  //buffer yet to be instantiated
        end

        rg_xyz_size <= rg_xyz_size - zeroExtend(looplimit);
        rg_sram_addr_b <= rg_sram_addr_b + looplimit * input_width;

        if(looplimit == rg_xyz_size) rg_finish_load <= True; //end of load immmediate

	endrule

	//LOAD from DRAM to SRAM
	rule rl_start_dram_Read( !(rg_x_size ==0 && rg_y_cntr==0) && !rg_load_params.is_reset);  

		if(rg_y_cntr == (rg_y_size-1)) begin //----- end of a 2D slice -----
			
			rg_dram_addr_a <= rg_dram_addr_a + rg_load_params.y_stride * (rg_bitwidth);
			rg_dram_addr_b <= rg_load_params.dram_address;
			rg_sram_addr_a <= rg_sram_addr_a + rg_zy_size * (rg_bitwidth);
			rg_sram_addr_b <= rg_load_params.sram_address;
			rg_y_cntr <= 0;
			rg_x_size <= rg_x_size - 1;
		end
		else begin
			rg_dram_addr_b <= rg_dram_addr_b + rg_load_params.z_stride * (rg_bitwidth);
			rg_sram_addr_b <= rg_sram_addr_b + rg_load_params.z_stride * (rg_bitwidth);
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

		/* ----code for loading into buffer----
		  -------- case statement -------*/
		if(loadparam instruction) begin

		end

		else if(loading inputs) begin
			for(Integer i=0; i<numWords; i=i+1)begin
             Bit#(TAdd#(inpbuf_bankbits,1)) m = zeroExtend(inp_bufferbank) + fromInteger(i);
             Bit#(inpbuf_indexbits) index = inp_index;
             Bit#(inpbuf_bankbits) bank = truncate(m);
             if(bank < inp_bufferbank)begin 
               index = index + 1;
             end
             Bit#(input_width) data = lv_data[(i+1)*input_width-1:i*input_width];
             input_buffer[bank].portB.request.put(makeRequest(True, 'b1 , index, data));
           end
		end

		else if(loading weights) begin
		//--------
		end

		else if(loading outputs) begin
		//--------
		end

		
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


		/* ----code for loading into buffer----
		  -------- case statement -------*/
		if(loadparam instruction) begin

		end

		else if(loading inputs) begin
			for(Integer i=0; i<numWords; i=i+1)begin
             Bit#(TAdd#(inpbuf_bankbits,1)) m = zeroExtend(inp_bufferbank) + fromInteger(i);
             Bit#(inpbuf_indexbits) index = inp_index;
             Bit#(inpbuf_bankbits) bank = truncate(m);
             if(bank < inp_bufferbank)begin 
               index = index + 1;
             end
             Bit#(input_width) data = lv_data[(i+1)*input_width-1:i*input_width];
             input_buffer[bank].portB.request.put(makeRequest(True, 'b1 , index, data));	//buffer yet to be instantiated
           end
		end

		else if(loading weights) begin
		//--------
		end

		else if(loading outputs) begin
		//--------
		end


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
		method Action put(Bit#(INS_WIDTH) instruction) if(rg_finish_load);
			
			rg_finish_load <= False;

			if(instruction.opcode == LOAD_PARAM) begin

				rg_load_params.dram_address <= dram_base + instruction.dram_offset;
				rg_load_params.sram_address <= instruction.sram_offset
				rg_load_params.x_size <= 'b1;
				rg_load_params.y_size <= 'b1;
				rg_load_params.z_size <= instruction.flags << 4; // flags contain no.of instructions, 16bytes per instruction
				rg_load_params.z_stride <= 0;
				rg_load_params.y_stride <= 0;
				rg_load_params.is_reset <= False;
				rg_bitwidth <= 0;
				rg_sram_addr_b <= instruction.sram_offset;
				rg_dram_addr_b <= dram_base + instruction.dram_offset;
				rg_burst_len <= instruction.flags << 4; //zsize 

			end

			else if(instruction.opcode == LOAD) begin

				param_Buffer.portB.request.put(makeRequest(False, 0, instruction.sram_offset, ?));

			end

		endmethod
	endinterface

endmodule


	