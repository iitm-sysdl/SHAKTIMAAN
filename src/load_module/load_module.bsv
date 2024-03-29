/* 
Author: Mohan Prasath G R, Gokulan Ravi
Email id: mohanprasathr@gmail.com, gokulan97@gmail.com
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
  	2. Put interface to get the parameters from dependency resolver
  	3. Get interface to send finish signal to dep. resolver

  	methods
  	 to send the data and index of the buffers to top module

  Assumptions:
	1. The precision of any operand will always be an exponent of 2

  TODO:
  	1. write Code for loading into buffers once finalized -- Done
  	2. logic to load parameters into param_buffer -- Done
  	3. Finalize sram address space
  	4. Finalize parameters size for load
  	5.

*/

package load_module;

import GetPut::*;
import FIFOF::*;
import Semi_FIFOF:: *;
import AXI4_Types:: *;
import AXI4_Fabric:: *;
import Connectable::*;
import isa::*;
import FIFOF::*;
import Vector::*;
`include "systolic.defines"

interface Ifc_load_Module#(numeric type dram_addr_width, numeric type data_width,
						   numeric type wt_index, numeric type wt_bank, numeric type wt_data,
						   numeric type if_index, numeric type if_bank, numeric type if_data,
						   numeric type of_index, numeric type of_bank, numeric type of_data,
						   numeric type max_index, numeric type max_bank);

	interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) master;
	interface Put#(Load_params) subifc_put_loadparams;  //to get parameters from the dependency module
	interface Get#(Bool) subifc_send_loadfinish;	//send the finish signal once the load is completed
	method ActionValue#(SRAMReq#(max_index, max_bank, data_width)) write_data;
	method Bool send_interrupt;
endinterface

//(*synthesize*)
//module mkload_Tb(Ifc_load_Module#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH,`SRAM_ADDR_WIDTH,TLog#(`WBUF_ENTRIES),TLog#(`WBUF_BANKS),`INWIDTH,TLog#(`IBUF_ENTRIES),TLog#(`IBUF_BANKS),`INWIDTH,TLog#(`OBUF_ENTRIES),TLog#(`OBUF_BANKS),`OUTWIDTH,TLog#(`OBUF_ENTRIES),TLog#(`OBUF_BANKS)));
//    let ifc();
//    mk_load_Module inst1(ifc);
//    return (ifc);
//endmodule

module mk_load_Module(Ifc_load_Module#(addr_width, data_width,
									   wt_index, wt_bank, wt_data,
									   if_index, if_bank, if_data,
									   of_index, of_bank, of_data,
									   max_index, max_bank))
		provisos(
			IsModule#(_m__, _c__),
			Add#(addr_width,0,`DRAM_ADDR_WIDTH),
			Add#(wt_data, 0, if_data),
			Mul#(iwords, wt_data, data_width),
			Mul#(owords, of_data, data_width),
			Mul#(data_bytes, 8, data_width),
			Mul#(if_bytes, 8, if_data),
			Mul#(wt_bytes, 8, wt_data),
			Mul#(of_bytes, 8, of_data),
      		Log#(if_bytes, if_shift),
      		Log#(data_bytes, d_shift),
      		Log#(wt_bytes, wt_shift),
      		Log#(of_bytes, of_shift),
      		//Compiler generated
      		Add#(c__, of_index, max_index),
      		Add#(d__, of_bank, max_bank),
      		Add#(e__, if_index, max_index),
      		Add#(f__, if_bank, max_bank),
      		Add#(g__, wt_index, max_index),
      		Add#(h__, wt_bank, max_bank)
		);

  let burst_len_shift = valueOf(d_shift); //used to calculate burst length
	let val_data_bytes  = valueOf(data_bytes);
  let axi_width = valueOf(data_width);

	let iWords = valueOf(iwords);
	let oWords = valueOf(owords);

	let ibuf_index = valueOf(if_index);
	let ibuf_bankbits = valueOf(if_bank);
  let ibuf_shift = valueOf(if_shift);

	let wbuf_index = valueOf(wt_index);
	let wbuf_bankbits = valueOf(wt_bank);

	let obuf_index = valueOf(of_index);
	let obuf_bankbits = valueOf(of_bank);
  let obuf_shift = valueOf(of_shift);

  function Tuple2#(Bit#(if_index),Bit#(if_bank)) split_address_IBUF(SRAM_address addr);
    Bit#(if_bank) gbank = addr[ibuf_bankbits-1:0];
    Bit#(if_index) gindex = addr[ibuf_index+ibuf_bankbits-1:ibuf_bankbits];
    return tuple2(gindex,gbank);
  endfunction

  function Tuple2#(Bit#(wt_index),Bit#(wt_bank)) split_address_WBUF(SRAM_address addr);
    Bit#(wt_bank) gbank = addr[wbuf_bankbits-1:0];
    Bit#(wt_index) gindex = addr[wbuf_index+wbuf_bankbits-1:wbuf_bankbits];
    return tuple2(gindex,gbank);
  endfunction

  function Tuple2#(Bit#(of_index),Bit#(of_bank)) split_address_OBUF(SRAM_address addr);
    Bit#(of_bank) gbank = addr[obuf_bankbits-1:0];
    Bit#(of_index) gindex = addr[obuf_index+obuf_bankbits-1:obuf_bankbits];
    return tuple2(gindex,gbank);
  endfunction

	AXI4_Master_Xactor_IFC#(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;

	FIFOF#(SRAM_address) ff_dest_addr <- mkSizedFIFOF(3) ;
	Reg#(Maybe#(Load_params)) rg_params <- mkReg(tagged Invalid);

	Reg#(SRAM_address) rg_zy_size <- mkReg(0); //zsize * ysize
	Reg#(SRAM_address) rg_xyz_size <- mkReg(0); 

	Reg#(DRAM_address) rg_dram_addr <- mkReg(0);
	Reg#(SRAM_address) rg_sram_addr <- mkReg(0);

	Reg#(Dim1) rg_x_cntr <- mkReg(0);
	Reg#(Dim1) rg_y_cntr <- mkReg(0);
	Reg#(Dim1) rg_z_cntr <- mkReg(0);

	Reg#(Bit#(8)) rg_burst_len <- mkReg(0); // (total no.of bytes per request) / (axidatawidth) - 1
	Reg#(Bool) rg_load_requests <- mkReg(False);

	Reg#(Bool) rg_burst <- mkReg(False);
	Reg#(Bool) rg_finish_load <- mkReg(True);

	Wire#(SRAMReq#(max_index, max_bank, data_width)) wr_buffer_req <- mkWire();
	Reg#(Bool) rg_interrupt <- mkReg(False);



	function Bool is_address_within_range(SRAM_address start_addr, SRAM_address end_addr, SRAM_address query);
		return (start_addr <= query && query <= end_addr);
	endfunction

	Reg#(Maybe#(SRAM_address)) rg_burst_addr <- mkReg(tagged Invalid);
	//LOAD from DRAM to SRAM
	rule rl_start_dram_Read(rg_params matches tagged Valid .params &&&
							//!params.is_reset &&&
							rg_load_requests);

		let lv_read_request = AXI4_Rd_Addr {araddr: rg_dram_addr, arid: `Load_master, arlen: rg_burst_len,
							arsize: 'b100, arburst: 'b01, aruser: 0, arprot: ?};

		//TODO: replace the multipliers in the below logic with shift operations
		if(rg_x_cntr == 0 && rg_y_cntr == 0) begin
			rg_load_requests <= False;
		end
		else if(rg_y_cntr == 0) begin
			rg_y_cntr <= params.y_size - 1;
			rg_x_cntr <= rg_x_cntr - 1;
			rg_dram_addr <= rg_dram_addr + (zeroExtend(pack(params.y_stride)) << (params.bitwidth ? ibuf_shift : obuf_shift));
		end
		else begin
			rg_y_cntr <= rg_y_cntr - 1;
			rg_dram_addr <= rg_dram_addr + (zeroExtend(pack(params.z_stride)) << (params.bitwidth ? ibuf_shift : obuf_shift));
		end

		ff_dest_addr.enq(rg_sram_addr);
		if(is_address_within_range(`IBUF_START, `IBUF_END, rg_sram_addr))
			rg_sram_addr <= rg_sram_addr + (1 << ibuf_bankbits);
		else if(is_address_within_range(`WBUF_START, `WBUF_END, rg_sram_addr))
			rg_sram_addr <= rg_sram_addr + (1 << wbuf_bankbits);
		else if(is_address_within_range(`OBUF_START, `OBUF_END, rg_sram_addr))
			rg_sram_addr <= rg_sram_addr + (1 << obuf_bankbits);

    m_xactor.i_rd_addr.enq(lv_read_request);
		//$display($time, "Request for address %x: [%x, %x]", rg_dram_addr, rg_x_cntr, rg_y_cntr);
	endrule
	
	rule rl_start_write(rg_params matches tagged Valid .params &&&
						m_xactor.o_rd_data.first.rid == `Load_master &&&
						m_xactor.o_rd_data.first.rresp==AXI4_OKAY);

		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		SRAM_address lv_sram_addr;
		if(isValid(rg_burst_addr))begin
			lv_sram_addr = validValue(rg_burst_addr);
		end
		else begin
			lv_sram_addr = ff_dest_addr.first;
		end

		if(!lv_resp.rlast) begin
      rg_z_cntr <= rg_z_cntr - fromInteger(params.bitwidth ? iWords : oWords);
			rg_burst_addr <= tagged Valid (lv_sram_addr + 
				((fromInteger(axi_width) >> 3) >> (params.bitwidth ? ibuf_shift : obuf_shift)));
		end
    else begin
			ff_dest_addr.deq;
			rg_burst_addr <= tagged Invalid;
      rg_z_cntr <= params.z_size;
		end

		/*----code for writing into buffer----*/

		//$display($time, "Received response for %x, values: %x", lv_sram_addr, lv_data);
		if(is_address_within_range(`IBUF_START, `IBUF_END, lv_sram_addr))begin
			Bit#(if_index) inp_index;
      Bit#(if_bank) inp_bufferbank;
      {inp_index, inp_bufferbank} = split_address_IBUF(lv_sram_addr);
			Sram_valid num_valid = truncate(min(rg_z_cntr, fromInteger(iWords)));
			wr_buffer_req <= SRAMReq{buffer: InputBuffer, index: zeroExtend(inp_index), bank: zeroExtend(inp_bufferbank), data: lv_data, num_valid: num_valid};
		end
		else if(is_address_within_range(`WBUF_START, `WBUF_END, lv_sram_addr))begin //loading weights
			Bit#(wt_index) wgt_index;
      Bit#(wt_bank) wgt_bufferbank;
      {wgt_index, wgt_bufferbank} = split_address_WBUF(lv_sram_addr);
			Sram_valid num_valid = truncate(min(rg_z_cntr, fromInteger(iWords)));
			wr_buffer_req <= SRAMReq{buffer: WeightBuffer, index: zeroExtend(wgt_index), bank: zeroExtend(wgt_bufferbank), data: lv_data, num_valid: num_valid};
		end
		else if(is_address_within_range(`OBUF_START, `OBUF_END, lv_sram_addr))begin //loading outputs
			Bit#(of_index) out_index;
      Bit#(of_bank) out_bufferbank;
      {out_index, out_bufferbank} = split_address_OBUF(lv_sram_addr);
			Sram_valid num_valid = truncate(min(rg_z_cntr, fromInteger(oWords)));
			wr_buffer_req <= SRAMReq{buffer: (is_address_within_range(`OBUF1_START, `OBUF1_END, lv_sram_addr) ? OutputBuffer1 : OutputBuffer2), 
																	index: zeroExtend(out_index), bank: zeroExtend(out_bufferbank), data: lv_data, num_valid: num_valid};
		end
	endrule

	rule rl_raise_interrupt(m_xactor.o_rd_data.first.rid == `Load_master && 
		m_xactor.o_rd_data.first.rresp==AXI4_SLVERR);
		let lv_resp <- pop_o(m_xactor.o_rd_data);
		rg_interrupt <= True;
	endrule

	rule rl_send_finish(rg_params matches tagged Valid .params &&& !ff_dest_addr.notEmpty() &&& !rg_load_requests &&& !rg_finish_load);
		rg_finish_load <= True;
		//$display($time, "Load completed %d %d", isValid(rg_params), rg_finish_load);
	endrule

  interface master = m_xactor.axi_side;

	interface Put subifc_put_loadparams;
		method Action put(Load_params parameters) if(rg_params matches tagged Invalid &&& rg_finish_load);
			//$display($time, "Received LOAD ,dram: %x, sram: %x, [%d, %d, %d], [%d, %d]", parameters.dram_address,
			//	parameters.sram_address, parameters.x_size, parameters.y_size, parameters.z_size,
			//	parameters.z_stride, parameters.y_stride);
			rg_finish_load <= False;
			rg_params <= tagged Valid parameters;
			rg_dram_addr <= parameters.dram_address;
			rg_sram_addr <= parameters.sram_address;
			Integer shift_len = (parameters.bitwidth ? valueOf(if_shift) : valueOf(of_shift));
			//of_shift will be max(if_shift,of_shift) and `DIM_WIDTH is the length of z_size
			Bit#(TAdd#(TAdd#(of_shift,1),`DIM_WIDTH1)) lv_shift = zeroExtend(parameters.z_size) << shift_len;
			Bit#(8) shift_op = truncate(lv_shift >> burst_len_shift);
			
      		rg_burst_len <= lv_shift >= fromInteger(val_data_bytes)  ? (shift_op - 1) : 0;

			rg_y_cntr <= parameters.y_size - 1;
			rg_x_cntr <= parameters.x_size - 1;
      		rg_z_cntr <= parameters.z_size;
			rg_load_requests <= True;
			rg_burst_addr <= tagged Invalid;
		endmethod
	endinterface
  
  method ActionValue#(SRAMReq#(max_index, max_bank, data_width)) write_data if(rg_params matches tagged Valid .params);
    return wr_buffer_req;
  endmethod

	method Bool send_interrupt = rg_interrupt;

	interface Get subifc_send_loadfinish;
		method ActionValue#(Bool) get if(isValid(rg_params) && rg_finish_load);
			rg_params <= tagged Invalid;
			//$display($time, "Load finish signal sent");
			return True;
		endmethod
	endinterface
endmodule

endpackage
