/* 
Author: Gokulan Ravi, Vinod Ganesan
Email id: gokulan97@gmail.com, g.vinod1993@gmail.com
*/

package accelerator;
  import fetch_decode::*;
  import dependency_resolver::*;
  import load_module::*;
  import store_module::*;
  import compute_top::*;
  import tensor_alu::*;
  import onchip_buffers::*;
	import isa::*;
	import ConcatReg::*;

  `include "systolic.defines"

	import DReg::*;
	import FIFOF::*;
  import GetPut::*;
  import AXI4_Fabric::*;
	import AXI4_Types::*;
  import Connectable::*;
	import BRAM::*;
	import Vector::*;

	/*
  This is the top interface of the systolic accelrator. It contains 4 sub-interfaces: 
	1. A load master interface to the external bus for sending load requests
	2. A store master interface to the external bus for sending store requests
	3. A fetch master interface to the external bus for sending instruction fetch requests
	4. A fetch slave interface to the external bus for receiving read/write requests from the host
		 processor.
	*/
  interface Ifc_accelerator;
    interface AXI4_Master_IFC#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH, 0) ifc_load_master;
    interface AXI4_Master_IFC#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH, 0) ifc_store_master;
    interface AXI4_Master_IFC#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH, 0) ifc_fetch_master;
    interface AXI4_Slave_IFC#(`DRAM_ADDR_WIDTH, `AXI_DATAWIDTH, 0) ifc_fetch_slave;
		method Bool send_interrupt_to_proc;
  endinterface

	(*synthesize*)
  module mkshaktimaan(Ifc_accelerator);
    let ifc();
    mk_accelerator inst1(ifc);
    return (ifc);
  endmodule

  (*synthesize*)
  module mkfetchdecode(Ifc_fetch_decode#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH));
    let ifc();
    mkfetch_decode _temp(ifc);
    return ifc;
  endmodule

  (*synthesize*)
  module mkdepend_resolver(Ifc_dependency_resolver);
    let ifc();
    mkdependency_resolver _temp(ifc);
    return ifc;
  endmodule

  (*synthesize*)
  module mkonchip_buffers(Ifc_onchip_buffers#(`SRAM_ADDR_WIDTH,`IBUF_ENTRIES,`IBUF_BANKS,`WBUF_ENTRIES,`WBUF_BANKS,`OBUF_ENTRIES,`OBUF_BANKS,`INWIDTH,`OUTWIDTH));
    let ifc();
    mkbuffers _temp(ifc);
    return ifc;
  endmodule

  (*synthesize*)
  module mkload(Ifc_load_Module#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH,TLog#(`WBUF_ENTRIES),TLog#(`WBUF_BANKS),`INWIDTH,TLog#(`IBUF_ENTRIES),TLog#(`IBUF_BANKS),`INWIDTH,TLog#(`OBUF_ENTRIES),TLog#(`OBUF_BANKS),`OUTWIDTH,TMax#(TLog#(`OBUF_ENTRIES),TMax#(TLog#(`IBUF_ENTRIES),TLog#(`WBUF_ENTRIES))),TMax#(TLog#(`OBUF_BANKS),TMax#(TLog#(`IBUF_BANKS),TLog#(`WBUF_BANKS)))));
      let ifc();
      mk_load_Module inst1(ifc);
      return (ifc);
  endmodule

  (*synthesize*)
  module mkstore(Ifc_col2im#(`DRAM_ADDR_WIDTH, `AXI_DATAWIDTH, `SRAM_ADDR_WIDTH, `INWIDTH, TLog#(`OBUF_ENTRIES), TLog#(`OBUF_BANKS), `OUTWIDTH, TDiv#(`AXI_DATAWIDTH,`OUTWIDTH)));
    let ifc();
    mkcol2im _temp(ifc);
    return ifc;
  endmodule

  (*synthesize*)
  module mkcompute(Ifc_compute_module#(`DRAM_ADDR_WIDTH,`SRAM_ADDR_WIDTH,`INWIDTH,`OUTWIDTH,`NUMROWS,`NUMCOLS,TLog#(`IBUF_ENTRIES),TLog#(`WBUF_ENTRIES),TLog#(`OBUF_ENTRIES)));
    let ifc();
    mkgemm inst1(ifc);
    return (ifc);
  endmodule

  (*synthesize*)
  module mktensoralu(Ifc_tensor_alu#(`OUTWIDTH, `NUMCOLS, TLog#(`OBUF_ENTRIES)));
  	let ifc();
  	mk_tensor_alu _temp(ifc);
  	return ifc;
  endmodule

	(*mutually_exclusive="rl_forward_old_output_from_obuf1_to_gemm, rl_send_read_rsp_obuf1_to_talu"*)
	(*mutually_exclusive="rl_forward_old_output_from_obuf2_to_gemm, rl_send_read_rsp_obuf2_to_talu"*)
  module mk_accelerator(Ifc_accelerator)
    provisos(Log#(`IBUF_ENTRIES, if_index),Log#(`WBUF_ENTRIES, wt_index),Log#(`OBUF_ENTRIES, of_index),
						 Log#(`IBUF_BANKS, if_bank), Log#(`WBUF_BANKS, wt_bank), Log#(`OBUF_BANKS, of_bank),
						 Max#(wt_index, if_index, m_index),Max#(m_index, of_index, max_index),
             Max#(wt_bank, if_bank, m_bank),Max#(m_bank, of_bank, max_bank),
             Max#(`INWIDTH, `OUTWIDTH, max_width),
             Mul#(`INWIDTH, in_words, `AXI_DATAWIDTH),Mul#(`OUTWIDTH, out_words, `AXI_DATAWIDTH),
						 Mul#(if_nfolds, in_words, `IBUF_BANKS), Mul#(wt_nfolds, in_words, `WBUF_BANKS), Mul#(of_nfolds, out_words, `OBUF_BANKS),
						 Add#(xc__, `INWIDTH, `OUTWIDTH),
						 Add#(xg__, if_index, max_index), Add#(xh__, if_bank, max_bank),
						 Add#(xi__, wt_index, max_index), Add#(xj__, wt_bank, max_bank), 
						 Add#(xk__, of_index, max_index), Add#(xl__, of_bank, max_bank)
						 );

		// Having a status register to keep track 
		// 0th bit -> Idle/busy, 1st bit -> Frontend error, 2nd bit -> Load error, 3rd bit -> Store error
		Reg#(Bit#(1)) rg_idle <- mkReg(0); 
		Reg#(Bit#(1)) rg_frontend_error <- mkReg(0); 
		Reg#(Bit#(1)) rg_load_error <- mkReg(0); 
		Reg#(Bit#(1)) rg_store_error <- mkReg(0);

		//Reg#(Bit#(8)) statusReg = concatReg8(1'b0,1'b0,1'b0,1'b0,rg_store_error, rg_frontend_error, rg_load_error, rg_idle);

		Vector#(`NUMCOLS, Reg#(Bool)) rg_inp_req_valid <- replicateM(mkDReg(False));

    let fetch_decode <- mkfetchdecode;
    let dependency_module <- mkdepend_resolver;
    let ld_module <- mkload;
    let st_module <- mkstore;
    let  buffers <- mkonchip_buffers;
	let gemm_module <- mkcompute;
	let tensor_alu <- mktensoralu; 

    mkConnection(fetch_decode.ifc_get_load_params, dependency_module.ifc_put_load_params);
    mkConnection(fetch_decode.ifc_get_store_params, dependency_module.ifc_put_store_params);
    mkConnection(fetch_decode.ifc_get_compute_params, dependency_module.ifc_put_compute_params);
    mkConnection(fetch_decode.ifc_get_alu_params, dependency_module.ifc_put_alu_params);

    mkConnection(dependency_module.ifc_get_load_instruction, ld_module.subifc_put_loadparams);
    mkConnection(ld_module.subifc_send_loadfinish, dependency_module.ifc_put_load_complete);

    mkConnection(dependency_module.ifc_get_store_instruction, st_module.subifc_put_storeparams);
    mkConnection(st_module.subifc_send_store_finish, dependency_module.ifc_put_store_complete);

    mkConnection(dependency_module.ifc_get_gemm_instruction, gemm_module.subifc_put_compute_params);
    mkConnection(gemm_module.subifc_get_compute_finish, dependency_module.ifc_put_gemm_complete);

		mkConnection(dependency_module.ifc_get_alu_instruction, tensor_alu.subifc_put_alu_params);
		mkConnection(tensor_alu.subifc_get_alu_complete, dependency_module.ifc_put_alu_complete);

    function BRAMRequest#(Bit#(a), Bit#(d)) makeRequest (Bool write, Bit#(a) addr, Bit#(d) data);
			return BRAMRequest{
				write: write,
				responseOnWrite: False,
				address : addr,
				datain : data
			};
    endfunction

		let iWords = valueOf(in_words);
		let oWords = valueOf(out_words);
		let iWidth = valueOf(`INWIDTH);
		let oWidth = valueOf(`OUTWIDTH);
		let vnRow = valueOf(`NUMROWS);
		let vnCol = valueOf(`NUMCOLS);

		FIFOF#(SRAMReq#(max_index, max_bank, `AXI_DATAWIDTH)) ff_ld_module_requests <- mkFIFOF();

		rule rl_get_requests_from_load_module;
			let req <- ld_module.write_data();
			ff_ld_module_requests.enq(req);
		endrule

		for(Integer i=0; i<valueOf(if_nfolds); i=i+1)begin
			rule rl_ld_ifmap(ff_ld_module_requests.first.buffer == InputBuffer && ff_ld_module_requests.first.bank == fromInteger(i*iWords));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<iWords; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.ibuf[i*iWords+j].portA.request.put(makeRequest(True, truncate(req.index), req.data[(iWords-j)*iWidth-1:(iWords-1-j)*iWidth]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		for(Integer i=0; i<valueOf(wt_nfolds); i=i+1)begin
			rule rl_ld_wgts(ff_ld_module_requests.first.buffer == WeightBuffer && ff_ld_module_requests.first.bank == fromInteger(i*iWords));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<iWords; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.wbuf[i*iWords+j].portA.request.put(makeRequest(True, truncate(req.index), req.data[(iWords-j)*iWidth-1:(iWords-j-1)*iWidth]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_ld_ofmap(ff_ld_module_requests.first.buffer == OutputBuffer1 && ff_ld_module_requests.first.bank == fromInteger(i*oWords));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<oWords; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf1[i*oWords+j].portB.request.put(makeRequest(True, truncate(req.index), req.data[(oWords-j)*oWidth-1:(oWords-j-1)*oWidth]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_ld_ofmap2(ff_ld_module_requests.first.buffer == OutputBuffer2 && ff_ld_module_requests.first.bank == fromInteger(i*oWords));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<oWords; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf2[i*oWords+j].portB.request.put(makeRequest(True, truncate(req.index), req.data[(oWords-j)*oWidth-1:(oWords-j-1)*oWidth]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		//Rule to take a compute read request and giving it to the Input or weight buffer! 
		//Using portB for Input and Weight buffers to Compute

		//(*mutually_exclusive = "rl_recv_read_req_ibuf_from_compute, rl_recv_read_req_wbuf_from_compute"*)
		
		//rule rl_recv_read_req_ibuf_from_gemm;
		//	let {index, active_rows} <- gemm_module.get_inp_addr.get();
		//	//let inp_addr = gemm_module.get_inp_addr();
		//	//let index = tpl_1(inp_addr);
		//	//let active_rows = tpl_2(inp_addr);
		//	for(Integer i=0; i<vnRow; i=i+1)begin
		//		if(fromInteger(i) < active_rows)begin
		//			buffers.ibuf[i].portB.request.put(makeRequest(False, index, ?));
		//		end
		//	end
		//endrule

		for(Integer i=0; i<vnRow; i=i+1)begin
			rule rl_recv_read_req_ibuf_from_gemm;
				let req <- gemm_module.get_inp_addr[i].get();
				if(req.pad_zero && req.valid)begin
					rg_inp_req_valid[i] <= True;
				end
				else if(req.valid)begin
					buffers.ibuf[i].portB.request.put(makeRequest(False, req.index, ?)); //TOCHECK
				end
				//else begin
				//	gemm_module.put_inp_resp[i].put(0); //This should fix partial row input problem if top half of systolic is properly used
				//end
			endrule
		end

		for(Integer i=0; i<vnRow; i=i+1)begin
			rule rl_send_read_resp_zero_to_gemm(rg_inp_req_valid[i]);
				gemm_module.put_inp_resp[i].put(0);
			endrule
		
			rule rl_send_read_resp_ibuf_to_gemm(!rg_inp_req_valid[i]);
				let value <- buffers.ibuf[i].portB.response.get();
				gemm_module.put_inp_resp[i].put(value);
			endrule
		end

		FIFOF#(Dim1) ff_wt_valid_cols <- mkFIFOF();

		rule rl_recv_read_req_wbuf_from_compute;
			let {index, num_valid} <- gemm_module.get_wt_addr();
			for(Integer i = 0; i < vnCol; i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					buffers.wbuf[i].portB.request.put(makeRequest(False, index, ?));
				end
			end
			ff_wt_valid_cols.enq(num_valid);
		endrule

		rule rl_send_read_rsp_wbuf_compute;
			Vector#(`NUMCOLS, Bit#(`INWIDTH)) weights = replicate(0);
			let num_valid = ff_wt_valid_cols.first;
			for(Integer i = 0; i < vnCol; i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					weights[i] <- buffers.wbuf[i].portB.response.get();
				end
			end
			gemm_module.put_wt_resp(weights);
			ff_wt_valid_cols.deq();
		endrule

		//FIFOF#(SRAMKRdReq#(of_index)) ff_old_out_reqs			<- mkFIFOF();
		Vector#(`NUMCOLS,FIFOF#(Bool))					ff_out_which_buffer <- replicateM(mkFIFOF());

		//rule rl_get_read_req_from_gemm;
		//	let req <- gemm_module.get_old_out_addr.get();
		//	ff_old_out_reqs.enq(req);
		//endrule

         		

		for(Integer i=0; i<valueOf(`NUMCOLS); i=i+1)begin
			//(*mutually_exclusive = "rl_forward_old_output_from_obuf1_to_gemm, rl_forward_old_output_from_obuf2_to_gemm"*)
			//(*mutually_exclusive = "rl_forward_old_output_from_obuf1_to_gemm, gemm_module.rl_send_init_acc_zero"*)
			//(*mutually_exclusive = "rl_forward_old_output_from_obuf2_to_gemm, gemm_module.rl_send_init_acc_zero"*)

			rule rl_send_gemm_read_req_to_obuf;
         			//let {index, active_cols} = ff_old_out_reqs.first;
         			let req <- gemm_module.get_old_out_addr[i].get();
         			//let index = tpl_1(out_addr);
         			//let active_cols = tpl_2(out_addr);
         			Bool which_buffer = req.index >> (valueOf(of_index)-1) == 'b1;
				if(req.valid) begin
         			 if(which_buffer) begin
         			 	buffers.obuf2[i].portA.request.put(makeRequest(False, req.index, ?)); //TOCHECK
         			 end
         			 else begin
         				buffers.obuf1[i].portA.request.put(makeRequest(False, req.index, ?)); //TOCHECK
         			 end
				end
         			ff_out_which_buffer[i].enq(which_buffer);
         		endrule

			rule rl_forward_old_output_from_obuf1_to_gemm(ff_out_which_buffer[i].first);
				let value <- buffers.obuf1[i].portA.response.get();
				gemm_module.put_old_out_resp[i].put(value);
				ff_out_which_buffer[i].deq();
			endrule

			rule rl_forward_old_output_from_obuf2_to_gemm(!ff_out_which_buffer[i].first);
				let value <- buffers.obuf2[i].portA.response.get();
				gemm_module.put_old_out_resp[i].put(value);
				ff_out_which_buffer[i].deq();
			endrule

         		rule rl_get_write_req_from_gemm;
         			let write_req <- gemm_module.get_new_output_data[i].get();
         			//let values = tpl_1(write_req);
         			//let index = tpl_2(write_req);
         			//let active_cols = tpl_3(write_req);
         			if(!write_req.valid)begin //TOCHECK
         				buffers.obuf1[i].portB.request.put(makeRequest(True, write_req.index, write_req.data));
         			end
         			else begin
         				buffers.obuf2[i].portB.request.put(makeRequest(True, write_req.index, write_req.data));
         			end
         		endrule
	        end

		FIFOF#(TALUOpReq#(of_index)) ff_req_in_tensor_alu <- mkFIFOF();
		FIFOF#(Tuple2#(Dim1, Bool)) ff_num_active_talu <- mkFIFOF();

		rule rl_recv_req_from_talu;
			let req <- tensor_alu.mv_send_req_op();
			ff_req_in_tensor_alu.enq(req);
		endrule

		rule rl_send_in_req_talu(ff_req_in_tensor_alu.first.buffer);
			let req = ff_req_in_tensor_alu.first;
			for(Integer i=0; i<valueOf(`NUMCOLS); i=i+1)begin
				//if(fromInteger(i) < req.num_valid)begin
					buffers.obuf1[i].portA.request.put(makeRequest(False, req.index, ?));
				//end
			end
			ff_req_in_tensor_alu.deq();
			ff_num_active_talu.enq(tuple2(req.num_valid, True));
		endrule

		rule rl_send_in_req_talu2(!ff_req_in_tensor_alu.first.buffer);
			let req = ff_req_in_tensor_alu.first;
			for(Integer i=0; i<valueOf(`NUMCOLS); i=i+1)begin
				//if(fromInteger(i) < req.num_valid)begin
					buffers.obuf2[i].portA.request.put(makeRequest(False, req.index, ?));
				//end
			end
			ff_req_in_tensor_alu.deq();
			ff_num_active_talu.enq(tuple2(req.num_valid, False));
		endrule

		rule rl_send_read_rsp_obuf1_to_talu(tpl_2(ff_num_active_talu.first));
			let num_valid = tpl_1(ff_num_active_talu.first);
			for(Integer i = 0; i < valueOf(`NUMCOLS); i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					let val <- buffers.obuf1[i].portA.response.get();
					tensor_alu.subifc_recv_op[i].put(val);
				end
			end
			ff_num_active_talu.deq();
		endrule
		
		rule rl_send_read_rsp_obuf2_to_talu(!tpl_2(ff_num_active_talu.first));
			let num_valid = tpl_1(ff_num_active_talu.first);
			for(Integer i = 0; i < valueOf(`NUMCOLS); i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					let val <- buffers.obuf2[i].portA.response.get();
					tensor_alu.subifc_recv_op[i].put(val);
				end
			end
			ff_num_active_talu.deq();
		endrule

		FIFOF#(TALUOutReq#(of_index, `OUTWIDTH, `NUMCOLS)) ff_out_req_from_talu <- mkFIFOF();

		rule rl_recv_write_from_talu;
			let req <- tensor_alu.mav_put_result();
			ff_out_req_from_talu.enq(req);
		endrule

		rule rl_write_talu_output_to_obuf1(ff_out_req_from_talu.first.buffer);
			let req = ff_out_req_from_talu.first;
			for(Integer i=0; i<valueOf(`NUMCOLS); i=i+1)begin
				if(fromInteger(i) < req.num_valid)begin
					buffers.obuf1[i].portB.request.put(makeRequest(True, req.index, req.values[i]));
				end
			end
		endrule

		rule rl_write_talu_output_to_obuf2(!ff_out_req_from_talu.first.buffer);
			let req = ff_out_req_from_talu.first;
			for(Integer i=0; i<valueOf(`NUMCOLS); i=i+1)begin
				if(fromInteger(i) < req.num_valid)begin
					buffers.obuf2[i].portB.request.put(makeRequest(True, req.index, req.values[i]));
				end
			end
		endrule

		FIFOF#(SRAMRdReq#(of_index, of_bank)) ff_req_from_store <- mkFIFOF();
		FIFOF#(Sram_valid) ff_num_valid_values <- mkFIFOF();

		rule rl_get_request_from_store;
			let req <- st_module.send_sram_req();
			ff_req_from_store.enq(req);
		endrule

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_st_ofmap(ff_req_from_store.first.buffer == OutputBuffer1 && ff_req_from_store.first.bank == fromInteger(i*oWords));
				let req = ff_req_from_store.first;
				let index = req.index;
				for(Integer j=0; j<oWords; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf1[i*oWords+j].portB.request.put(makeRequest(False, index, ?));
					end
				end
				ff_req_from_store.deq();
				ff_num_valid_values.enq(req.num_valid);
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_ld_ofmap2(ff_req_from_store.first.buffer == OutputBuffer2 && ff_req_from_store.first.bank == fromInteger(i*oWords));
				let req = ff_req_from_store.first;
				let index = req.index;
				for(Integer j=0; j<oWords; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf2[i*oWords+j].portB.request.put(makeRequest(False, index, ?));
					end
				end
				ff_req_from_store.deq();
				ff_num_valid_values.enq(req.num_valid);
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_send_resp_to_st_module;
				Vector#(out_words, Bit#(`OUTWIDTH)) values;
				Sram_valid num_valid = ff_num_valid_values.first;
				for(Integer j=0; j<valueOf(out_words); j=j+1)begin
					if(fromInteger(j) < num_valid)begin
						values[j] <- buffers.obuf1[i*oWords+j].portB.response.get();
					end
					else begin
						values[j] = 'b0;
					end
				end
				ff_num_valid_values.deq();
				st_module.recv_sram_resp(values);
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_send_resp_to_st_module2;
				Vector#(out_words, Bit#(`OUTWIDTH)) values;
				Sram_valid num_valid = ff_num_valid_values.first;
				for(Integer j=0; j<valueOf(out_words); j=j+1)begin
					if(fromInteger(j) < num_valid)begin
						values[j] <- buffers.obuf2[i*oWords+j].portB.response.get();
					end
					else begin
						values[j] = 'b0;
					end
				end
				ff_num_valid_values.deq();
				st_module.recv_sram_resp(values);
			endrule
		end

		// Need to expose this to the slave interface somehow 
		rule rl_update_status_register;
			if(fetch_decode.send_interrupt) begin 
				rg_frontend_error <= 1; 
			end
			else if(ld_module.send_interrupt) begin
				rg_load_error <= 1;
			end
			else if(st_module.send_interrupt) begin
				rg_store_error <= 1;
			end
		endrule

		method Bool send_interrupt_to_proc = st_module.send_interrupt || ld_module.send_interrupt || fetch_decode.send_interrupt;
			

    interface ifc_load_master = ld_module.master;
    interface ifc_store_master = st_module.master;
    interface ifc_fetch_master = fetch_decode.master;
    interface ifc_fetch_slave = fetch_decode.slave;
  endmodule
endpackage
