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

  `include "systolic.defines"

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
  interface Ifc_accelerator#(numeric type dram_addr_width, numeric type sram_addr_width, numeric type data_width,
                             numeric type wt_entries, numeric type wt_bank, numeric type if_entries, numeric type if_bank,
														 numeric type of_entries, numeric type of_bank,
                             numeric type in_width, numeric type out_width, numeric type nRow, numeric type nCol);
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_load_master;
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_store_master;
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_fetch_master;
    interface AXI4_Slave_IFC#(dram_addr_width, data_width, 0) ifc_fetch_slave;
  endinterface

	(*synthesize*)
  module mktop_tb(Ifc_accelerator#(32, 26, 128, 32768, 16, 32768, 16, 32768, 16, 16, 32, 16, 16));
    let ifc();
    mk_accelerator inst1(ifc);
    return (ifc);
  endmodule

		(*mutually_exclusive="rl_forward_old_output_from_obuf1_to_gemm, rl_send_read_rsp_obuf1_to_talu"*)
		(*mutually_exclusive="rl_forward_old_output_from_obuf2_to_gemm, rl_send_read_rsp_obuf2_to_talu"*)

  module mk_accelerator(Ifc_accelerator#(dram_addr_width, sram_addr_width, data_width,
                                         wt_entries, wt_nbanks, if_entries, if_nbanks, of_entries, of_nbanks,
                                         in_width, out_width, nRow, nCol))
    provisos(Log#(if_entries, if_index),
						 Log#(wt_entries, wt_index),
						 Log#(of_entries, of_index),
						 Log#(if_nbanks, if_bank), Log#(wt_nbanks, wt_bank), Log#(of_nbanks, of_bank),
						 Max#(wt_index, if_index, m_index),
             Max#(m_index, of_index, max_index),
             Max#(wt_bank, if_bank, m_bank),
             Max#(m_bank, of_bank, max_bank),
             Max#(in_width, out_width, max_width),
             Mul#(in_width, in_words, data_width),
             Mul#(out_width, out_words, data_width),
             Max#(in_words, out_words, max_words),
						 Mul#(if_nfolds, in_words, if_nbanks), Mul#(wt_nfolds, in_words, wt_nbanks), Mul#(of_nfolds, out_words, of_nbanks),
             //Add#(d1, 0, 8), Add#(d2, 0, 4), Add#(boo, 0, 1),
             //Add#(dram_addr_width, sram_addr_width, a__), Mul#(d1, 5, b__), Mul#(boo, 2, c__),
             //Add#(a__, b__, d__), Add#(d__, c__, e__), Add#(e__, mem_pad, 120),
             //Add#(of_index, if_index, f__), Add#(wt_index, f__, g__),
             //Mul#(4, d1, h__), Mul#(6, d2, i__), Add#(g__, h__, j__),
             //Add#(j__, i__, k__), Add#(k__, boo, l__), Add#(l__, gemm_pad, 120),
             //Mul#(2, of_index, m__), Mul#(7, d1, n__), Mul#(2, d2, o__),
             //Add#(m__, n__, p__), Add#(p__, o__, q__), Mul#(3, boo, r__),
             //Add#(q__, r__, s__), Add#(s__, alu_pad, 120),
						 Mul#(i__, 8, data_width), Add#(8, lm__, wt_index),
						 Add#(sram_addr_width, 0, 26), Add#(dram_addr_width, 0, 32),
						 Add#(8, xa__, of_index),
						 Add#(8, xb__, out_width),
						 Add#(xc__, in_width, out_width),
						 Add#(4, xd__, if_index),
						 Add#(xe__, out_width, data_width),
						 Add#(xf__, of_index, 26),
						 Add#(xg__, if_index, max_index), Add#(xh__, if_bank, max_bank),
						 Add#(xi__, wt_index, max_index), Add#(xj__, wt_bank, max_bank),
						 Add#(xk__, of_index, max_index), Add#(xl__, of_bank, max_bank),
						 Mul#(xm__, 8, in_width), Mul#(xn__, 8, out_width),
						 Add#(mem_pad, 0, 20),
						 Add#(if_index, TAdd#(of_index, TAdd#(wt_index, gemm_pad)), 63),
						 Add#(of_index, TAdd#(of_index, alu_pad), 53)
             );

    Ifc_fetch_decode#(dram_addr_width, data_width) fetch_decode <- mkfetch_decode;
    Ifc_dependency_resolver#(if_index, of_index, wt_index, mem_pad, mem_pad, gemm_pad, alu_pad)
                                                  dependency_module <- mkdependency_resolver;
    Ifc_load_Module#(dram_addr_width, data_width, sram_addr_width,
                   wt_index, wt_bank, in_width,
                   if_index, if_bank, in_width,
                   of_index, of_bank, out_width,
                   max_index, max_bank, max_width, max_words, mem_pad) ld_module <- mk_load_Module;
    Ifc_col2im#(dram_addr_width, data_width, sram_addr_width,
                   of_index, of_bank, out_width,
                   out_words, mem_pad) st_module <- mkcol2im;
    Ifc_onchip_buffers#(sram_addr_width, 
                  if_index, if_nbanks, if_entries,
                  wt_index, wt_nbanks, wt_entries,
                  of_index, of_nbanks, of_entries,
                  in_width, out_width) buffers <- mkbuffers;

    Ifc_compute_module#(dram_addr_width, sram_addr_width,
                  in_width, out_width, nRow, nCol,
                  if_index, wt_index, of_index, gemm_pad) gemm_module <- mkgemm;

		Ifc_tensor_alu#(out_width, nCol, of_index, alu_pad) tensor_alu <- mk_tensor_alu; 

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
		let iWidth = valueOf(in_width);
		let oWidth = valueOf(out_width);
		let vnRow = valueOf(nRow);
		let vnCol = valueOf(nCol);

		FIFOF#(SRAMReq#(max_index, max_bank, data_width)) ff_ld_module_requests <- mkFIFOF();

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
		for(Integer i=0; i<vnRow; i=i+1)begin
			rule rl_recv_read_req_ibuf_from_gemm;
				let req <- gemm_module.get_inp_addr[i].get();
				if(req.valid)begin
					buffers.ibuf[i].portB.request.put(makeRequest(False, req.index, ?));
				end
			endrule
		end

		for(Integer i=0; i<vnRow; i=i+1)begin
			rule rl_send_read_resp_ibuf_to_gemm;
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
			Vector#(nCol, Bit#(in_width)) weights = replicate(0);
			let num_valid = ff_wt_valid_cols.first;
			for(Integer i = 0; i < vnCol; i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					weights[i] <- buffers.wbuf[i].portB.response.get();
				end
			end
			gemm_module.put_wt_resp(weights);
			ff_wt_valid_cols.deq();
		endrule

		Vector#(nCol, FIFOF#(SRAMKRdReq#(of_index))) ff_old_out_reqs <- replicateM(mkFIFOF());
		Vector#(nCol, FIFOF#(Bool)) ff_out_which_buffer <- replicateM(mkFIFOF());

		for(Integer i=0; i<valueOf(nCol); i=i+1)begin
			rule rl_get_read_req_from_gemm;
				let req <- gemm_module.get_old_out_addr[i].get();
				ff_old_out_reqs[i].enq(req);
			endrule
			
			rule rl_send_gemm_read_req_to_obuf1(ff_old_out_reqs[i].first.valid);
				let req = ff_old_out_reqs[i].first;
				buffers.obuf1[i].portA.request.put(makeRequest(False, req.index, ?));
				ff_old_out_reqs[i].deq;
				ff_out_which_buffer[i].enq(True);
			endrule

			rule rl_send_gemm_read_req_to_obuf2(!ff_old_out_reqs[i].first.valid);
				let req = ff_old_out_reqs[i].first;
				buffers.obuf2[i].portA.request.put(makeRequest(False, req.index, ?));
				ff_old_out_reqs[i].deq();
				ff_out_which_buffer[i].enq(False);
			endrule

			(*mutually_exclusive = "rl_forward_old_output_from_obuf1_to_gemm, rl_forward_old_output_from_obuf2_to_gemm"*)
			(*mutually_exclusive = "rl_forward_old_output_from_obuf1_to_gemm, gemm_module.rl_send_init_acc_zero"*)
			(*mutually_exclusive = "rl_forward_old_output_from_obuf2_to_gemm, gemm_module.rl_send_init_acc_zero"*)

			rule rl_forward_old_output_from_obuf1_to_gemm(ff_out_which_buffer[i].first);
				let value <- buffers.obuf1[i].portA.response.get();
				gemm_module.put_old_out_resp[i].put(value);
			endrule

			rule rl_forward_old_output_from_obuf2_to_gemm(!ff_out_which_buffer[i].first);
				let value <- buffers.obuf2[i].portA.response.get();
				gemm_module.put_old_out_resp[i].put(value);
			endrule
		end

		Vector#(nCol, FIFOF#(SRAMKWrReq#(of_index, out_width))) ff_new_out_reqs <- replicateM(mkFIFOF());

		for(Integer i=0; i<valueOf(nCol); i=i+1)begin
			rule rl_get_write_req_from_gemm;
				let req <- gemm_module.get_new_output_data[i].get();
				ff_new_out_reqs[i].enq(req);
			endrule

			rule rl_write_out_from_gemm(ff_new_out_reqs[i].first.valid);
				let req = ff_new_out_reqs[i].first;
				buffers.obuf1[i].portB.request.put(makeRequest(True, req.index, req.data));
				ff_new_out_reqs[i].deq();
			endrule

			rule rl_write_out_from_gemm_to_obuf2(!ff_new_out_reqs[i].first.valid);
				let req = ff_new_out_reqs[i].first;
				buffers.obuf2[i].portB.request.put(makeRequest(True, req.index, req.data));
				ff_new_out_reqs[i].deq();
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
			for(Integer i=0; i<valueOf(nCol); i=i+1)begin
				if(fromInteger(i) < req.num_valid)begin
					buffers.obuf1[i].portA.request.put(makeRequest(False, req.index, ?));
				end
			end
			ff_req_in_tensor_alu.deq();
			ff_num_active_talu.enq(tuple2(req.num_valid, True));
		endrule

		rule rl_send_in_req_talu2(!ff_req_in_tensor_alu.first.buffer);
			let req = ff_req_in_tensor_alu.first;
			for(Integer i=0; i<valueOf(nCol); i=i+1)begin
				if(fromInteger(i) < req.num_valid)begin
					buffers.obuf2[i].portA.request.put(makeRequest(False, req.index, ?));
				end
			end
			ff_req_in_tensor_alu.deq();
			ff_num_active_talu.enq(tuple2(req.num_valid, False));
		endrule

		rule rl_send_read_rsp_obuf1_to_talu(tpl_2(ff_num_active_talu.first));
			Vector#(nCol, Bit#(out_width)) vec_data = replicate(0);
			let num_valid = tpl_1(ff_num_active_talu.first);
			for(Integer i = 0; i < valueOf(nCol); i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					let val <- buffers.obuf1[i].portA.response.get();
					vec_data[i] = val;
				end
			end
			tensor_alu.ma_recv_op(vec_data);
			ff_num_active_talu.deq();
		endrule
		
		rule rl_send_read_rsp_obuf2_to_talu(!tpl_2(ff_num_active_talu.first));
			Vector#(nCol, Bit#(out_width)) vec_data = replicate(0);
			let num_valid = tpl_1(ff_num_active_talu.first);
			for(Integer i = 0; i < valueOf(nCol); i=i+1) begin
				if(fromInteger(i) < num_valid)begin
					let val <- buffers.obuf2[i].portA.response.get();
					vec_data[i] = val;
				end
			end
			tensor_alu.ma_recv_op(vec_data);
			ff_num_active_talu.deq();
		endrule

		FIFOF#(TALUOutReq#(of_index, out_width, nCol)) ff_out_req_from_talu <- mkFIFOF();

		rule rl_recv_write_from_talu;
			let req <- tensor_alu.mav_put_result();
			ff_out_req_from_talu.enq(req);
		endrule

		rule rl_write_talu_output_to_obuf1(ff_out_req_from_talu.first.buffer);
			let req = ff_out_req_from_talu.first;
			for(Integer i=0; i<valueOf(nCol); i=i+1)begin
				if(fromInteger(i) < req.num_valid)begin
					buffers.obuf1[i].portB.request.put(makeRequest(True, req.index, req.values[i]));
				end
			end
		endrule

		rule rl_write_talu_output_to_obuf2(!ff_out_req_from_talu.first.buffer);
			let req = ff_out_req_from_talu.first;
			for(Integer i=0; i<valueOf(nCol); i=i+1)begin
				if(fromInteger(i) < req.num_valid)begin
					buffers.obuf2[i].portB.request.put(makeRequest(True, req.index, req.values[i]));
				end
			end
		endrule

		FIFOF#(SRAMRdReq#(of_index, of_bank)) ff_req_from_store <- mkFIFOF();
		FIFOF#(Dim2) ff_num_valid_values <- mkFIFOF();

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
				ff_ld_module_requests.deq();
				ff_num_valid_values.enq(req.num_valid);
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_send_resp_to_st_module;
				Vector#(out_words, Bit#(out_width)) values;
				Dim2 num_valid = ff_num_valid_values.first;
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
				Vector#(out_words, Bit#(out_width)) values;
				Dim2 num_valid = ff_num_valid_values.first;
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

    interface ifc_load_master = ld_module.master;
    interface ifc_store_master = st_module.master;
    interface ifc_fetch_master = fetch_decode.master;
    interface ifc_fetch_slave = fetch_decode.slave;
  endmodule
endpackage
