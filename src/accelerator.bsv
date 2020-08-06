/* 
Author: Gokulan Ravi, Vinod Ganesan
Email id: gokulan97@gmail.com, g.vinod1993@gmail.com
*/

package dnn_accelerator;
  import fetch_decode::*;
  import dependency_resolver::*;
  import load_module::*;
  import store_module::*;
  import compute_top::*;
  import tensor_alu::*;
  import onchip_buffers::*;

  `include "systolic.defines"

	import FIFOF::*;
  import GetPut::*;
  import AXI4_Fabric::*;
  import Connection::*;
	import BRAM::*;
	/*
  This is the top interface of the systolic accelrator. It contains 4 sub-interfaces: 
	1. A load master interface to the external bus for sending load requests
	2. A store master interface to the external bus for sending store requests
	3. A fetch master interface to the external bus for sending instruction fetch requests
	4. A fetch slave interface to the external bus for receiving read/write requests from the host
		 processor.
	*/
  interface Ifc_accelerator#(dram_addr_width, sram_addr_width, data_width,
                             wt_index, wt_bank, if_index, if_bank, of_index, of_bank,
                             in_width, out_width, nRow, nCol);
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_load_master;
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_store_master;
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_fetch_master;
    interface AXI4_Slave_IFC#(dram_addr_width, data_width, 0) ifc_fetch_slave;
  endinterface

  module mk_accelerator(Ifc_accelerator#(dram_addr_width, sram_addr_width, data_width,
                                         wt_index, wt_bank, if_index, if_bank, of_index, of_bank,
                                         in_width, out_width, nRow, nCol))
    provisos(Max#(wt_index, if_index, m_index),
             Max#(m_index, of_index, max_index),
             Max#(wt_bank, if_bank, m_bank),
             Max#(m_bank, of_bank, max_bank),
             Max#(in_width, out_width, max_width),
             Mul#(in_width, in_words, data_width),
             Mul#(out_width, out_words, data_width),
             Max#(in_words, out_words, max_words),
						 Log#(if_nbanks, if_bank), Log#(wt_nbanks, wt_bank), Log#(of_nbanks, of_bank),
						 Mul#(if_nfolds, in_words, if_nbanks), Mul#(wt_nfolds, in_words, wt_nbanks), Mul#(of_nfolds, out_words, of_nbanks),
             Eq#(D1, 8), Eq#(D2, 4), Eq#(boo, 1),
             Add#(dram_addr_width, sram_addr_width, a__), Mul#(D1, 5, b__), Mul#(boo, 2, c__),
             Add#(a__, b__, d__), Add#(d__, c__, e__), Add#(e__, mem_pad, 120),
             Add#(of_index, if_index, f__), Add#(wt_index, f__, g__),
             Mul#(6, D1, h__), Mul#(6, D2, i__), Add#(g__, h__, j__),
             Add#(j__, i__, k__), Add#(k__, boo, l__), Add#(l__, gemm_pad, 120),
             Mul#(2, of_index, m__), Mul#(7, D1, n__), Mul#(2, D2, o__),
             Add#(m__, n__, p__), Add#(p__, o__, q__), Mul#(2, bo, r__),
             Add#(q__, r__, s__), Add#(s__, alu_pad, 120)
             );

    Ifc_fetch_decode#(dram_addr_width, data_width) fetch_module <- mkfetch_decode;
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
                  if_index, if_bank, 
                  wt_index, wt_bank, 
                  of_index, of_bank, 
                  in_width, out_width) buffers <- mkbuffers;

    Ifc_compute_module#(dram_addr_width, sram_addr_width,
                  in_width, out_width, nRow, nCol,
                  if_index, if_bank,
                  wt_index, wt_bank,
                  of_index, of_bank) gemm_module <- mkgemm;

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
		mkConnection(tensor_alu.subifc_get_alu_complete, dependency_module.if_put_alu_complete);

    function BRAMRequest#(Bit#(a), Bit#(d)) makeRequest (Bool write, Bit#(a) addr, Bit#(d) data);
			return BRAMRequest{
				write: write,
				responseOnWrite: False,
				address : addr,
				datain : data
			};
    endfunction
		
		FIFOF#(SRAMReq#(max_index, max_bank, data_width)) ff_ld_module_requests <- mkFIFOF();

		rule rl_get_requests_from_load_module;
			let req <- ld_module.write_data();
			ff_ld_module_requests.enq(req);
		endrule

		for(Integer i=0; i<valueOf(if_nfolds); i=i+1)begin
			rule rl_ld_ifmap(ff_ld_module_requests.first.buffer == InputBuffer && ff_ld_module_requests.first.bank == fromInteger(i*in_words));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<in_words; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.ibuf[i*in_words+j].portB.request.put(makeRequest(truncate(req.index), req.data[(j+1)*in_width-1:j*in_width]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		for(Integer i=0; i<valueOf(wt_nfolds); i=i+1)begin
			rule rl_ld_wgts(ff_ld_module_requests.first.buffer == WeightBuffer && ff_ld_module_requests.first.bank == fromInteger(i*in_words));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<in_words; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.wbuf[i*in_words+j].portB.request.put(makeRequest(truncate(req.index), req.data[(j+1)*in_width-1:j*in_width]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_ld_ofmap(ff_ld_module_requests.first.buffer == OutputBuffer1 && ff_ld_module_requests.first.bank == fromInteger(i*out_words));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<out_words; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf1[i*out_words+j].portB.request.put(makeRequest(truncate(req.index), req.data[(j+1)*out_width-1:j*out_width]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_ld_ofmap2(ff_ld_module_requests.first.buffer == OutputBuffer2 && ff_ld_module_requests.first.bank == fromInteger(i*out_words));
				let req = ff_ld_module_requests.first;
				for(Integer j=0; j<out_words; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf2[i*out_words+j].portB.request.put(makeRequest(truncate(req.index), req.data[(j+1)*out_width-1:j*out_width]));
					end
				end
				ff_ld_module_requests.deq();
			endrule
		end

		//Rule to take a compute read request and giving it to the Input or weight buffer! 
		//Using portB for Input and Weight buffers to Compute
		//2 port SRAMs are not required for Input and Output Buffer -- TODO: Optimize it 

		(*mutually_exclusive = "rl_recv_read_req_ibuf_from_compute, rl_recv_read_req_wbuf_from_compute"*)
		rule rl_recv_read_req_ibuf_from_compute;
			Vector#(nRow, SRAMKRdReq#(if_index)) request <- gemm_module.get_inp_addr();
			for(Integer i = 0; i < vnRow; i=i+1) begin
				buffers.ibuf[i].portB.request.put(makeRequest(False, '0, request[i].index, ?));  
			end
		endrule
		
		(*mutually_exclusive = "rl_send_read_rsp_ibuf_compute, rl_send_read_rsp_wbuf_compute"*)
		rule rl_send_read_rsp_ibuf_compute;
			for(Integer i = 0; i < vnRow; i=i+1) begin
				let val <- buffers.ibuf[i].portB.response.get();
				gemm_module.put_inp_resp[i].put(val);
			end
		endrule

		rule rl_recv_read_req_wbuf_from_compute;
			Vector#(nRow, SRAMKRdReq#(wt_index)) request <- gemm_module.get_wt_addr();
			for(Integer i = 0; i < vnRow; i=i+1) begin
				buffers.wbuf[i].portB.request.put(makeRequest(False, '0, request[i].index, ?));
			end
		endrule


		rule rl_send_read_rsp_wbuf_compute;
			for(Integer i = 0; i < vnRow; i=i+1) begin
				let val <- buffers.wbuf[i].portB.response.get();
				gemm_module.put_wt_resp[i].put(val);
			end
		endrule

		//Rules to connect the output buffers
		//Using PortA for compute to obuf write 
		//Using PortB for compute to obuf read
		rule rl_send_write_req_obuf1_from_compute;
			Vector#(nCol, Get#(SRAMKWrReq#(of_index, out_width))) request <- gemm_module.get_new_output_data();
			for(Integer i = 0; i < vnCol; i=i+1) begin
				buffers.obuf1[i].portA.request.put(makeRequest(True, '1, request[i].index, request[i].data));
			end
		endrule

		rule rl_send_read_req_obuf1_from_compute;
			Vector#(nCol, Get#(SRAMKRdReq#(of_index))) request <- gemm_module.get_old_out_addr();
			for(Integer i = 0; i < vnCol; i=i+1) begin
				buffers.obuf1[i].portB.request.put(makeRequest(False, '1, request[i].index, request[i].data));
			end
		endrule

		rule rl_send_read_rsp_obuf1_to_compute;
			for(Integer i = 0; i < vnCol; i=i+1) begin
				let val <- buffers.obuf1[i].portB.response.get(); 
				gemm_module.put_old_out_resp[i].put(val);	
			end
		endrule

		//Using PortA for tensorALU to obuf2 write
		rule rl_send_read_req_obuf1_from_talu;
			let request <- tensor_alu.mv_send_req_op();
			for(Integer i = 0; i < vnCol; i=i+1) begin
				buffers.obuf1[i].portA.request.put(makeRequest(False, '0, tpl_1(request), ?)); 
			end
		endrule

		rule rl_send_read_rsp_obuf1_to_talu;
			Vector#(num_col, Bit#(alu_width)) vec_data = 0;
			for(Integer i = 0; i < vnCol; i=i+1) begin
				let val <- buffers.obuf1[i].portA.response.get(); 
				vec_data[i] = val;
			end
			tensor_alu.ma_recv_op(vec_data);
		endrule

		//TODO: Not sure what to do with the dim1 parameter coming out from tensorALU
		//Need to resolve this!
		rule rl_send_write_req_talu_to_obuf2;
			let res <- tensor_alu.mav_put_result();
			for(Integer i = 0; i < vnCol; i=i+1) begin
				buffers.obuf2[i].portB.request.put(makeRequest(True, '1, tpl_1(res), tpl_2(res)));
			end
		endrule

		FIFOF#(SRAMRdReq#(of_index, of_banks)) ff_req_from_store <- mkFIFOF();
		FIFOF#(Dim2) ff_num_valid_values <- mkFIFOF();

		rule rl_get_request_from_store;
			let req <- st_module.send_sram_req();
			ff_req_from_store.enq(req);
		endrule

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_st_ofmap(ff_req_from_store.first.buffer == OutputBuffer1 && ff_req_from_store.first.bank == fromInteger(i*out_words));
				let req = ff_req_from_store.first;
				let index = req.index;
				for(Integer j=0; j<out_words; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf1[i*out_words+j].portB.request.put(makeRequest(index, ?));
					end
				end
				ff_req_from_store.deq();
				ff_num_valid_values.enq(req.num_valid);
			endrule
		end

		for(Integer i=0; i<valueOf(of_nfolds); i=i+1)begin
			rule rl_ld_ofmap2(ff_req_from_store.first.buffer == OutputBuffer2 && ff_req_from_store.first.bank == fromInteger(i*out_words));
				let req = ff_req_from_store.first;
				let index = req.index;
				for(Integer j=0; j<out_words; j=j+1)begin
					if(fromInteger(j) < req.num_valid)begin
						buffers.obuf2[i*out_words+j].portB.request.put(makeRequest(index, ?));
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
				for(Integer i=0; i<valueOf(out_words); i=i+1)begin
					if(fromInteger(i) < num_valid)begin
						values[i] <- buffers.obuf1.portB.response.get();
					end
					else begin
						values[i] = 'b0;
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
				for(Integer i=0; i<valueOf(out_words); i=i+1)begin
					if(fromInteger(i) < num_valid)begin
						values[i] <- buffers.obuf2.portB.response.get();
					end
					else begin
						values[i] = 'b0;
					end
				end
				ff_num_valid_values.deq();
				st_module.recv_sram_resp(values);
			endrule
		end

    interface ifc_load_master = ld_module.master;
    interface ifc_store_master = st_module.master;
    interface ifc_fetch_master = fetch_module.master;
    interface ifc_fetch_slave = fetch_module.slave;
  endmodule
endpackage
