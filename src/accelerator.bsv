/* 
Author: Gokulan Ravi
Email id: gokulan97@gmail.com
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

  import GetPut::*;
  import AXI4_Fabric::*;
  import Connection::*;

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

    rule rl_write_data_ld_to_buf;
      Vector#(max_words, SRAMReq#(max_index, max_bank, max_data)) requests <- ld_module.write_data;
    endrule

    interface ifc_load_master = ld_module.master;
    interface ifc_store_master = st_module.master;
    interface ifc_fetch_master = fetch_module.master;
    interface ifc_fetch_slave = fetch_module.slave;
  endmodule
endpackage
