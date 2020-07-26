/* 
Author: Gokulan Ravi
Email id: gokulan97@gmail.com
*/

package dnn_accelerator;
  import load_module::*;
  import store_module::*;
  import compute_top::*;
  import tensor_alu::*;
  import onchip_buffers::*;

  `include "systolic.defines"

  import GetPut::*;
  import AXI4_Fabric::*;

  interface Ifc_accelerator#(dram_addr_width, sram_addr_width, data_width,
                             wt_index, wt_bank, if_index, if_bank, of_index, of_bank,
                             in_width, out_width, nRow, nCol);
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_load_master;
    interface AXI4_Master_IFC#(dram_addr_width, data_width, 0) ifc_store_master;
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
             Mul#(2, of_index, m__), Mul#(7, D1, n__), Mul#(4, D2, o__),
             Add#(m__, n__, p__), Add#(p__, o__, q__), Mul#(2, bo, r__),
             Add#(q__, r__, s__), Add#(s__, alu_pad, 120)
             );

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

    rule rl_write_data_ld_to_buf;
      Vector#(max_words, SRAMReq#(max_index, max_bank, max_data)) requests <- ld_module. write_data;
    endrule

    interface ifc_load_master = ld_module.master;
    interface ifc_store_master = st_module.master;
  endmodule


endpackage
