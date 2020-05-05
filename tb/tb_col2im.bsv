package tb_col2im;
import col2im   ::*;
import bram ::*;
import AXI4_Types:: *;
import Connectable :: *;
`include "Logger.bsv"
module mktb_col2im();
    Reg#(Bit#(32)) rg_cycle <- mkReg(0);
    /*
    Simulation Parameters:
    addr_width: 32 
    data_width: 64
    accumbanks/nCol: 7 (number of accumulator banks = number of systolic columns)
    mulWidth2: 64 (data_width of accumbanks)
    nRow: 7 (for a 7x7 systolic array)
    accumindexbits: 6 (64 entries in each bank)
    convColbits: 3 (total convolution o/p size can go upto 7x7)
    dram base_addr: 'h80000000
    window_output_col_size: 3 (3x3 filter size)
    */
    Ifc_col2im#(32,64,7,64,7,6,3) ifc_c2i <- mkcol2im('h80000000,3,3);
    Ifc_bram_axi4#(32,64,0,25) main_memory <- mkbram_axi4('h80000000,
                                                "code.mem", "MainMEM");
    mkConnection(ifc_c2i.axi_buffer_wreq, main_memory.slave);
    rule rl_cycle;
      rg_cycle <= rg_cycle +1;
      if(rg_cycle>20)
        $finish(0);
    endrule
    rule rl_init(rg_cycle==0);
        ifc_c2i.init('0,'d7,'d7,False);
    endrule
    rule rl_put_data;
        match {.valid,.bank_num,.bank_row_num} = ifc_c2i.outp_buffer_addr();
       `logLevel(tb,0,$format("cycle %d : valid %d : bank_num %d : row_num %d",rg_cycle,valid,bank_num,bank_row_num))
        if(valid)
            ifc_c2i.buffer_val(zeroExtend(rg_cycle));
    endrule
    rule rl_disp_axi_req;
        let lv_axi = ifc_c2i.axi_buffer_wreq;
       `logLevel(tb,0,$format("AXI: wvalid %d : wdata %d",lv_axi.m_wvalid,lv_axi.m_wdata))
    endrule
endmodule
endpackage