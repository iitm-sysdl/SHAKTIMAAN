package axi_test;

import Semi_FIFOF:: *;
import AXI4_Types:: *;
import AXI4_Fabric:: *;

import Connectable::*;

`define paddr 32
typedef 64 ELEN;
`define Num_Slaves 1

interface Ifc_axi_test;
    interface AXI4_Master_IFC#(`paddr, ELEN, 0) bus_mem_master;
    interface AXI4_Slave_IFC#(`paddr, ELEN,0) bus_mem_slave;
endinterface 

    function Tuple2#(Bool, Bit#(TLog#(`Num_Slaves))) fn_slave_map (Bit#(`paddr) addr);
      Bit#(TLog#(`Num_Slaves)) slave_num = 0;
      if(addr >= 'd2000 && addr<= 'd3000)
        slave_num = 0;
      slave_num = 0;
      return tuple2(True, slave_num);
    endfunction:fn_slave_map

(*synthesize*)
module mkaxi_test(Empty);
    AXI4_Fabric_IFC #(1, 1, `paddr, ELEN, 0) fabric <- mkAXI4_Fabric(fn_slave_map);

    AXI4_Slave_Xactor_IFC#(`paddr, ELEN, 0) s_xactor  <- mkAXI4_Slave_Xactor;
  	AXI4_Master_Xactor_IFC #(`paddr, ELEN, 0) m_xactor <- mkAXI4_Master_Xactor;

    mkConnection(m_xactor.axi_side, fabric.v_from_masters[0]);
    mkConnection(fabric.v_to_slaves[0], s_xactor.axi_side);

    Reg#(Bit#(32)) count <- mkReg(1024);

    rule rl_master_send_request(count > 0);
      count <= count - 1;

      let wrreq = AXI4_Wr_Addr {awaddr: 2000, 
											 awid: 0, awlen: 0, awprot:0,
											 awsize: 0, awburst: 0, //arburst: 00-FIXED 01-INCR 10-WRAP
											 awuser: 0 };
      let w  = AXI4_Wr_Data {wdata : {32'b0, count}, wstrb : '1, wlast : True,                  
                             wid : 0}; 
      
      m_xactor.i_wr_addr.enq(wrreq);
      m_xactor.i_wr_data.enq(w);
      $display($time, "Count: %d", count); 
//      $display($time, "Sending data %d\n", w.wdata);
    endrule


    rule rl_master_ack;
      let ar <- pop_o(m_xactor.o_wr_resp);
    endrule

    rule rl_slave_recv_request;
      let ar <- pop_o(s_xactor.o_wr_addr);
      let ad <- pop_o(s_xactor.o_wr_data);
      
      let resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: ar.awuser, bid:ar.awid};
      s_xactor.i_wr_resp.enq(resp);

      $display($time, "Receiving data %d\n", ad.wdata);  
    endrule

endmodule

endpackage

