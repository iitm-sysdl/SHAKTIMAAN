/* 
Author: Vinod Ganesan
Email id: g.vinod1993@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package fetchDecode;

import  frontend_common::*;
import  GetPut::*;
import  Connectable::*;
import Semi_FIFOF::*;
import  AXI4_Types  ::*;
import  AXI4_Fabric ::*;
import  FIFOF::*;

`define LOAD  'h0
`define STORE 'h1
`define SET_PC 'h0
`define RESET_PC 'h1

//Temporary defines -- will go off during integration
`define ADDR_WIDTH 128
`define DATA_WIDTH 128


interface Ifc_fetchDecode;
   
   //Slave Ifc to receive initial PC, reset PC etc. 
   //Master IFC to send and receive request/response from Memory 
   interface AXI4_Slave_IFC#(`ADDR_WIDTH,`DATA_WIDTH,0) fetch_slave; 
   interface AXI4_Master_IFC#(`ADDR_WIDTH, `DATA_WIDTH, 0) master;
   interface Get#(Bit#(ILEN))  toloadQdep;
   interface Get#(Bit#(ILEN))  tocomputeQdep;
   interface Get#(Bit#(ILEN))  tostoreQdep;

   //method Bit#(TAdd#(ILEN,1)) toDepResolver;
endinterface

//For verification Purposes
interface Ifc_slave;
  interface AXI4_Slave_IFC#(`ADDR_WIDTH, `DATA_WIDTH, 0) slave;
  interface AXI4_Master_IFC#(`ADDR_WIDTH, `DATA_WIDTH, 0) fetch_master;
endinterface 

//TODO Define the following parameters 
// ILEN, insSize

//typedef struct{
//  Bit#(opWidth) opCode;
//  Bit#(4) deptFlags;
//
//}instLen#(numric type opWidth) deriving(Bits, Eq);


module mkfetchDecode(Ifc_fetchDecode);

  //iLEN = valueOf(ILEN);
  //opwidth = valueOf(opWidth);:
  //Data-structures required for fetch and Decode
  Reg#(Bit#(ILEN)) pc <- mkReg(-1); 

  //Room for optimization.. Qwidth can be <ILEN, ILEN is overprovisioned
  FIFOF#(Bit#(ILEN)) instrQueue  <- mkSizedFIFOF(valueOf(InsSize));
 
  Wire#(Bit#(ILEN)) wr_loadQ  <- mkWire();
  Wire#(Bit#(ILEN)) wr_storeQ <- mkWire();
  Wire#(Bit#(ILEN)) wr_computeQ <- mkWire();

  AXI4_Master_Xactor_IFC #(`ADDR_WIDTH, `DATA_WIDTH, 0) m_xactor <- mkAXI4_Master_Xactor;  
  AXI4_Slave_Xactor_IFC#(`ADDR_WIDTH,`DATA_WIDTH,0) s_xactor  <- mkAXI4_Slave_Xactor;

 
  function Action set_fetch(Bit#(`ADDR_WIDTH) addr, Bit#(`DATA_WIDTH) data);
    action
      case(addr)
          `SET_PC  : pc <= extend(data);
          `RESET_PC : pc <= '1;
      endcase
    endaction
  endfunction

  //Configuration Space to write PC
  rule rlAXIwrConfig;

     let aw <- pop_o(s_xactor.o_wr_addr);
     let w  <- pop_o(s_xactor.o_wr_data);
     let lv_addr = aw.awaddr;
     let lv_data = w.wdata;
     
     $display($time, "\t Setting Config Address %h %h", lv_addr, lv_data);
     set_fetch(lv_addr, truncate(lv_data));
     
     let resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
     s_xactor.i_wr_resp.enq (resp);

  endrule

  //Simple condition to check now pc!='1, can replace with regWriteSideEffect
  //TODO Normal fetch requests have a 3 cycle latency - should this be a burst? 
  rule sendReqtoMem(pc!='1);
        let nextPC = pc+fromInteger(valueOf(ILEN));
        pc <= nextPC;
        let read_request = AXI4_Rd_Addr {araddr: nextPC, 
                         arid: {4'b0101}, arlen: 0,
                         arsize: '1, arburst: 2'b00, //arburst: 00-FIXED 01-INCR 10-WRAP
                         aruser: 0 };  //0101 --random id to mark the requests
          
        m_xactor.i_rd_addr.enq(read_request);
  endrule

  rule recvReqfromMem;
        let resp <- pop_o(m_xactor.o_rd_data);
        let inst = resp.rdata;
        instrQueue.enq(inst);
  endrule

  //This rule may not fire if any of the queue is full.. should use 
  //aggressive conditions or something, or break it into multiple queues 
  rule decodeInstr(instrQueue.notEmpty());
    let inst = instrQueue.first;
    instrQueue.deq();
    Bit#(4) opcodE = inst[valueOf(ILEN)-1:valueOf(ILEN)-valueOf(OpWidth)+1];
    //This is useless here right? 
    if(opcodE == `LOAD) begin
      wr_loadQ <= inst;
    end
    else if(opcodE == `STORE) begin
      wr_storeQ <= inst;
    end
    else begin
      wr_computeQ <= inst;
    end  
  endrule
  

   interface fetch_slave = s_xactor.axi_side; 
   interface master = m_xactor.axi_side;


   interface Get toloadQdep;
    method ActionValue#(Bit#(ILEN)) get;
      return wr_loadQ;
    endmethod
   endinterface

   interface Get tostoreQdep;
    method ActionValue#(Bit#(ILEN)) get;
      return wr_storeQ;
    endmethod
   endinterface

  interface Get tocomputeQdep;
    method ActionValue#(Bit#(ILEN)) get;
      return wr_computeQ;
    endmethod
   endinterface

//Method going to Dependency Resolver Module
  //method Bit#(TAdd#(ILEN,1)) toDepResolver;
  //  return wr_val;
  //endmethod

endmodule

//Unit testing
//module mkslave and a mkTb that connects master and slave 
//Test scenarios 
/* 1. Able to send a successful AXI request 
   2. Receiving an AXI response with a dummy instruction
   3. Able to decode the dummy instruction and enqueue into the necessary Command Queue
   4. Instruction fetch starts after PC is set 
   TODO
   to test the following scenarios,
   1. When the command queues are full, the fetch still happens and fills the instruction queue 
   2. All buffer full and buffer empty conditions are working as intended 
*/

module mkslave(Ifc_slave);

  AXI4_Master_Xactor_IFC #(`ADDR_WIDTH, `DATA_WIDTH, 0) m_tb_xactor <- mkAXI4_Master_Xactor;  
  AXI4_Slave_Xactor_IFC#(`ADDR_WIDTH,`DATA_WIDTH,0) s_tb_xactor  <- mkAXI4_Slave_Xactor;
  Reg#(Bit#(ILEN)) rg_inst <- mkReg({3'b000,4'b0000,'1});
  Reg#(Bit#(1)) rg_start <- mkReg(0);


  //Check this write response dequeue
  //rule rl_deq_wr_resp;
  //  m_tb_xactor.i_wr_resp.deq();
  //endrule

  rule set_pc(rg_start == 0);
    $display("Setting PC");
    rg_start <= 1;
    let write_data = AXI4_Wr_Data { wdata: '0, wstrb: '1, wlast: True, wid: ?};
    let write_addr = AXI4_Wr_Addr { awaddr: zeroExtend(4'b1010), awuser: 0, awlen: 0,
                          awsize: 0, awburst: 0, awid: ?};
    // enqueue the request.
    m_tb_xactor.i_wr_data.enq(write_data);
    m_tb_xactor.i_wr_addr.enq(write_addr);
 
  endrule

  rule rl_AXI_rd_req(rg_start == 1);
    let rd_req <- pop_o(s_tb_xactor.o_rd_addr);
    let lv_resp= AXI4_Rd_Data {rresp:AXI4_OKAY, rdata: rg_inst, ruser: ?}; //TODO user?
    s_tb_xactor.i_rd_data.enq(lv_resp);
  endrule


  interface slave = s_tb_xactor.axi_side;
  interface fetch_master =  m_tb_xactor.axi_side;

endmodule

module mkTb(Empty);
  //In this module, instantiate the AXI Fabric - connect the tb with the fetch decode and test  
endmodule

endpackage
