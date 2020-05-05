/* 
Author: Vinod Ganesan
Email id: g.vinod1993@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package depResolver;
import frontend_common::*;
import GetPut::*;
import Connectable::*;
import FIFOF::*;


`define Pop_next_dep  4'b0001
`define Pop_prev_dep  4'b0010
`define Push_next_dep 4'b0100
`define Push_prev_dep 4'b1000


interface Ifc_depResolver;
 //Inputs
 interface Put#(Bit#(1)) gemmtoLoadpush;
 interface Put#(Bit#(1)) loadtoGemmpush;
 interface Put#(Bit#(1)) gemmtoStorepush;
 interface Put#(Bit#(1)) storetoGemmpush;

 //method Action instrFromfetch(Bit#(ILEN) instruction);
 interface Put#(Bit#(ILEN)) fromloadDep;
 interface Put#(Bit#(ILEN)) fromstoreDep;
 interface Put#(Bit#(ILEN)) fromcomputeDep;

 interface Get#(Bit#(ILEN)) toloadModule;
 interface Get#(Bit#(ILEN)) tostoreModule; 
 interface Get#(Bit#(ILEN)) tocomputeModule;  

endinterface 

interface Ifc_tb_slave;

  interface Get#(Bit#(1)) sendgemmtoLoad;
  interface Get#(Bit#(1)) sendloadtoGemm;
  interface Get#(Bit#(1)) sendgemmtoStore;
  interface Get#(Bit#(1)) sendstoretoGemm;

  interface Get#(Bit#(ILEN)) toloadDep;
  interface Get#(Bit#(ILEN)) tostoreDep;
  interface Get#(Bit#(ILEN)) tocomputeDep;

  interface Put#(Bit#(ILEN)) fromloadModule;
  interface Put#(Bit#(ILEN)) fromstoreModule;
  interface Put#(Bit#(ILEN)) fromcomputeModule;

endinterface 


module mkdepResolver(Ifc_depResolver);

  FIFOF#(Bit#(ILEN)) tloadQ     <- mkSizedFIFOF(valueOf(TloadQdepth));
  FIFOF#(Bit#(ILEN)) tstoreQ    <- mkSizedFIFOF(valueOf(TstoreQdepth));
  FIFOF#(Bit#(ILEN)) tcomputeQ  <- mkSizedFIFOF(valueOf(TcomputeQdepth));

  FIFOF#(Bit#(ILEN)) loadQ      <- mkSizedFIFOF(valueOf(LoadQdepth));
  FIFOF#(Bit#(ILEN)) storeQ     <- mkSizedFIFOF(valueOf(StoreQdepth));
  FIFOF#(Bit#(ILEN)) computeQ   <- mkSizedFIFOF(valueOf(ComputeQdepth));

  FIFOF#(Bit#(1)) gemmtoloadQ   <- mkSizedFIFOF(valueOf(G2lQdepth));
  FIFOF#(Bit#(1)) gemmtostoreQ  <- mkSizedFIFOF(valueOf(G2sQdepth));
  FIFOF#(Bit#(1)) loadtogemmQ   <- mkSizedFIFOF(valueOf(L2gQdepth));
  FIFOF#(Bit#(1)) storetogemmQ  <- mkSizedFIFOF(valueOf(S2gQdepth));

 let iLEN = valueOf(ILEN);
 let opCode = valueOf(Opcode);
 let dePT   = valueOf(Dept);


  rule rl_schedload;
    let load_inst = tloadQ.first;
    let deptFlags = load_inst[iLEN-opCode-1:iLEN-opCode-dePT];  //deptFlags
   
    if((deptFlags == `Pop_next_dep && gemmtoloadQ.notEmpty()) || deptFlags != `Pop_next_dep) begin
      tloadQ.deq; 
      loadQ.enq(load_inst);
      if(deptFlags == `Pop_next_dep)
        gemmtoloadQ.deq;
    end
  endrule

  rule rl_schedstore; 
    let store_inst = tstoreQ.first;
    let deptFlags  = store_inst[iLEN-opCode-1:iLEN-opCode-dePT];

    if((deptFlags == `Pop_prev_dep && gemmtostoreQ.notEmpty()) || deptFlags != `Pop_prev_dep) begin
      tstoreQ.deq;
      storeQ.enq(store_inst);

      if(deptFlags == `Pop_prev_dep)
        gemmtostoreQ.deq;
    end
  endrule 

  rule rl_schedcompute;
    let compute_inst = tcomputeQ.first;
    let deptFlags    = compute_inst[iLEN-opCode-1:iLEN-opCode-dePT];

    if(((deptFlags == `Pop_prev_dep && loadtogemmQ.notEmpty()) || deptFlags != `Pop_prev_dep) &&
        ((deptFlags == `Pop_next_dep && storetogemmQ.notEmpty()) || deptFlags != `Pop_next_dep)) begin
        tcomputeQ.deq;
        computeQ.enq(compute_inst);

        if(deptFlags == `Pop_prev_dep)
          loadtogemmQ.deq;

        if(deptFlags == `Pop_next_dep)
          storetogemmQ.deq;
    end
  endrule

  //Being explicit with Get-Put interfaces here so that it's easy to read and understand 

  interface Get toloadModule;
    method ActionValue#(Bit#(ILEN)) get;
      let load_inst = loadQ.first;
      loadQ.deq;
      return load_inst;
    endmethod
  endinterface 

  interface Get tostoreModule;
    method ActionValue#(Bit#(ILEN)) get;
      let store_inst = storeQ.first;
      storeQ.deq;
      return store_inst;
    endmethod
  endinterface 

  interface Get tocomputeModule;
    method ActionValue#(Bit#(ILEN)) get;
      let compute_inst = computeQ.first;
      computeQ.deq;
      return compute_inst;
    endmethod 
  endinterface 


  interface Put fromloadDep;
    method Action put(Bit#(ILEN) loadinst);
      tloadQ.enq(loadinst);
    endmethod
  endinterface 

  interface Put fromstoreDep;
    method Action put(Bit#(ILEN) storeinst);
      tstoreQ.enq(storeinst);
    endmethod
  endinterface 

  interface Put fromcomputeDep;
    method Action put(Bit#(ILEN) computeinst);
      tcomputeQ.enq(computeinst);
    endmethod
  endinterface 
      
  interface Put gemmtoLoadpush;
    method Action put(Bit#(1) token);
      gemmtoloadQ.enq(token);
    endmethod
  endinterface
  
  interface Put gemmtoStorepush;
    method Action put(Bit#(1) token);
      gemmtostoreQ.enq(token);
    endmethod
  endinterface
  
  interface Put loadtoGemmpush;
    method Action put(Bit#(1) token);
      loadtogemmQ.enq(token);
    endmethod
  endinterface 
  
  interface Put storetoGemmpush;
    method Action put(Bit#(1) token);
      storetogemmQ.enq(token);
    endmethod
  endinterface 

endmodule

module mkdepResolverSlave(Ifc_tb_slave);

   Wire#(Bit#(ILEN)) wr_loadinst  <- mkWire();
   Wire#(Bit#(ILEN)) wr_storeinst <- mkWire();
   Wire#(Bit#(ILEN)) wr_computeinst <- mkWire();
   Wire#(Bit#(1)) wr_gemmtoload_token <- mkWire();
   Wire#(Bit#(1)) wr_gemmtostore_token <- mkWire();
   Wire#(Bit#(1)) wr_loadtogemm_token <- mkWire();
   Wire#(Bit#(1)) wr_storetogemm_token <- mkWire();

   Reg#(Bit#(4)) rg_test_counter <- mkReg(0);

  //Placeholders
  rule test_load_nodep(rg_test_counter == 0);
    wr_loadinst <= {4'b0000, 4'b0000, '1};
    rg_test_counter <= 1;
  endrule

  rule test_load_dep(rg_test_counter == 1);
    wr_gemmtoload_token <= 1;
    wr_loadinst <= {4'b0000, `Pop_next_dep, '1};  
    rg_test_counter <= 2;
  endrule

  rule test_store_nodep(rg_test_counter == 3);
  endrule

  rule test_store_dep(rg_test_counter == 4);
  endrule

  rule test_compute_nodep(rg_test_counter == 5);
  endrule

  rule test_compute_dep1(rg_test_counter == 6);
  endrule

  rule test_compute_dep2(rg_test_counter == 7);
  endrule

  interface Put fromloadModule;
    method Action put(Bit#(ILEN) ins);
      $display(" Success - Received a Load Instruction " ); 
    endmethod
  endinterface 

  interface Put fromstoreModule;
    method Action put(Bit#(ILEN) ins);
      $display(" Success - Received a Store Instruction ");
    endmethod
  endinterface 

  interface Put fromcomputeModule;
    method Action put(Bit#(ILEN) ins);
      $display(" Success - Received a Compute Instruction ");
    endmethod 
  endinterface 

  interface Get toloadDep;
    method ActionValue#(Bit#(ILEN)) get;
      return wr_loadinst; 
    endmethod
  endinterface 

  interface Get tostoreDep;
    method ActionValue#(Bit#(ILEN)) get;
      return wr_storeinst;
    endmethod
  endinterface 

  interface Get tocomputeDep;
    method ActionValue#(Bit#(ILEN)) get;
      return wr_computeinst;
    endmethod
  endinterface 
      
  interface Get sendgemmtoLoad;
    method ActionValue#(Bit#(1)) get;
      return wr_gemmtoload_token;
    endmethod
  endinterface
  
  interface Get sendgemmtoStore;
    method ActionValue#(Bit#(1)) get;
      return wr_gemmtostore_token;
    endmethod
  endinterface
  
  interface Get sendloadtoGemm;
    method ActionValue#(Bit#(1)) get;
      return wr_loadtogemm_token;
    endmethod
  endinterface 
  
  interface Get sendstoretoGemm;
    method ActionValue#(Bit#(1)) get;
      return wr_storetogemm_token;
    endmethod
  endinterface 

endmodule

module mkTb(Empty);

 Ifc_depResolver depResolver <- mkdepResolver();
 Ifc_tb_slave    depResolverslave <- mkdepResolverSlave();

 mkConnection(depResolver.loadtoGemmpush,  depResolverslave.sendloadtoGemm);
 mkConnection(depResolver.gemmtoLoadpush,  depResolverslave.sendgemmtoLoad);
 mkConnection(depResolver.storetoGemmpush, depResolverslave.sendstoretoGemm);
 mkConnection(depResolver.gemmtoStorepush, depResolverslave.sendgemmtoStore);

 mkConnection(depResolver.fromloadDep,    depResolverslave.toloadDep);
 mkConnection(depResolver.fromstoreDep,   depResolverslave.tostoreDep);
 mkConnection(depResolver.fromcomputeDep, depResolverslave.tocomputeDep);

 mkConnection(depResolver.toloadModule,    depResolverslave.fromloadModule);
 mkConnection(depResolver.tostoreModule,   depResolverslave.fromstoreModule);
 mkConnection(depResolver.tocomputeModule, depResolverslave.fromcomputeModule);

endmodule



endpackage
