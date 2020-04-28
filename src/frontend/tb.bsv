package tb;

import fetchDecode::*;
import depResolver::*;
import  GetPut::*;
import  Connectable::*;
import Semi_FIFOF::*;
import  AXI4_Types  ::*;
import  AXI4_Fabric ::*;
import  FIFOF::*;
import  frontend_common ::*;


interface Ifc_token;
  interface Get#(Bit#(1)) sendgemmtoLoad;
endinterface

module mkToken(Ifc_token);

  Wire#(Bit#(1)) wr_gemmtoload_token <- mkWire();

  rule token;
    wr_gemmtoload_token <= 1;
  endrule

  interface Get sendgemmtoLoad;
    method ActionValue#(Bit#(1)) get;
      return wr_gemmtoload_token;
    endmethod
  endinterface

endmodule

module mkTB(Empty);
  //In this module, instantiate the AXI Fabric - connect the tb with the fetch decode and test
  Reg#(Bit#(20)) rg_clock_val <- mkReg(0);

  //instantiating modules of fetchDecode
  Ifc_fetchDecode fetchDecode <- mkfetchDecode;
  Ifc_slave dram <- mkslave;

  //instantiating modules of depResover
  Ifc_depResolver depResolver <- mkdepResolver();
  Ifc_tb_slave    depResolverslave <- mkdepResolverSlave();
  Ifc_token       dummyToken <- mkToken();


  

  Reg#(Bit#(20)) rg_max_clock <- mkReg(200);

  //connect Frontend master and dram slave
  mkConnection(fetchDecode.master, dram.slave);
  //connect dram master and frontend slave
  mkConnection(dram.fetch_master, fetchDecode.fetch_slave);

  //pass the fetched inst to depResolver
  mkConnection(depResolver.fromloadDep,    fetchDecode.toloadQdep);
  mkConnection(depResolver.fromstoreDep,   fetchDecode.tostoreQdep);
  mkConnection(depResolver.fromcomputeDep, fetchDecode.tocomputeQdep);

  //watch any scheduled inst
  mkConnection(depResolver.toloadModule,    depResolverslave.fromloadModule);
  mkConnection(depResolver.tostoreModule,   depResolverslave.fromstoreModule);
  mkConnection(depResolver.tocomputeModule, depResolverslave.fromcomputeModule);


  mkConnection(depResolver.gemmtoLoadpush,  dummyToken.sendgemmtoLoad);


  rule rl_clock(rg_clock_val < rg_max_clock);
    rg_clock_val <= rg_clock_val + 1 ;
  endrule

  rule rl_finish(rg_clock_val == rg_max_clock);
    $finish;
  endrule

  rule rl_print_50_cyc(rg_clock_val%50==0);
    //$display("fetch decode TB getting triggered!");
  endrule

endmodule

endpackage