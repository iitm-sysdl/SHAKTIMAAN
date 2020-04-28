/*
Copyright (c) 2018, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of
  conditions and the following disclaimer in the documentation and/or other materials provided
 with the distribution.
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Author: Neel Gala
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package bram;
  import BRAMCore :: *;
	import DReg::*;
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import AXI4_Lite_Types   :: *;
	import AXI4_Lite_Fabric  :: *;
	import BUtils::*;
  import GetPut::*;
	import device_common::*;
  import Assert::*;

  `include "Logger.bsv"
  export Ifc_bram_axi4 (..);
  export Ifc_bram_axi4lite (..);
  export mkbram_axi4;
  export mkbram_axi4lite;

  interface UserInterface#(numeric type addr_width,  numeric type data_width, numeric type index_size);
    method Action read_request (Bit#(addr_width) addr);
    method Action write_request (Tuple3#(Bit#(addr_width), Bit#(data_width),
                                                                  Bit#(TDiv#(data_width, 8))) req);
    method ActionValue#(Tuple2#(Bool, Bit#(data_width))) read_response;
    method ActionValue#(Bool) write_response;
  endinterface

  // to make is synthesizable replace addr_width with Physical Address width
  // data_width with data lane width
  module mkbram#(parameter Integer slave_base, parameter String readfile,
                                                parameter String modulename )
      (UserInterface#(addr_width, data_width, index_size))
      provisos(
        Mul#(TDiv#(data_width, TDiv#(data_width, 8)), TDiv#(data_width, 8),data_width)  );

    Integer byte_offset = valueOf(TLog#(TDiv#(data_width, 8)));
  	// we create 2 32-bit BRAMs since the xilinx tool is easily able to map them to BRAM32BE cells
  	// which makes it easy to use data2mem for updating the bit file.

    BRAM_DUAL_PORT_BE#(Bit#(TSub#(index_size, TLog#(TDiv#(data_width, 8)))), Bit#(data_width),
                                                                    TDiv#(data_width,8)) dmemMSB <-
          mkBRAMCore2BELoad(valueOf(TExp#(TSub#(index_size, TLog#(TDiv#(data_width, 8))))), False,
                            readfile, False);

    Reg#(Bool) read_request_sent[2] <-mkCRegA(2,False);

    // A write request to memory. Single cycle operation.
    // This model assumes that the master sends the data strb aligned for the data_width bytes.
    // Eg. : is size is HWord at address 0x2 then the wstrb for 64-bit data_width is: 'b00001100
    // And the data on the write channel is assumed to be duplicated.
    method Action write_request (Tuple3#(Bit#(addr_width), Bit#(data_width),
                                                                  Bit#(TDiv#(data_width, 8))) req);
      let {addr, data, strb}=req;
			Bit#(TSub#(index_size,TLog#(TDiv#(data_width, 8)))) index_address=
			                          (addr - fromInteger(slave_base))[valueOf(index_size)-1:byte_offset];
			dmemMSB.b.put(strb,index_address,truncateLSB(data));
      `logLevel( bram, 0, $format("",modulename,": Recieved Write Request for Address: %h Index: %h\
 Data: %h wrstrb: %h", addr, index_address, data, strb))
  	endmethod

    // The write response will always be an error.
    method ActionValue#(Bool) write_response;
      return False;
    endmethod

    // capture a read_request and latch the address on a BRAM.
    method Action read_request (Bit#(addr_width) addr);
			Bit#(TSub#(index_size,TLog#(TDiv#(data_width, 8)))) index_address=
			                          (addr - fromInteger(slave_base))[valueOf(index_size)-1:byte_offset];
      dmemMSB.a.put(0, index_address, ?);
      read_request_sent[1]<= True;
      `logLevel( bram, 0, $format("",modulename,": Recieved Read Request for Address: %h Index: %h",
                                                                            addr, index_address))
  	endmethod

    // respond with data from the BRAM.
    method ActionValue#(Tuple2#(Bool, Bit#(data_width))) read_response if(read_request_sent[0]);
      read_request_sent[0]<=False;
      return tuple2(False, dmemMSB.a.read());
    endmethod
  endmodule

  interface Ifc_bram_axi4#(numeric type addr_width, numeric type data_width, numeric type user_width,
                                                                           numeric type index_size);
    interface AXI4_Slave_IFC#(addr_width, data_width, user_width) slave;
  endinterface

  typedef enum {Idle, Burst} Mem_State deriving(Eq, Bits, FShow);

  module mkbram_axi4#( parameter Integer slave_base, parameter String readfile,
        parameter String modulename )
        (Ifc_bram_axi4#(addr_width, data_width, user_width, index_size))
      provisos(
        Mul#(TDiv#(data_width, TDiv#(data_width, 8)), TDiv#(data_width, 8),data_width)  );
    UserInterface#(addr_width, data_width, index_size) dut <- mkbram(slave_base, readfile,
        modulename);
	  AXI4_Slave_Xactor_IFC #(addr_width, data_width, user_width)  s_xactor <- mkAXI4_Slave_Xactor;
    Reg#(Bit#(4)) rg_rd_id <-mkRegA(0);
    Reg#(Mem_State) read_state <-mkRegA(Idle);
    Reg#(Mem_State) write_state <-mkRegA(Idle);
	  Reg#(Bit#(8)) rg_readburst_counter<-mkRegA(0);
	  Reg#(AXI4_Rd_Addr	#(addr_width, user_width)) rg_read_packet <-mkRegA(?);
		Reg#(AXI4_Wr_Addr	#(addr_width, user_width)) rg_write_packet<-mkRegA(?);
    Wire#(Bool) wr_read_ack <- mkWire();

    // If the request is single then simple send ERR. If it is a burst write request then change
    // state to Burst and do not send response.
    rule write_request_address_channel(write_state==Idle);
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
      dut.write_request(tuple3(aw.awaddr, w.wdata, w.wstrb));
	    let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
      if(!w.wlast)
        write_state<= Burst;
      else
  	  	s_xactor.i_wr_resp.enq (b);
			rg_write_packet<=aw;
    endrule
    // if the request is a write burst then keeping popping all the data on the data_channel and
    // send a error response on receiving the last data.
    rule write_request_data_channel(write_state==Burst);
      let w  <- pop_o (s_xactor.o_wr_data);
  		let address=axi4burst_addrgen(rg_write_packet.awlen, rg_write_packet.awsize,
            rg_write_packet.awburst, rg_write_packet.awaddr);
      dut.write_request(tuple3(address, w.wdata, w.wstrb));
	    let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: rg_write_packet.awuser,
                          bid:rg_write_packet.awid};
      rg_write_packet.awaddr<=address;
      if(w.wlast) begin
	  	  s_xactor.i_wr_resp.enq (b);
        write_state<= Idle;
      end
    endrule
    // read first request and send it to the dut. If it is a burst request then change state to
    // Burst. capture the request type and keep track of counter.
    rule read_request_first(read_state==Idle);
		  let ar<- pop_o(s_xactor.o_rd_addr);
      dut.read_request(ar.araddr);
      rg_rd_id<= ar.arid;
      if(ar.arlen!=0)
        read_state<=Burst;
      rg_readburst_counter<=0;
		  rg_read_packet<=ar;
    endrule
    // incase of burst read,  generate the new address and send it to the dut untill the burst
    // count has been reached.
    rule read_request_burst(read_state==Burst && wr_read_ack);
      if(rg_readburst_counter==rg_read_packet.arlen)
        read_state<=Idle;
      else begin
  		  let address=axi4burst_addrgen(rg_read_packet.arlen, rg_read_packet.arsize,
            rg_read_packet.arburst, rg_read_packet.araddr);
        rg_read_packet.araddr<=address;
        rg_readburst_counter<= rg_readburst_counter+1;
        dut.read_request(address);
      end
    endrule
    // get data from the memory. shift,  truncate, duplicate based on the size and offset.
    rule read_response;
      wr_read_ack<=True;
      let {err, data0}<-dut.read_response;
      AXI4_Rd_Data#(data_width, user_width) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 ,
        rlast:rg_readburst_counter==rg_read_packet.arlen, ruser: 0, rid:rg_read_packet.arid};
      `logLevel( bram, 1, $format("",modulename,": Responding Read Request with Data: %h ",data0))
      s_xactor.i_rd_data.enq(r);
    endrule
    interface slave = s_xactor.axi_side;
  endmodule

  interface Ifc_bram_axi4lite#(numeric type addr_width, numeric type data_width, numeric type user_width,
                                                                           numeric type index_size);
    interface AXI4_Lite_Slave_IFC#(addr_width, data_width, user_width) slave;
  endinterface


  module mkbram_axi4lite#(parameter Integer slave_base, parameter String readfile,
         parameter String modulename )
        (Ifc_bram_axi4lite#(addr_width, data_width, user_width, index_size))
      provisos(
        Mul#(TDiv#(data_width, TDiv#(data_width, 8)), TDiv#(data_width, 8),data_width)  );
    UserInterface#(addr_width, data_width, index_size) dut <- mkbram(slave_base, readfile,
                                                                                        modulename);
	  AXI4_Lite_Slave_Xactor_IFC #(addr_width, data_width, user_width)  s_xactor <- mkAXI4_Lite_Slave_Xactor;
    Integer byte_offset = valueOf(TDiv#(data_width, 32));
    Reg#(Bit#(2)) rg_size <-mkRegA(3);
    Reg#(Bit#(TAdd#(1, TDiv#(data_width, 32)))) rg_offset <-mkRegA(0);
    // If the request is single then simple send ERR. If it is a burst write request then change
    // state to Burst and do not send response.
    rule write_request_address_channel;
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
	    let b = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: aw.awuser};
      dut.write_request(tuple3(aw.awaddr, w.wdata, w.wstrb));
	  	s_xactor.i_wr_resp.enq (b);
    endrule
    // read first request and send it to the dut. If it is a burst request then change state to
    // Burst. capture the request type and keep track of counter.
    rule read_request_first;
		  let ar<- pop_o(s_xactor.o_rd_addr);
      dut.read_request(ar.araddr);
      rg_size<= ar.arsize;
      rg_offset<= ar.araddr[byte_offset:0];
    endrule
    // get data from the memory. shift,  truncate, duplicate based on the size and offset.
    rule read_response;
      let {err, data0}<-dut.read_response;
      AXI4_Lite_Rd_Data#(data_width, user_width) r = AXI4_Lite_Rd_Data {rresp: AXI4_LITE_OKAY, rdata: data0 ,
        ruser: 0};
      `logLevel( bram, 1, $format("",modulename,": Responding Read Request with Data: %h ",data0))
      s_xactor.i_rd_data.enq(r);
    endrule
    interface slave = s_xactor.axi_side;
  endmodule

//  interface Ifc_bram_TLU#(numeric type a, numeric type w, numeric type z, numeric type index_size);
//    interface Ifc_fabric_side_slave_link_lite#(a, w, z) read_slave;
//    interface Ifc_fabric_side_slave_link_lite#(a, w, z) write_slave;
//  endinterface
//
//  module mkmemory_TLU#(Bit#(a) base, parameter String msb_file, parameter String
//                                                lsb_file)(Ifc_bram_TLU#(a, w, z, index_size))
//    provisos(Mul#(w, 8, data_width),
//             Add#(data_width, e, 64),
//             Mul#(8, a__,  data_width),
//             Mul#(16, b__, data_width),
//             Mul#(32, c__, data_width),
//             Div#(data_width, 8, w),
//             Add#(32, e__, data_width), // required by BSV
//             Add#(4, f__, TDiv#(data_width, 8)),  // required by BSV
//             Add#(d__, 2, z)); // to ensure that we are only operating upto 64 bits
//
//    UserInterface#(a, data_width, index_size) dut <- mkmemory(base, msb_file, lsb_file);
//    Ifc_Slave_link_lite#(a, w, z)  write_xactor <- mkSlaveXactorLite(True, True);
//    Ifc_Slave_link_lite#(a, w, z)  read_xactor <- mkSlaveXactorLite(True, True);
//
//    Integer verbosity = `VERBOSITY;
//    Integer byte_offset = valueOf(TDiv#(data_width, 32));
//    Reg#(Bit#(z)) rg_size <-mkRegA(3);
//    Reg#(Bit#(2)) rg_source <- mkRegA(0);
//    Reg#(Bit#(TAdd#(1, TDiv#(data_width, 32)))) rg_offset <-mkRegA(0);
//    // If the request is single then simple send ERR. If it is a burst write request then change
//    // state to Burst and do not send response.
//    rule write_request_address_channel;
//      let req<- write_xactor.core_side.xactor_request.get;
//      dut.write_request(tuple3(req.a_address, req.a_data, req.a_mask));
//      let lv_resp = D_channel_lite{d_opcode: AccessAck, d_size: ?, d_source: req.a_source,
//                                                      d_sink: ?, d_data: ?, d_error: False};
//	  	write_xactor.core_side.xactor_response.put(lv_resp);
//    endrule
//    // read first request and send it to the dut. If it is a burst request then change state to
//    // Burst. capture the request type and keep track of counter.
//    rule read_request_first;
//      let req <- read_xactor.core_side.xactor_request.get;
//      dut.read_request(req.a_address);
//      rg_size<= req.a_size;
//      rg_offset<= req.a_address[byte_offset:0];
//      rg_source<= req.a_source;
//    endrule
//    // get data from the memory. shift,  truncate, duplicate based on the size and offset.
//    rule read_response;
//      let {err, data0}<-dut.read_response;
//  		let transfer_size=rg_size;
//      let shift_amount = {3'b0, rg_offset}<<3;
//      data0=data0>>shift_amount;
//      if(transfer_size=='d2)
//        data0=duplicate(data0[31:0]);
//      else if(transfer_size=='d1)
//        data0=duplicate(data0[15:0]);
//      else if(transfer_size=='d0)
//        data0=duplicate(data0[7:0]);
//
//      D_channel_lite#(w, z) lv_resp=D_channel_lite { d_opcode : AccessAckData, d_size : rg_size,
//            d_source : rg_source, d_sink : ?, d_data : data0, d_error : False};
//  		if(verbosity!=0)
//        `logLevel( bram, 0, $format("",modulename,": Responding Read Request with Data: %h ", data0);
//	  	read_xactor.core_side.xactor_response.put(lv_resp);
//    endrule
//    interface read_slave = read_xactor.fabric_side;
//    interface write_slave = write_xactor.fabric_side;
//  endmodule
endpackage
