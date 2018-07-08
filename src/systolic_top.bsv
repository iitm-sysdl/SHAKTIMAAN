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

Author: Vinod Ganesan
Email id: g.vinod1993@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package systolic_top;
  import  AXI4_Types  ::*;
  import  AXI4_Fabric ::*;
  import  Connectable ::*;
  import  systolic ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;
  import BRAMCore :: *;
	import BUtils::*;
  import Semi_FIFOF::*;
	`include "defined_parameters.bsv"
	import defined_types::*;
  `include "systolic.defs"
	import axi_addr_generator::*;

  interface Ifc_systolic_top#(numeric type nRow, numeric type nCol, 
                              numeric type addr, numeric type data,
                              numeric type accumaddr,numeric type gbufaddr,  
                              numeric type nFEntries,numeric type mulWidth);
    interface AXI4_Slave_IFC#(`PADDR,data,0) slave_systolic; //32-bit address? 4 16-bits?
  endinterface
	
  typedef enum{Idle,HandleBurst} Mem_state deriving(Bits,Eq);

    (*synthesize*)
    module mksystolic3(Ifc_systolic_top#(3,3,32,32,32,16,2,16));
        let ifc();
        mksystolic_top inst(ifc);
        return (ifc);
    endmodule

  module mksystolic_top(Ifc_systolic_top#(nRow,nCol,addr,data,gbufaddr,accumaddr,nFEntries,mulWidth))
    provisos(
             Add#(a__,2,nRow),
             Add#(b__,2,nCol),
             Add#(d__,16,mulWidth),  //Change every 16 with MulWidth
             Add#(mulWidth,2,mulWidth2),
             Add#(c__, accumaddr, 32),
             Add#(e__, gbufaddr, 32),
             Add#(g__, 16, data),
             Add#(f__, 32, data)  //This is conflicting with above
            );
    let vnFEntries = valueOf(nFEntries);
    let vnRow = valueOf(nRow);
    let vnCol = valueOf(nCol);
    let gBUF  = valueOf(TExp#(gbufaddr));
    let aCC   = valueOf(TExp#(accumaddr));

    Ifc_systolic#(nRow,nCol,mulWidth)                systolic_array <- mksystolic;  
    Vector#(nRow, FIFOF#(Bit#(16)))                  rowBuf    <- replicateM(mkSizedFIFOF(vnFEntries));
    Vector#(nCol, FIFOF#(Tuple3#(Bit#(16),Bit#(8),Bit#(2)))) colBuf    <- replicateM(mkSizedFIFOF(vnFEntries));
    //The Co-ord Value Bit#(8) is temporarily put here
    BRAM_DUAL_PORT_BE#(Bit#(gbufaddr), Bit#(16), 2)  gBuffer   <- mkBRAMCore2BE(gBUF,False);
    BRAM_DUAL_PORT_BE#(Bit#(accumaddr), Bit#(32), 4) accumBuf  <- mkBRAMCore2BE(aCC,False);
    AXI4_Slave_Xactor_IFC#(`PADDR,data,0)              s_xactor  <- mkAXI4_Slave_Xactor;
    Reg#(Mem_state) rg_wr_state <- mkReg(Idle);
    Reg#(Mem_state) rg_rd_state <- mkReg(Idle);
		Reg#(AXI4_Wr_Addr#(`PADDR,0)) rg_write_packet<-mkReg(?);
		Reg#(AXI4_Rd_Addr#(`PADDR,0)) rg_read_packet <-mkReg(?);

    
    //Should there be a Master interface with bigger busWidth?
    //Assuming a Memory Map for 10 Configuration Registers --- 0 to 'h24
    //Assuming a Memory Map for 10KB from 'h28 to 

    Reg#(Bit#(9))   accumIndex           <- mkReg(0);
    Reg#(Bit#(8))   rg_readburst_counter <- mkReg(0);
    Reg#(Bool)      gACheck              <- mkReg(False);

    /* =========================== Function Definitions ============================== */
     // function Action populate_buffers(Bit#(addr) lv_addr, Bit#(data) lv_data, Bit#(TDiv#(data,8))
     //     wstrb);
     //     let accindex  = lv_addr - fromInteger(`AccumBufStart);
     //     let gbufindex = lv_addr - fromInteger(`GBufStart);

     //     //Write Request to AccumBuffer Range
     //     if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
     //       accumBuf.b.put(wstrb[3:0],accindex,truncate(lv_data));
     //     end
     //     else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
     //       gBuffer.b.put(wstrb[1:0], gbufindex, truncate(lv_data));
     //     end

     //     //Logic to write into Configuration Address Space
     // endfunction

      //function Action read_req(Bit#(addr) lv_addr);
      //  if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin 
      //    accumBuf.a.put(0,lv_addr - fromInteger(`AccumBufStart) , ?);
      //    gACheck <= True;
      //  end
      //  else begin
      //    gBuffer.b.put(0,lv_addr - fromInteger(`GBufStart),?);
      //    gACheck <= False;
      //  end
      //endfunction


     /* =================== Rules to Connect Row Buffers to Arrays ================== */
      for(Integer i = 0; i < vnRow; i=i+1) begin
        rule send_row_buf_value;
          Maybe#(Bit#(16)) mval = tagged Valid rowBuf[i].first;
          rowBuf[i].deq;
          systolic_array.rfifo[i].send_rowbuf_value(mval);
        endrule
      end
    /* ============================================================================== */

    /* ==================== Rules to Connect Col Buffers to Arrays  ================= */
      for(Integer i = 0; i < vnCol; i=i+1) begin
        rule send_col_buf_value;
          let val = colBuf[i].first;
          let val1 = tagged Valid tpl_1(val);
          let finVal = tuple3(val1,tpl_2(val),tpl_3(val)); //This is very crude! 
          colBuf[i].deq;
          systolic_array.cfifo[i].send_colbuf_value(finVal);
        endrule
      end
    /* ============================================================================== */

    /* ===================== Logic to populate the Buffers ========================== */

    rule rlAXIwrReq(rg_wr_state == Idle);
     let aw <- pop_o(s_xactor.o_wr_addr);
     let w  <- pop_o(s_xactor.o_wr_data);
     //populate_buffers(aw.awaddr,w.wdata,w.wstrb);
     let lv_addr = aw.awaddr;
     let lv_data = w.wdata;
     let wstrb = w.wstrb;
     Bit#(accumaddr) accindex       = truncate(lv_addr - fromInteger(`AccumBufStart));
     Bit#(gbufaddr) gbufindex      = truncate(lv_addr - fromInteger(`GBufStart));

          //Write Request to AccumBuffer Range
          if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
            accumBuf.b.put(wstrb[3:0],accindex,truncate(lv_data));
          end
          else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
            gBuffer.b.put(wstrb[1:0], gbufindex, truncate(lv_data));
          end


     let resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
     if(aw.awlen != 0) begin 
       rg_wr_state <= HandleBurst;
		   let new_address=burst_address_generator(aw.awlen,aw.awsize,aw.awburst,aw.awaddr);
       aw.awaddr = new_address;
       rg_write_packet <= aw;
     end
     else begin
		  	s_xactor.i_wr_resp.enq (resp);
     end
    endrule

    rule rlAXIwrReqBurst(rg_wr_state == HandleBurst);
      let w <- pop_o(s_xactor.o_wr_data);
      let resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: rg_write_packet.awuser, bid:rg_write_packet.awid};
      //populate_buffers(rg_write_packet.awaddr, w.data,w.wstrb);

      let lv_addr = rg_write_packet.awaddr;
      let lv_data = w.wdata;
      let wstrb = w.wstrb;
      Bit#(accumaddr) accindex  = truncate(lv_addr - fromInteger(`AccumBufStart));
      Bit#(gbufaddr)  gbufindex = truncate(lv_addr - fromInteger(`GBufStart));

          //Write Request to AccumBuffer Range
          if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
            accumBuf.b.put(wstrb[3:0],accindex,truncate(lv_data));
          end
          else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
            gBuffer.b.put(wstrb[1:0], gbufindex, truncate(lv_data));
          end

      
      let new_address=burst_address_generator(rg_write_packet.awlen,rg_write_packet.awsize,
                                              rg_write_packet.awburst,rg_write_packet.awaddr);
      rg_write_packet.awaddr<=new_address;
      if(w.wlast) begin
        rg_wr_state <= Idle;
        s_xactor.i_wr_resp.enq(resp);
      end
    endrule

    /* ============================================================================= */

    /* =================== Logic to read the Values of Buffers ===================== */

    rule rlAXIreadReq(rg_rd_state == Idle);
			let ar <- pop_o(s_xactor.o_rd_addr);
      rg_read_packet <= ar;
      //read_req(ar.araddr);
      let lv_addr = ar.araddr;
      Bit#(gbufaddr) gIndex = truncate(lv_addr - fromInteger(`GBufStart));
      Bit#(accumaddr) accIndex = truncate(lv_addr - fromInteger(`AccumBufStart));
      
      if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin 
          accumBuf.a.put(0,accIndex , ?);
          gACheck <= True;
        end
        else begin
          gBuffer.b.put(0,gIndex,?);
          gACheck <= False;
        end
      rg_rd_state <= HandleBurst;
    endrule

    rule rlAXIreadBurst(rg_rd_state == HandleBurst);
      if(gACheck) begin  //Could be Potentially a buggy code
       let accumVal = accumBuf.a.read();
       let rA = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: extend(accumVal) ,rlast:
                             rg_readburst_counter==rg_read_packet.arlen, ruser: 0, 
                             rid:rg_read_packet.arid};
	    s_xactor.i_rd_data.enq(rA);
      end
      else begin
       let gVal = gBuffer.a.read();
       let rG = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: extend(gVal) ,rlast:
                              rg_readburst_counter==rg_read_packet.arlen, ruser: 0, 
                              rid:rg_read_packet.arid};
      s_xactor.i_rd_data.enq(rG);
      end 
		  let new_address=burst_address_generator(rg_read_packet.arlen,rg_read_packet.arsize,
                                              rg_read_packet.arburst,rg_read_packet.araddr);

      if(rg_readburst_counter==rg_read_packet.arlen)begin
				rg_readburst_counter<=0;
				rg_rd_state<=Idle;
			end
      else begin
      //read_req(new_address);
      let lv_addr = new_address;
      Bit#(gbufaddr) gIndex = truncate(lv_addr - fromInteger(`GBufStart));
      Bit#(accumaddr) accIndex = truncate(lv_addr - fromInteger(`AccumBufStart));
      if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin 
          accumBuf.a.put(0,accIndex, ?);
          gACheck <= True;
        end
        else begin
          gBuffer.b.put(0,gIndex,?);
          gACheck <= False;
        end
      end
       //Not Considering Transfer Size for Now
    endrule

    /* ============================================================================== */

  interface slave_systolic =  s_xactor.axi_side;
  endmodule


endpackage



