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
  import  systolic  ::*;
  import  ConcatReg ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;
  import BRAMCore :: *;
	import BUtils::*;
  import Semi_FIFOF::*;
  import UniqueWrappers::*;


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
             Log#(nRow,gbufbankaddress),
             Log#(nCol,accumbankaddress),
             Add#(gindexaddr,TAdd#(gbufbankaddress,2),gbufaddr),
             Add#(accumindexaddr, TAdd#(accumbankaddress,2), accumaddr),
             Add#(g__, 16, data),
             Add#(f__, 32, data)  //This is conflicting with above
            );
    let vnFEntries = valueOf(nFEntries);
    let vnRow      = valueOf(nRow);
    let vnCol      = valueOf(nCol);
    let gBUF       = valueOf(TExp#(gbufaddr));
    let aCC        = valueOf(TExp#(accumaddr));
    let gbufBank   = valueOf(gbufbankaddress);
    let bufAccum   = valueOf(accumbankaddress);
    let gIndex     = valueOf(gindexaddr);
    let accIndex   = valueOf(accumindexaddr);
    let accMem     = valueOf(TExp#(accumindexaddr));
    let gMem       = valueOf(TExp#(gindexaddr));

    Ifc_systolic#(nRow,nCol,mulWidth)                         systolic_array    <- mksystolic;  
    Vector#(nRow, FIFOF#(Bit#(16)))                           rowBuf            <- replicateM(mkSizedFIFOF(vnFEntries));
    Vector#(nCol, FIFOF#(Tuple3#(Bit#(16),Bit#(8),Bit#(2))))  colBuf            <- replicateM(mkSizedFIFOF(vnFEntries)); 
    //The Co-ord Value Bit#(8) is temporarily put here
    Vector#(gbufbankaddress, BRAM_DUAL_PORT_BE#(Bit#(gindexaddr),Bit#(16),2))     gBuffer  <- replicateM(mkBRAMCore2BE(gMem,False));
    Vector#(nCol, BRAM_DUAL_PORT_BE#(Bit#(accumindexaddr),Bit#(32),4))            accumBuf <- replicateM(mkBRAMCore2BE(accMem,False));
    //BRAM_DUAL_PORT_BE#(Bit#(gbufaddr), Bit#(16), 2)  gBuffer   <- mkBRAMCore2BE(gBUF,False);
    //BRAM_DUAL_PORT_BE#(Bit#(accumaddr), Bit#(32), 4) accumBuf  <- mkBRAMCore2BE(aCC,False);
    AXI4_Slave_Xactor_IFC#(`PADDR,data,0)            s_xactor  <- mkAXI4_Slave_Xactor;
    
    
    //Configuration Register Space
    Reg#(Bit#(4)) filter_rows       <- mkReg(0);  
    Reg#(Bit#(4)) filter_cols       <- mkReg(0);
    Reg#(Bit#(8)) filter_dims       =  concatReg2(filter_rows,filter_cols); 
    Reg#(Bit#(3)) padding_size      <- mkReg(0);
    Reg#(bit) startBit              <- mkReg(0);

    //FIFO Loader FSM
    Reg#(Bit#(5)) gen_state         <- mkReg(-1);   //32 states might be overkill
    Reg#(Bit#(gindexaddr)) rg_event_cntr <- mkReg(0);
    Reg#(Bit#(nRow)) rg_rl_counter <- mkReg(0);

    //Local Registers
    Reg#(Bit#(4))                 row_counter       <- mkReg(0);
    Reg#(Mem_state)               rg_wr_state       <- mkReg(Idle);
    Reg#(Mem_state)               rg_rd_state       <- mkReg(Idle);
		Reg#(AXI4_Wr_Addr#(`PADDR,0)) rg_write_packet   <- mkReg(?);
		Reg#(AXI4_Rd_Addr#(`PADDR,0)) rg_read_packet    <- mkReg(?);
    Reg#(Bit#(accumbankaddress))  rg_abufbank       <- mkRegU();
    Reg#(Bit#(gbufbankaddress))   rg_gbufbank       <- mkRegU();
    Vector#(nRow, Reg#(Bit#(gindexaddr))) vec_index <- replicateM(mkReg(0));
    Vector#(nRow, Reg#(Bit#(gbufbankaddress))) bank_index <- replicateM(mkReg(0));
    Reg#(Bit#(accumindexaddr))  rg_acc_counter <- mkReg(0);

    //Not sure if this needs to be in place -- TEMPORARY
    Vector#(nRow, Reg#(Bit#(TExp#(gbufbankaddress)))) vec_bank_counter;
    for(Integer i = 0; i < vnRow; i=i+1) begin
      vec_bank_counter[i] <- mkReg(fromInteger(i*vnRow));
    end

    //Should there be a Master interface with bigger busWidth? -- This enables Systolic to access
    //Memory directly!!! Should that power be given?
    //Assuming a Memory Map for 10 Configuration Registers --- 0 to 'h24
    //Assuming a Memory Map for 10KB from 'h28 to 

    //Reg#(Bit#(9))   accumIndex           <- mkReg(0);
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

      function Tuple2#(Bit#(gindexaddr),Bit#(gbufbankaddress)) 
               split_address_gbuf(Bit#(`PADDR) addr);

          Bit#(TSub#(`PADDR,2)) alignAddr = addr[`PADDR-1:2];  //Hardcoded Data Value
          Bit#(gindexaddr)      gindex    = alignAddr[gIndex-1:0];
          Bit#(gbufbankaddress) gbank     = alignAddr[gbufBank+gIndex-1:gIndex];
        return tuple2(gindex,gbank);
      endfunction

      function Tuple2#(Bit#(accumindexaddr), Bit#(accumbankaddress))
               split_address_accbuf(Bit#(`PADDR) addr);

          Bit#(TSub#(`PADDR,2)) alignAddr   = addr[`PADDR-1:4];  //Hardcoded Data value
          Bit#(accumbankaddress) accumbank  = alignAddr[accIndex+bufAccum-1:accIndex];
          Bit#(accumindexaddr)   accumindex = alignAddr[accIndex-1:0];
        return tuple2(accumindex,accumbank);
      endfunction

      // =========================================================
      // Function Wrappers
      // =========================================================
      Wrapper#(Bit#(`PADDR), Tuple2#(Bit#(gindexaddr),Bit#(gbufbankaddress))) split_address_gbufW <-
      mkUniqueWrapper(split_address_gbuf);

      Wrapper#(Bit#(`PADDR), Tuple2#(Bit#(accumindexaddr),Bit#(accumbankaddress)))
      split_address_accbufW <- mkUniqueWrapper(split_address_accbuf);

      // =========================================================



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

    /* ===================== Logic to populate the Global/Acc Buffers =============== */

    rule rlAXIwrReq(rg_wr_state == Idle);
     let aw <- pop_o(s_xactor.o_wr_addr);
     let w  <- pop_o(s_xactor.o_wr_data);
     //populate_buffers(aw.awaddr,w.wdata,w.wstrb);
     let lv_addr = aw.awaddr;
     let lv_data = w.wdata;
     let wstrb = w.wstrb;
     //Bit#(accumaddr) accindex = truncate(lv_addr - fromInteger(`AccumBufStart));
     //Bit#(gbufaddr) gbufindex = truncate(lv_addr - fromInteger(`GBufStart));
     let {gindex, gbufbank} <- split_address_gbufW.func(lv_addr);
     let {aindex, abufbank} <- split_address_accbufW.func(lv_addr);

          //Write Request to AccumBuffer Range
          if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
            //accumBuf.b.put(wstrb[3:0],accindex,truncate(lv_data));
            accumBuf[abufbank].b.put(wstrb[3:0],aindex,truncate(lv_data));
          end
          else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
            //gBuffer.b.put(wstrb[1:0], gbufindex, truncate(lv_data));
            gBuffer[gbufbank].b.put(wstrb[1:0],gindex,truncate(lv_data));
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
      let {gindex, gbufbank} <- split_address_gbufW.func(lv_addr);
      let {aindex, abufbank} <- split_address_accbufW.func(lv_addr);

          //Write Request to AccumBuffer Range
          if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
            accumBuf[abufbank].b.put(wstrb[3:0],aindex,truncate(lv_data));
          end
          else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
            gBuffer[gbufbank].b.put(wstrb[1:0], gindex, truncate(lv_data));
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


    //Actually there is no utility to read the GlobalIndex actually but providing the hardware
    //anyway
    rule rlAXIreadReq(rg_rd_state == Idle);
			let ar <- pop_o(s_xactor.o_rd_addr);
      rg_read_packet <= ar;
      //read_req(ar.araddr);
      let lv_addr = ar.araddr;
      //Bit#(gbufaddr) gIndex = truncate(lv_addr - fromInteger(`GBufStart));
      //Bit#(accumaddr) accIndex = truncate(lv_addr - fromInteger(`AccumBufStart));
      let {gindex,gbufbank}  <- split_address_gbufW.func(lv_addr);
      let {aindex, abufbank} <- split_address_accbufW.func(lv_addr);
      rg_abufbank <= abufbank;
      rg_gbufbank <= gbufbank;
      if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin 
          accumBuf[abufbank].a.put(0,aindex, ?);
          gACheck <= True;
        end
        else begin
          gBuffer[gbufbank].a.put(0,gindex,?);
          gACheck <= False;
        end
      rg_rd_state <= HandleBurst;
    endrule

    rule rlAXIreadBurst(rg_rd_state == HandleBurst);
      if(gACheck) begin  //Could be Potentially a buggy code
       let accumVal = accumBuf[rg_abufbank].a.read();
       let rA = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: extend(accumVal) ,rlast:
                             rg_readburst_counter==rg_read_packet.arlen, ruser: 0, 
                             rid:rg_read_packet.arid};
	    s_xactor.i_rd_data.enq(rA);
      end
      else begin
       let gVal = gBuffer[rg_gbufbank].a.read();
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
      //Bit#(gbufaddr) gIndex = truncate(lv_addr - fromInteger(`GBufStart));
      //Bit#(accumaddr) accIndex = truncate(lv_addr - fromInteger(`AccumBufStart));
      let {gindex,gbufbank}  <- split_address_gbufW.func(lv_addr);
      let {aindex, abufbank} <- split_address_accbufW.func(lv_addr);
 
      if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin 
          accumBuf[abufbank].a.put(0,aindex, ?);
          gACheck <= True;
        end
        else begin
          gBuffer[gbufbank].a.put(0,gindex,?);
          gACheck <= False;
        end
      end
       //Not Considering Transfer Size for Now
    endrule

    /* ============================================================================== */

    /* ============== Logic to Populate the Row/Col FIFO from Buffers ================ */
    /* ======================== Address Generator Circuit ============================ */
    
    //rule start_loading(gen_state>=0 && gen_state < vnRow);  //vnRow cycles
    // gen_state <= gen_state + 1; 
    //endrule
    
    //The Fanout of this entire logic is going to be huge
    //Pseudo Code
    //Need a Request Counter to send request every cycle - Req Counter stops counting at vnRowth
    //Cycle
    //Need a next address index variable which can be used to index the Gbuffer


    //NEW LOGIC

    //Need is to require throughput of one request per cycle! But the counters increment is not in
    //tandem

    //This cannot keep incrementing due to resource constraints that might cause stalls in gbuf
    rule rl_increment_counter(startBit==1'b1);
      rg_rl_counter <= rg_rl_counter + 1; 
      //Fanout of this reg is going to be huge. Can have a vector of reg with lesser size to do this
    endrule

    // Scheme - 1
    for(Integer i = 0; i < gbufBank; i=i+1) begin
      rule send_req(startBit==1'b1);
        gBuffer[i].a.put(0,vec_index[i], ?); 
        if(rg_rl_counter >= fromInteger(i*gbufBank)) begin //i*filter_row
          vec_index[i] <= vec_index[i]+1;
        end
      endrule

    //If the BRAM is not guarded, have a state switch explicitly!!
      rule recv_rsp_enq_fifo(startBit==1'b1);
        let gVal = gBuffer[i].a.read();
        for(Integer j = i*gbufBank; j < i*(gbufBank+1) ; j=j+1) begin
         if(rg_rl_counter >= fromInteger(j))  //Added now after Discussion with Neel
          rowBuf[j].enq(gVal);
        end
          //Else either enqueue zero or nothing at all
      endrule
     end

    // Scheme - 2
    // Employ Neel's scheme and have both of them in place!!! Comment what is not required

    /* ====================== Loading Value from Accum to Accum Buffer =============== */
        
      rule rl_get_accumulator;
        rg_acc_counter <= rg_acc_counter+1;
        for(Integer i = 0; i < vnCol; i=i+1) begin
          let x <- systolic_array.cfifo[i].send_accumbuf_value;
          accumBuf[i].b.put(0, rg_acc_counter, x);                 
        end
      endrule
      

    /* =============================================================================== */




  interface slave_systolic =  s_xactor.axi_side;
  endmodule


endpackage



