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

Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package systolic_top;

 /* ========= Imports ============ */ 
  import  AXI4_Types  ::*;
  import  AXI4_Fabric ::*;
  import  Connectable ::*;
  import  systolic  ::*;
  import  ConcatReg ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;
  import BRAMCore :: *;
  import BRAM ::*;
	import BUtils::*;
  import Semi_FIFOF::*;
  import UniqueWrappers::*;
  import ConfigReg::*;
  `include "systolic.defines"
	import axi_addr_generator::*;
  import functions::*;
`define PADDR 32
 /* ========= Exports ============ */
   export Ifc_systolic_top_axi4(..);
   export mksystolic_top_axi4;

  // Parameterized Interface which takes in number of rows/cols of Systolic array as input -- it
  // is capable of handling non-square systolic arrays but for simplicity of first cut design its
  // assumed to be a square size only, nRow and nCol are used for future extensions, could've done
  // with just one variable -- and it takes in accum buffer and gbuffer's addr width and also the
  // number of entries for the systolic buffers which decouples these buffers from the systolic
  // arrays. Finally mulWidth is also a parameter which is used in case we are going to use
  // parameterized multipliers.

  // For now, the inputs are the square root of the number of rows/cols which is squared to make the
  // systolic array. This is done because in WS dataflow weights are retained and general filter size
  // is 3x3, 5x5 or 7x7 -- not considering depthwise convolutions now -- Hence systolic array is
  // generally 9x9, 25x25 or 49x49 but it can be any square size!! But, for now it can't be arbitrary
  // size like 7x7 or 8x8 since it becomes hard to handle WS dataflow with arbitrary systolic array
  // size! It's not difficult to give that support though! 

  interface Ifc_systolic_top_axi4#(numeric type addr_width, numeric type data_width, 
                                   numeric type user_width, numeric type nRow, numeric type nCol, 
                                   numeric type accumbanks, numeric type accumentries,
                                   numeric type gbufbanks, numeric type gbufentries,
                                   numeric type nFEntries,numeric type mulWidth);
    interface AXI4_Slave_IFC#(`PADDR,data_width,0) slave_systolic; //32-bit address? 4 16-bits?
  endinterface
	
  typedef enum{Idle,HandleBurst, HandleGBurst, HandleABurst} Mem_state deriving(Bits,Eq);

//  (*synthesize*)
//  module mktest(Empty);
//    Ifc_systolic_top_axi4#(32, 64, 0, 3, 3, 16, 16, 2, 16) sys <- mksystolic_top_axi4;
//  endmodule
  
//    (*synthesize*)
//    module mksystolic3(Ifc_systolic_top_axi4#(32,64,0,2,2/*16,16,*/,4,1024,2,1024,2,16));
//        let ifc();
//        mksystolic_top_axi4 inst(ifc);
//        return (ifc);
//    endmodule

  (*descending_urgency="rl_send_gbuf_request, rlAXIwrReq, rl_GbufReq"*)
  (*descending_urgency="rl_send_gbuf_request, rlAXIwrReqBurst, rl_GbufReq"*)
  (*descending_urgency="rl_get_accumulator, rl_AbufReq"*)
  module mksystolic_top_axi4(Ifc_systolic_top_axi4#(addr_width,data_width,user_width,sqrtnRow,sqrtnCol,
    accumbanks, accumentries, gbufbanks, gbufentries,
    nFEntries,mulWidth))
    provisos(
             Add#(a__,2,sqrtnRow),  // Square root of Number of Rows of Array must atleast be 2
             Add#(b__,2,sqrtnCol),  // Square root of Number of Cols of Array must atleast be 2
             Mul#(sqrtnRow,sqrtnRow,nRow), //The sqrtRow is squared to get the number of rows
             Mul#(sqrtnCol,sqrtnCol,nCol), //The sqrtCol is squared to get the number of cols
             Log#(nCol, weightindexbits),
             //Commenting the below check for now, should be handled
             //Exp#(weightindexbits, nCol), // These two to ensure that nCol is a power of 2
             Mul#(mulWidth,2,mulWidth2), //mulWidth2 is a parameter which is mulWidth+2 -- Not used
             Div#(mulWidth2, 2, mulWidth),
            //The number of rowbank bits -- the number of banks required are square root of the
             //systolic array row size in our design. Why? Consider a filter size of 3x3, when 
             //unrolled it takes a column of 9 elements, but the activation inputs have a spatial
             //reuse because the filter slides across the activation plane when it comes to
             //convolution operation. Because of this sliding, it is easier to handle
             //with perfect square systolic size. But the problem here is we are fixing the array
             //size statically for some filter size say 3x3 or 5x5 hence across layers if the size 
             //varies which inevitably happens then tiling needs to happen at the software side
             //which might lead to loss of performance 
             Log#(accumbanks, accumbankbits),
             Log#(accumentries, accumindexbits),
             Log#(gbufbanks, gbufbankbits),
             Log#(gbufentries, gbufindexbits),
             //required by bsc
             Div#(data_width, mulWidth, nwords),
             Mul#(TDiv#(mulWidth2, 4), 4, mulWidth2),
             Mul#(TDiv#(mulWidth, 2), 2, mulWidth),
             Add#(c__, mulWidth, TMul#(mulWidth, 2)),
             Add#(d__, 8, data_width),
             Add#(e__, 16, data_width),
             Add#(f__, mulWidth, data_width),
             Add#(g__, mulWidth2, data_width),
             Add#(h__, 32, data_width)
            );
    let vnFEntries = valueOf(nFEntries);
    let vnRow      = valueOf(nRow);
    let vnCol      = valueOf(nCol);

    let numBanksABuf = valueOf(accumbanks);
    let numEntriesABuf = valueOf(accumentries);
    let numBanksGBuf = valueOf(gbufbanks);
    let numEntriesGBuf = valueOf(gbufentries);
    let numBankBitsABuf = valueOf(accumbankbits);
    let numIndexBitsABuf = valueOf(accumindexbits);
    let numBankBitsGBuf = valueOf(gbufbankbits);
    let numIndexBitsGBuf = valueOf(gbufindexbits);
    let numIndexBitsWeight = valueOf(weightindexbits);

    let sqRow      = valueOf(sqrtnRow);
    let sqCol      = valueOf(sqrtnCol);

    let bitWidth = valueOf(mulWidth);
    let numWords = valueOf(nwords); // Max. Number of input/weight values received from the bus per cycle

    BRAM_Configure gbufcfg = defaultValue;
    BRAM_Configure accumbufcfg = defaultValue;

    gbufcfg.memorySize = numEntriesGBuf;
    gbufcfg.loadFormat = None; //Can be used to load hex if needed 

    accumbufcfg.memorySize = numEntriesABuf;
    accumbufcfg.loadFormat = None;

    Ifc_systolic#(nRow,nCol,mulWidth) systolic_array    <- mksystolic;  
    Vector#(nRow, FIFOF#(Bit#(mulWidth))) rowBuf        <- replicateM(mkSizedFIFOF(vnFEntries));
    Vector#(nCol, FIFOF#(Tuple4#(Bit#(mulWidth),Bit#(mulWidth2),Bit#(8),Bit#(2)))) colBuf <- replicateM(mkSizedFIFOF(vnFEntries)); 
    //The Co-ord Value Bit#(8) is temporarily put here
    //Vector#(sqrtnRow, BRAM2PortBE#(Bit#(gindexaddr),Bit#(mulWidth),2))  gBuffer  <- replicateM(mkBRAM2ServerBE(gbufcfg));
    //Vector#(nCol, BRAM2PortBE#(Bit#(accumindexaddr),Bit#(mulWidth2),4)) accumBuf <- replicateM(mkBRAM2ServerBE(accumbufcfg));

    Vector#(gbufbanks, BRAM2PortBE#(Bit#(gbufindexbits), Bit#(mulWidth), 2)) gBuffer  <- replicateM(mkBRAM2ServerBE(gbufcfg));
    Vector#(accumbanks, BRAM2PortBE#(Bit#(accumindexbits), Bit#(mulWidth2), 4)) aBuffer <- replicateM(mkBRAM2ServerBE(accumbufcfg));

    //BRAM_DUAL_PORT_BE#(Bit#(gbufaddr), Bit#(16), 2)  gBuffer   <- mkBRAMCore2BE(gBUF,False);
    //BRAM_DUAL_PORT_BE#(Bit#(accumaddr), Bit#(32), 4) accumBuf  <- mkBRAMCore2BE(aCC,False);
    AXI4_Slave_Xactor_IFC#(`PADDR,data_width,0) s_xactor  <- mkAXI4_Slave_Xactor;
   
    //The current Accelerator Configuration is simple. The transactions begin by software writing
    //into weight buffers which is followed by the transfer of feature map kernels into the global
    //buffers -- Now the decision that needs to be made here is to know when to start the
    //transactions 
    //Software has visibility into reading 
    
    //Configuration Register Space
    Reg#(Bit#(4))  filter_rows       <- mkReg(0);  
    Reg#(Bit#(4))  filter_cols       <- mkReg(0);
    Reg#(Bit#(8))  filter_dims       =  concatReg2(filter_rows,filter_cols); 
    Reg#(Bit#(3))  padding_size      <- mkReg(0);
    Reg#(Bit#(8))  ifmap_rowdims     <- mkReg(0);
    Reg#(Bit#(8))  ifmap_coldims     <- mkReg(0);
    Reg#(bit)      startBit          <- mkReg(0); //Denotes the start of the transaction
    Reg#(bit)      accumWeight       <- mkReg(1); //Denotes if the ColBuf is accum or weight
    Reg#(Bit#(2))  sysConfig         =  concatReg2(accumWeight, startBit); 
    Reg#(Bit#(8))  rg_weight_counter <- mkReg(0); //Config --Hardcoded to 8 for now
    Reg#(Bool)     set_trigger_dims  <- mkReg(False);
    Reg#(Bit#(`PADDR))   input_address <- mkReg(0);
    Reg#(Maybe#(Bit#(`PADDR))) output_address <- mkReg(tagged Invalid);
    //Reg#(Bit#(8)) rg_coord_counter <- mkReg(1); //Local counter
    
    Vector#(nCol, Reg#(Bit#(8))) rg_coord_counters <- replicateM(mkReg(1));

    Vector#(nRow, Reg#(Bit#(8))) vec_row_counter <- replicateM(mkReg(0));
//  Vector#(nRow, Reg#(Bit#(3))) vec_start_value;
    Vector#(nRow, Reg#(Bit#(8))) vec_end_value   <- replicateM(mkReg(0));

    //Status Bits
    Reg#(bit) rg_idle    <- mkReg(0);
    Reg#(bit) rg_running <- mkReg(0);
    Reg#(Bit#(accumentries))  rg_acc_counter <- mkReg(0);

    //FIFO Loader FSM
    Reg#(Bit#(5)) gen_state         <- mkReg(-1);   //32 states might be overkill
    Reg#(Bit#(gbufentries)) rg_event_cntr <- mkReg(0);
    Reg#(Bit#(nRow)) rg_rl_counter <- mkReg(1);

    //Local Registers
    Reg#(Bit#(4))                 row_counter       <- mkReg(0);
    Reg#(Mem_state)               rg_wr_state       <- mkReg(Idle);
    Reg#(Mem_state)               rg_rd_state       <- mkReg(Idle);
		Reg#(AXI4_Wr_Addr#(`PADDR,0)) rg_write_packet   <- mkReg(?);
		Reg#(AXI4_Rd_Addr#(`PADDR,0)) rg_read_packet    <- mkReg(?);
		Wire#(AXI4_Rd_Addr#(`PADDR,0)) wr_gbuf          <- mkWire();
		Wire#(AXI4_Rd_Addr#(`PADDR,0)) wr_abuf          <- mkWire();
    Reg#(Bit#(accumbankbits))  rg_abufbank       <- mkRegU();
    Reg#(Bit#(gbufbankbits))   rg_gbufbank       <- mkRegU();
    Vector#(sqrtnRow, Reg#(Bit#(gbufentries))) vec_index <- replicateM(mkConfigReg(0));
    Vector#(sqrtnRow, Reg#(Bit#(gbufentries))) vec_input <- replicateM(mkConfigReg(-1)); //Have a config to clear
    Vector#(nRow, Reg#(Bit#(gbufbankbits))) bank_index <- replicateM(mkReg(0));

    Reg#(Bool) rg_feed_input <- mkReg(False);

    //Not sure if this needs to be in place -- TEMPORARY
    //Vector#(nRow, Reg#(Bit#(TExp#(gbufbankaddress)))) vec_bank_counter;
    //for(Integer i = 0; i < sqRow; i=i+1) begin
    //  vec_bank_counter[i] <- mkReg(fromInteger(i*vnRow));
    //end

    //Should there be a Master interface with bigger busWidth? -- This enables Systolic to access
    //Memory directly!!! Should that power be given?
    //Assuming a Memory Map for 10 Configuration Registers --- 0 to 'h24
    //Assuming a Memory Map for 10KB from 'h28 to 

    //Reg#(Bit#(9))   accumIndex           <- mkReg(0);
    Reg#(Bit#(8))   rg_readburst_counter <- mkReg(0);
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

      function BRAMRequestBE#(Bit#(a), Bit#(d), n) makeRequest (Bool write,Bit#(n) wstrb,Bit#(a) addr, Bit#(d)
        data);
            return BRAMRequestBE{
                                writeen: wstrb ,
                                responseOnWrite: True,
                                address   : addr,
                                datain : data
                              };
      endfunction

      function Tuple2#(Bit#(gbufindexbits),Bit#(gbufbankbits)) split_address_gbuf(Bit#(`PADDR) addr);
        Bit#(TSub#(`PADDR,2)) alignAddr = addr[`PADDR-3:0];
        Bit#(gbufbankbits) gbank = alignAddr[numBankBitsGBuf-1:0];
        Bit#(gbufindexbits) gindex = alignAddr[numIndexBitsGBuf+numBankBitsGBuf-1:numBankBitsGBuf];
        return tuple2(gindex,gbank);
      endfunction

      function Tuple2#(Bit#(accumindexbits), Bit#(accumbankbits)) split_address_accbuf(Bit#(`PADDR) addr);
        Bit#(TSub#(`PADDR,2)) alignAddr   = addr[`PADDR-1:2];
        Bit#(accumbankbits) accumbank = alignAddr[numBankBitsABuf-1:0];
        Bit#(accumindexbits) accumindex = alignAddr[numIndexBitsABuf+numBankBitsABuf-1:numBankBitsABuf];
        return tuple2(accumindex,accumbank);
      endfunction

      function Reg#(Bit#(n)) writeSideEffect (Reg#(Bit#(n)) regi, Action a);
        return ( interface Reg;
                  method Bit#(n) _read = regi._read;
                  method Action _write (Bit#(n) x);
                    regi._write(x);
                    a;
                  endmethod
                 endinterface
               );
      endfunction
      
      Reg#(Bit#(16)) ifmap_dims =
      writeSideEffect(concatReg2(ifmap_rowdims,ifmap_coldims),set_trigger_dims._write(True));

      function Action set_systolic(Bit#(`PADDR) addr, Bit#(data_width) data);
        action
            case(addr)
                `FilterDims  : filter_dims <= truncate(data);
                `IfmapDims   : ifmap_dims <= truncate(data);
                `sysConfig   : begin 
                                  sysConfig <= data[1:0];
                                  $display("\t setting startbit \n");
                                  rg_feed_input <= True;
                               end
                `CoordCount  : for(Integer i=0; i<vnCol; i=i+1)begin
                                rg_coord_counters[i] <= truncate(data);
                              end
                `weightCount : rg_weight_counter <= truncate(data);
                `InputAddr   : begin
                                input_address <= truncate(data);
                                $display($time, "Input address set: %d\n", data);
                              end
            endcase
        endaction
      endfunction

      function Bit#(16) get_systolic(Bit#(`PADDR) addr);
        Reg#(Bit#(16)) register =  (
                case(addr)
                  `IfmapDims  : ifmap_dims;
                  //`CoordCount : rg_coord_counter;
                  //`startBit  : startBit;
                   default   : readOnlyReg(16'b0);
                endcase
                );
        return register;
      endfunction
    
      //Will this be a serial path? If at all, register it off

      // =========================================================
      // Function Wrappers
      // =========================================================
      Wrapper#(Bit#(`PADDR), Tuple2#(Bit#(gbufindexbits),Bit#(gbufbankbits))) split_address_gbufW <-
      mkUniqueWrapper(split_address_gbuf);

      Wrapper#(Bit#(`PADDR), Tuple2#(Bit#(accumindexbits),Bit#(accumbankbits)))
      split_address_accbufW <- mkUniqueWrapper(split_address_accbuf);

      // =========================================================

     /* =================== Rules to Connect Row Buffers to Arrays ================== */
      for(Integer i = 0; i < vnRow; i=i+1) begin
        rule send_row_buf_value;
          Maybe#(Bit#(mulWidth)) mval = tagged Valid rowBuf[i].first;
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
          let finVal = tuple4(val1,tpl_2(val),tpl_3(val),tpl_4(val)); //This is very crude! 
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
     let {gindex, gbufferbank} = split_address_gbuf(lv_addr);
     $display($time, "lv_addr: %h lv_data: %h gindex: %h gbufferbank: %h",lv_addr,lv_data,gindex,gbufferbank);
     //Write Request to AccumBuffer Range
     if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
       //This one is not used for now, but needs to be implemented
       //accumBuf.b.put(wstrb[3:0],accindex,truncate(lv_data));
       //accumBuf[abufbank].b.put(wstrb[3:0],aindex,truncate(lv_data));
       //Send Slave Error
     end
     else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd)begin
       for(Integer i=0; i<numWords; i=i+1)begin
         Bit#(TAdd#(gbufbankbits,1)) m = zeroExtend(gbufferbank) + fromInteger(i);
         Bit#(gbufindexbits) index = gindex;
         Bit#(gbufbankbits) bank = truncate(m);
         if(bank < gbufferbank)begin 
           index = index + 1;
         end
         Bit#(mulWidth) data = lv_data[(i+1)*bitWidth-1:i*bitWidth];
         $display($time, "Sending request to gbuf: %h bank, %h index, value: %d", m, index, data);
         gBuffer[bank].portB.request.put(makeRequest(True, wstrb[1:0], index, data));
       end
     end
     else if(lv_addr >= `WeightStart && lv_addr <= `WeightEnd) begin
       Bit#(2) mulW = 0;  //Temporary data value
       if(accumWeight == 1'b1)begin
        Bit#(weightindexbits) index = lv_addr[numIndexBitsWeight+1:2];
         for(Integer i=0; i<numWords; i=i+1)begin
           Bit#(weightindexbits) m = truncate(index + fromInteger(i));// % fromInteger(vnCol); 
           colBuf[m].enq(tuple4(lv_data[(i+1)*valueOf(mulWidth)-1:i*valueOf(mulWidth)], 0, rg_coord_counters[m], mulW));
           //$display($time, "Sending weight %d to column %d till %d", lv_data[(i+1)*mulWidth-1:i*mulWidth], m, rg_coord_counters[m]);
           $display($time, "Sending weight to column %d till %d", m, rg_coord_counters[m]);
           rg_coord_counters[m] <= rg_coord_counters[m] + 1;
         end
       end
     end
     else begin //Configuration Address Space
       $display($time, "\t Setting Config Address \n");
       set_systolic(lv_addr, truncate(lv_data));
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

      let lv_addr = rg_write_packet.awaddr;
      let lv_data = w.wdata;
      let wstrb = w.wstrb;
      let {gindex, gbufferbank} = split_address_gbuf(lv_addr);

      //Write Request to AccumBuffer Range
      if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin
        //accumBuf[abufbank].b.put(wstrb[3:0],aindex,truncate(lv_data));
        //Send Slave Error
      end
      else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
        for(Integer i = 0; i < numWords ; i=i+1) begin
          Bit#(TAdd#(gbufbankbits, 1)) gbufIn = zeroExtend(gbufferbank)+fromInteger(i);
          Bit#(gbufbankbits) m = truncate(gbufIn);
          gBuffer[m].portB.request.put(makeRequest(True,wstrb[1:0],gindex,lv_data[(i+1)*valueOf(mulWidth)-1:i*valueOf(mulWidth)]));
        end
      end
      else if(lv_addr >= `WeightStart && lv_addr <= `WeightEnd) begin
        Bit#(2) mulW = 0;  //Temporary data value
        if(accumWeight == 1'b1)begin
          Bit#(numIndexBitsWeight) startindex = lv_addr[numIndexBitsWeight+1:2];
          for(Integer i=0; i<numWords; i=i+1)begin
            Bit#(4) m = (startindex + fromInteger(i));// % fromInteger(vnCol);
            colBuf[m].enq(tuple4(lv_data[(i+1)*valueOf(mulWidth)-1:i*valueOf(mulWidth)], 0, rg_coord_counters[m], mulW));
            rg_coord_counters[m] <= rg_coord_counters[m] + 1;
          end
        end
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
      let lv_addr = ar.araddr;
      if(lv_addr >= `AccumBufStart && lv_addr <= `AccumBufEnd) begin 
          $display($time, "Request to read from Accumulator");
          wr_abuf <= ar;
          rg_rd_state <= HandleABurst;
      end
      else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin
          wr_gbuf <= ar;
          rg_rd_state <= HandleGBurst;
      end
      else begin //Configuration Address Space
      end
    endrule

    rule rl_GbufReq;
        $display($time, "\t Rule GBuf Firing");
        let lv_addr = wr_gbuf.araddr;
        let {gindex,gbufbank} = split_address_gbuf(lv_addr);
        // Since every read request always sends busWidth number of bits, those many number of bits
        // should be sent. Hence, to send those many values, those many requests should be sent to
        // buffers to fetch them.
        for(Integer i=0; i<numWords; i=i+1)begin
          Bit#(TAdd#(gbufbankbits,1)) gbufIn = zeroExtend(gbufbank)+fromInteger(i);
          Bit#(gbufbankbits) m = truncate(gbufIn);
          gBuffer[m].portB.request.put(makeRequest(False,0,gindex,?));
        end
    endrule

    //These two rules can be composed into one rule, room for optimization
    //TODO: The logic inside, which is replicated an be composed to save area

    rule rlAXIGReadBurst(rg_rd_state == HandleGBurst);
      let lv_addr = rg_read_packet.araddr;
      let {gindex, gbufbank} = split_address_gbuf(lv_addr);
      Bit#(data_width) gVal = 0;
      for(Integer i=0; i<numWords; i=i+1)begin
        Bit#(TAdd#(gbufbankbits, 1)) gbufIn = zeroExtend(gbufbank) + fromInteger(i);
        Bit#(gbufbankbits) m = truncate(gbufIn);
        let temp <- gBuffer[m].portB.response.get();
        gVal = ( gVal << valueOf(mulWidth)) | zeroExtend(temp);
      end
      let rG = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: gVal, rlast:
      rg_readburst_counter==rg_read_packet.arlen, ruser: 0, rid: rg_read_packet.arid};
      s_xactor.i_rd_data.enq(rG);
      if(rg_readburst_counter == rg_read_packet.arlen)begin
        rg_readburst_counter <= 0;
        rg_rd_state <= Idle;
      end
    endrule

    rule set_dims_register(set_trigger_dims);
      for(Integer i = 0; i < vnRow; i=i+1) begin
        vec_end_value[i] <= ifmap_coldims +  fromInteger(i);
      end
    endrule

    Reg#(Bit#(8)) rg_row <- mkReg(0);
    Vector#(sqrtnRow, FIFOF#(Tuple2#(Bit#(gbufindexbits), Bit#(gbufbankbits)))) ff_flow_ctrl <- replicateM(mkSizedFIFOF(1));
    Vector#(sqrtnRow, Reg#(Bit#(8))) rg_cols <- replicateM(mkReg(0));

    function Bool end_of_row;
      Bool endRow = True;
      for(Integer i=0; i<sqRow; i=i+1)begin
        endRow = endRow && (rg_cols[i] == ifmap_coldims-1);
      end
      return endRow;
    endfunction

    //Rule to send initial acc values to the systolic array.
    Reg#(Bool) rg_temp <- mkReg(False);
    rule rl_send_acc_to_array(startBit==1'b1 &&& !rg_temp);
      for(Integer i=0; i<vnCol; i=i+1)begin
        systolic_array.cfifo[i].send_acc_value(0);
      end
      rg_temp <= True;
    endrule

    for(Integer i=0; i<sqRow; i=i+1)begin

      //Request for input activation value sent to global buffers
      rule rl_send_gbuf_request(startBit==1'b1 && rg_feed_input &&
        rg_cols[i]!=zeroExtend(filter_cols) && rg_row < ifmap_rowdims-zeroExtend(filter_rows)+1);
        let inp_addr = input_address + (zeroExtend(rg_row)+fromInteger(i)) * zeroExtend(ifmap_coldims) + zeroExtend(rg_cols[i]);
        let {gindex, gbank} = split_address_gbuf(inp_addr);
        gBuffer[gbank].portB.request.put(makeRequest(False, 0, gindex, ?));
        ff_flow_ctrl[i].enq(tuple2(gindex, gbank));
        $display(input_address, rg_row, i, ifmap_coldims, rg_cols[i]);
        $display($time, "Rule %d, Address: %d, Request for %d index sent to bank %d, rg_cols[%d]=%d, sqRow: %d",
        i, inp_addr, gindex,  gbank, i, rg_cols[i], sqRow);
      endrule
      
      //Response received from global buffers and sent to arrays
      rule rl_send_gval_to_array(startBit==1'b1 && rg_feed_input);
        let gindex = tpl_1(ff_flow_ctrl[i].first);
        let gbank = tpl_2(ff_flow_ctrl[i].first);
        ff_flow_ctrl[i].deq;
        let gVal <- gBuffer[gbank].portB.response.get();
        let numValues = min(rg_cols[i]+1, min(zeroExtend(filter_cols), ifmap_coldims-rg_cols[i]));
        let val1 = rg_cols[i]+1;
        let val2 = filter_cols;
        let val3 = ifmap_coldims-rg_cols[i];
        Bit#(8) startIndex;

        //The following logic can be optimized to a single if statement
        //Left as is for understanding purposes
        if(val1 <= zeroExtend(val2) && val1 <= val3)
          startIndex = 0;
        else if(zeroExtend(val2) <= val3 && zeroExtend(val2) <= val1)
          startIndex = 0;
        else
          startIndex = val3;
        $display("Value %d received from bank %d, startIndex = %d, numValues = %d, val1 = %d, val2 = %d, val3 = %d", gVal, gbank, startIndex, numValues, val1, val2, val3);
        for(Integer j=0; j<sqRow; j=j+1)begin
          let jj = fromInteger(j);
          if(jj >= startIndex && jj < startIndex + numValues)begin
            systolic_array.rfifo[i*sqRow+jj].send_rowbuf_value(tagged Valid gVal);
            $display($time, "Value %d from bank %d, index %d, sent to row %d", gVal, gbank, gindex, i*sqRow+jj);
          end
        end
        rg_cols[i] <= rg_cols[i] + 1;
      endrule
    end

   /* ====================== Loading Value from Accum to Accum Buffer =============== */
       
    //Put the for loop outside if you want a specific portion of systolic to operate even when some
    //other portion is not free. But the problem here is to figure out how to increment the counter.

    Rules accumbufrules = emptyRules;
    for(Integer i=0; i<vnCol; i=i+1)begin
      Rules rs = (
        rules
          //Rule to get acc value at the end of computation and store in accumulator buffer
          rule rl_get_accumulator;
            let x <- systolic_array.cfifo[i].send_accumbuf_value;
            $display($time, "Enqueuing Data into Accumulator Bank: index: %d Bank: %d with Value %d",rg_acc_counter, i, x);
            aBuffer[i].portB.request.put(makeRequest(True,'1, 0, x));                 
          endrule
        endrules);
      accumbufrules = rJoin(accumbufrules, rs);
    end

    Rules rt = (
    rules
      //Rule to reply to the master when request to read from accumulator buffer is received
      rule rl_AbufReq;
        let lv_addr = wr_abuf.araddr;
        let {aindex, abufbank} = split_address_accbuf(lv_addr);
        rg_readburst_counter <= 1;
        $display($time, "\t Rule ABuf Firing lv_addr: %h aindex: %d abufbank: %d ", lv_addr, aindex, abufbank);
        for(Integer i=0; i<numWords/2; i=i+1)begin
          Bit#(TAdd#(accumbankbits, 1)) gbufIn = zeroExtend(abufbank) + fromInteger(i);
          Bit#(accumbankbits) m = truncate(gbufIn);
          aBuffer[m].portB.request.put(makeRequest(False, 0, aindex, ?));
        end
      endrule
    endrules);

    accumbufrules = rJoinDescendingUrgency(accumbufrules, rt);
    addRules(accumbufrules);

    Rules accumbufresprules = emptyRules;
    for(Integer i = 0; i < vnCol; i=i+1) begin
    Rules rl = (
      rules
        rule rl_get_accumulator_response;
          let x <- aBuffer[i].portB.response.get();
          $display($time, "Getting Write Response for %d Value: %d", i, x);
        endrule
      endrules);
      accumbufresprules = rJoin(accumbufresprules, rl);
    end

    Rules ru = (
    rules
      rule rlAXIAReadBurst(rg_rd_state == HandleABurst && rg_readburst_counter != 0);
        let lv_addr = rg_read_packet.araddr;
        let {aindex, abufbank} = split_address_accbuf(lv_addr);
        
        //Read data from acc buffers
        Bit#(data_width) aVal = 0;
        for(Integer i=0; i<numWords/2; i=i+1)begin
          Bit#(accumbankbits) m = truncate(abufbank + fromInteger(i));
          let temp <- aBuffer[m].portB.response.get();
          aVal = (aVal << valueOf(mulWidth2)) | zeroExtend(temp);
        end
        
        //Prepare AXI response
        let rA = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: aVal, rlast:
        rg_readburst_counter==rg_read_packet.arlen, ruser: 0, rid: rg_read_packet.arid};
        
        //Send AXI response
        s_xactor.i_rd_data.enq(rA);
        
        //If last beat in response, end transaction
        if(rg_readburst_counter==rg_read_packet.arlen)begin
          rg_readburst_counter <= 0;
          rg_rd_state <= Idle;
        end
        else begin
          rg_readburst_counter <= rg_readburst_counter + 1;//Increment readburst counter
          
          //Send new request for next burst
          let new_addr = burst_address_generator(rg_read_packet.arlen, rg_read_packet.arsize,
                          rg_read_packet.arburst, rg_read_packet.araddr);
          let {newaindex, newabufbank} = split_address_accbuf(new_addr);
          
          $display($time, "\t Rule ABuf Firing lv_addr: %h aindex: %d abufbank: %d ", lv_addr, aindex, abufbank);
          
          for(Integer i=0; i<numWords/2; i=i+1)begin
            Bit#(TAdd#(accumbankbits, 1)) gbufIn = zeroExtend(newabufbank) + fromInteger(i);
            Bit#(accumbankbits) m = truncate(gbufIn);
            aBuffer[m].portB.request.put(makeRequest(False, 0, newaindex, ?));
          end
          rg_read_packet.araddr <= new_addr;
        end
      endrule

    endrules);

    accumbufresprules = rJoinDescendingUrgency(accumbufresprules, ru);

  interface slave_systolic =  s_xactor.axi_side;
  endmodule
endpackage
