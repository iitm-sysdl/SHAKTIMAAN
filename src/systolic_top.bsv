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
  `include "defined_parameters.bsv"
  import defined_types::*;
  `include "systolic.defines"
  import axi_addr_generator::*;
  import functions::*;

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
   interface AXI4_Master_IFC#(addr_width, data_width, user_width) master;
   interface AXI4_Slave_IFC#(`PADDR,data_width,0) slave; //32-bit address? 4 16-bits?
  endinterface

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

    AXI4_Master_Xactor_IFC #(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;  
    AXI4_Slave_Xactor_IFC#(`PADDR,data_width,0) s_xactor  <- mkAXI4_Slave_Xactor;

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

    //DMA config Registers

    //Mem to Systolic
    Reg#(Bit#(`PADDR)) input_address <- mkReg(0);  //start address of either gbuf or weight
    Reg#(Bit#(`PADDR)) mem_rdaddr <- mkReg(0);  // memory read address register
    Reg#(Bit#(32)) mem_sys_cr <- mkConfigReg(0);  //mem to systolic config register, definition still incomplete, kept it as 32bits for now
    Reg#(Bit#(16)) mem_sys_cntr <- mkConfigReg(0); //no. of bursts to be transferred
    //Systolic to Mem
    Reg#(Bit#(`PADDR)) output_address <- mkReg(0);  //start address of accumbuf
    Reg#(Bit#(`PADDR)) mem_wraddr <- mkReg(0);  // memory write address register
    Reg#(Bit#(32)) sys_mem_cr <- mkConfigReg(0);  //systolic to mem config register
    Reg#(Bit#(16)) sys_mem_cntr <- mkConfigReg(0); //no. of bursts to be transferred

    //Local registers
    Reg#(Bool) rg_is_memsyscntr_zero[2] <- mkCReg(2,True);
    Reg#(Bool) rg_is_sysmemcntr_zero[2] <- mkCReg(2,True);

    Reg#(Bit#(addr_width)) rg_inp_addr <- mkConfigReg(0);   //Local globalbuf or weight address register - start address
    Reg#(Bit#(addr_width)) rg_mem_rdaddr <- mkConfigReg(0); //local memory read address register - start address
    Reg#(Bit#(addr_width)) rg_out_addr <- mkConfigReg(0);   //Local accumbuf address register - start address
    Reg#(Bit#(addr_width)) rg_mem_wraddr <- mkConfigReg(0); //local memory write address register - start address

    Reg#(Bit#(8)) rg_sys_wrburst_cnt <- mkReg(0);
    Reg#(Bit#(8)) rg_mem_wrburst_cnt <- mkReg(0);


    Reg#(Bit#(addr_width)) rg_syswrite_addr <- mkReg(0); //registers to store incremental burst addresses
    Reg#(Bit#(addr_width)) rg_sysread_addr <- mkReg(0);  //registers to store incremental burst addresses

    Reg#(Bool) rg_finish_memwrite[2] <- mkCReg(2,True);










    // We also want to pass the destination address for each read over
    // to the write "side", along with some other metadata.
    // The depth of this fifo limits the number of outstanding reads
    // which may be pending before the write.  The maximum outstanding
    // reads depends on the overall latency of the read requests.
    FIFOF#(Bit#(addr_width)) destAddrFs_sys <- mkSizedFIFOF(2) ;  //memory to systolic
    FIFOF#(Bit#(addr_width)) destAddrFs_mem <- mkSizedFIFOF(2) ;  // systolic to memory


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
                `OutputAddr   : begin
                                output_address <= truncate(data);
                                end
                `MemReadAddr : begin
                                mem_rdaddr <= truncate(data);
                                end
                `MemtoSysCfg   : begin
                                  //if(data[0]== 1) begin  //going to enable the transfer
                                    //if(mem_sys_cr[0]) begin  // transfer is not already going on
                                      mem_sys_cr <= truncate(data);
                                      rg_inp_addr <= input_address;
                                      rg_mem_rdaddr <= mem_rdaddr;
                                    //end
                                  end
                                end
                `MemtoSysCntr   : begin
                                    mem_sys_cntr <= truncate(data);
                                    $display($time, "MemtoSys Cntr set: %d\n", data);
                                  end
                `SystoMemCfg   : begin
                                  //if(data[0] == 1) begin  //going to enable the transfer
                                    //if(sys_mem_cr[0] == 1) begin  // transfer is not already going on
                                      sys_mem_cr <= truncate(data);
                                      rg_out_addr <= output_address;
                                      rg_mem_wraddr <= mem_wraddr;
                                    end
                                  end
                                end
                `MemWriteAddr  : begin
                                  mem_wraddr <= truncate(data);
                                 end
                `SystoMemCntr   : begin
                                  sys_mem_cntr <= truncate(data);
                                  $display($time, "SystoMem Cntr set: %d\n", data);
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

    /* ===========================AXI - Logic to write to Configure Registers ==================================== */

    rule rlAXIwrConfig;

     let aw <- pop_o(s_xactor.o_wr_addr);
     let w  <- pop_o(s_xactor.o_wr_data);
     let lv_addr = aw.awaddr;
     let lv_data = w.wdata;
     
     $display($time, "\t Setting Config Address \n");
     set_systolic(lv_addr, truncate(lv_data));
     
     let resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
     s_xactor.i_wr_resp.enq (resp);

    endrule
    /* ==================================================================================================== */

    function Bit#(16) fn_decr_cndtr(Bit#(16) cndtr, Bit#(2) tsize, Bit#(`Burst_length_bits) bsize);
    Bit#(17) lv_to_sub= (zeroExtend(bsize)+1) << tsize;
    Bit#(17) lv_result= {1'b0,cndtr}-lv_to_sub;
    if(lv_result[16]==1)    //underflow. Can happen in burst mode when the bytes to be transferred is not an exact multiple of the burst length x burst size.
      return 0;
        else
        return lv_result[15:0];
    endfunction

    // This function increments the source or destination address depending on
    // the size of the transfer.
    // Note that though STM's DMA defines tsize=2'b11 as reserved, we use it to
    // perform a 64-bit data transfer.
    function Bit#(addr_width) fn_incr_address(Bit#(addr_width) addr, Bit#(2) tsize, Bit#(`Burst_length_bits) bsize) provisos(Add#(addr_width,1, a),
                        Add#(z__, 8, a));
      Bit#(a) lv_to_add= (zeroExtend(bsize)+1) << tsize;
      Bit#(a) lv_result= {1'b0,addr}+lv_to_add;
      return truncate(lv_result);
    endfunction

    /* =================================AXI - Logic to read from Memory and write to Systolic ============================== */
  
    rule rl_memcntr_is_zero;
      rg_is_memsyscntr_zero[0] <= (mem_sys_cntr == 0);
    endrule 

    rule rl_startMemRead (!rg_is_memsyscntr_zero[0]//no of bytes remaining to transfer is not 0
                          && mem_sys_cr[0] == 1);
        let lv_mem_sys_cr = mem_sys_cr;
        Bit#(addr_width) lv_araddr = rg_mem_rdaddr; //set the address to read from
        Bit#(2) lv_arsize = 2'b11; //set the transfer size -- fixing to 8 bytes
        Bit#(1) lv_burst_type = lv_mem_sys_cr[7]; //0: Fixed, 1: INCR which is consistent with that of AXI4
        Bit#(8) lv_burst = lv_mem_sys_cr[15:8];  //burst length

        if(lv_mem_sys_cr[6]==1)  //peripheral increment mode is on
          rg_inp_addr<= fn_incr_address(rg_inp_addr, lv_arsize, mem_sys_cr[15:8]);
        if(lv_mem_sys_cr[7]==1)  //memory increment mode is on
          rg_mem_rdaddr<= fn_incr_address(rg_mem_rdaddr, lv_arsize, mem_sys_cr[15:8]);

        $display($time,"\tSystolic starting read from memory address %h", lv_araddr);

        destAddrFs_sys.enq(rg_inp_addr); //Enqueue the write address

        // Create a read request, and enqueue it
        // Since there can be multiple pending requests, either read or
        // writes, we use the arid field to mark these.
        let read_request = AXI4_Rd_Addr {araddr: lv_araddr, 
                         arid: {4'b0101}, arlen: lv_burst,
                         arsize: zeroExtend(lv_arsize), arburst: zeroExtend(lv_burst_type), //arburst: 00-FIXED 01-INCR 10-WRAP
                         aruser: 0 };  //0101 --random id to mark the requests
          
        m_xactor.i_rd_addr.enq(read_request);
        $display("Sending a read request with araddr: %h arid: 001 arlen: %h arsize: %h arburst: %h",lv_araddr,lv_burst,lv_arsize,lv_burst_type); 

       // currentReadRs[chanNum][0]<= currentReadRs[chanNum][0] + 1;
        mem_sys_cntr <= fn_decr_cndtr(mem_sys_cntr, lv_arsize, mem_sys_cntr[15:8]); //Request for one burst is complete

    endrule

    rule rl_startSystolicWrite(m_xactor.o_rd_data.first.rid == {4'b0101} && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                                && rg_sys_wrburst_cnt == 0)

        let resp <- pop_o(m_xactor.o_rd_data);
        let lv_mem_sys_cr = mem_sys_cr;
        let lv_addr = destAddrFs_sys.first;
        let lv_data = resp.rdata;
        let lv_burst_type = lv_mem_sys_cr[6];   //burst type of systolic
        let lv_awsize = 2'b11; //set the transfer size -- fixing to 8 bytes
        Bit#(8) lv_burst_len = lv_mem_sys_cr[15:8];
        let {gindex, gbufferbank} = split_address_gbuf(lv_addr);
        //$display($time, "lv_addr: %h lv_data: %h gindex: %h gbufferbank: %h",lv_addr,lv_data,gindex,gbufferbank);

         if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd)begin
           for(Integer i=0; i<numWords; i=i+1)begin
             Bit#(TAdd#(gbufbankbits,1)) m = zeroExtend(gbufferbank) + fromInteger(i);
             Bit#(gbufindexbits) index = gindex;
             Bit#(gbufbankbits) bank = truncate(m);
             if(bank < gbufferbank)begin 
               index = index + 1;
             end
             Bit#(mulWidth) data = lv_data[(i+1)*bitWidth-1:i*bitWidth];
             $display($time, "Sending request to gbuf: %h bank, %h index, value: %d", m, index, data);
             gBuffer[bank].portB.request.put(makeRequest(True, 1'b1 , index, data));
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

        if(lv_burst_len>0) begin 
          rg_sys_wrburst_cnt<=rg_sys_wrburst_cnt + 1;  //// this will fire the next rule when doing a write in burst mode.
          let new_address=burst_address_generator(lv_burst_len,lv_awsize,lv_burst_type,lv_addr);
          rg_syswrite_addr <= new_address;
        // $display("Starting burst mode write....");
        end

        destAddrFs_sys.deq;  //dequeing this FIFO will cause startRead to fire.
        $display ($time,"\t startWrite addr: %h data: %h", lv_addr,resp.rdata);

    endrule

    rule rl_systolicWriteBurst (m_xactor.o_rd_data.first.rid == {4'b0101} && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                                && rg_sys_wrburst_cnt != 0);

        Bool lv_last = (rg_sys_wrburst_cnt == mem_sys_cr[15:8]) //burst length in config reg

        let resp <- pop_o(m_xactor.o_rd_data);

        let lv_mem_sys_cr = mem_sys_cr;
        let lv_addr = rg_syswrite_addr;
        let lv_data = resp.rdata;
        let lv_burst_type = lv_mem_sys_cr[6];   //burst type of systolic
        let lv_awsize = 2'b11;
        Bit#(8) lv_burst_len = lv_mem_sys_cr[15:8];

        let {gindex, gbufferbank} = split_address_gbuf(lv_addr);

        else if(lv_addr >= `GBufStart && lv_addr <= `GBufEnd) begin //write to gbuf
          for(Integer i = 0; i < numWords ; i=i+1) begin
            Bit#(TAdd#(gbufbankbits, 1)) gbufIn = zeroExtend(gbufferbank)+fromInteger(i);
            Bit#(gbufbankbits) m = truncate(gbufIn);
            gBuffer[m].portB.request.put(makeRequest(True,'b1,gindex,lv_data[(i+1)*valueOf(mulWidth)-1:i*valueOf(mulWidth)]));
          end
        end
        else if(lv_addr >= `WeightStart && lv_addr <= `WeightEnd) begin //write to weight fifos
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
        
        let new_address=burst_address_generator(lv_burst_len,lv_awsize,lv_burst_type,lv_addr);
        rg_syswrite_addr <=new_address;

        if(lv_last)begin
          $display("\tLast data received..."); // last beat of the current burst
          rg_sys_wrburst_cnt<=0;  // rl_startSystolicWrite will fire next for next set of burst 
          if(rg_is_memsyscntr_zero) begin
            $display("\tMemory to systolic transfer done\n");  //last beat of last burst
          end
        end
        else begin
          rg_sys_wrburst_cnt<=rg_sys_wrburst_cnt + 1;
        end

    endrule


    /* ==================================================================================================== */

    /* ======================Sending values from gbuf and weights to fifos and starting systolic operation==============*/

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
        let inp_addr = gbuf_startaddr + (zeroExtend(rg_row)+fromInteger(i)) * zeroExtend(ifmap_coldims) + zeroExtend(rg_cols[i]);
        let {gindex, gbank} = split_address_gbuf(inp_addr);
        gBuffer[gbank].portB.request.put(makeRequest(False, 0, gindex, ?));
        ff_flow_ctrl[i].enq(tuple2(gindex, gbank));
        $display(gbuf_startaddr, rg_row, i, ifmap_coldims, rg_cols[i]);
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

    /* =====================AXI - Logic to read from Systolic and write to Memory ============================= */

    rule rl_syscntr_is_zero;
      rg_is_sysmemcntr_zero[0] <= (sys_mem_cntr == 0);
    endrule 

    //Put the for loop outside if you want a specific portion of systolic to operate even when some
    //other portion is not free. But the problem here is to figure out how to increment the counter.

    Rules accumbufrules = emptyRules; //set to rules which send request to accumbuffer

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
    //rule to start read from the accum buffer
    rule rl_startSystolicRead (!rg_is_sysmemcntr_zero[0] && sys_mem_cr[0] == 1); //no. of bytes to transfer is not zero

        let lv_sys_mem_cr = sys_mem_cr;
        Bit#(addr_width) lv_araddr = rg_out_addr; //set the address to read from
        Bit#(2) lv_arsize = 2'b11; //set the transfer size -- fixing to 8 bytes
        Bit#(1) lv_burst_type = lv_sys_mem_cr[6]; //0: Fixed, 1: INCR which is consistent with that of AXI4
        Bit#(8) lv_burst = lv_sys_mem_cr[15:8];  //burst length

        if(lv_sys_mem_cr[6]==1)  //peripheral increment mode is on
          rg_out_addr<= fn_incr_address(rg_out_addr, lv_arsize, sys_mem_cr[15:8]);
        if(lv_mem_sys_cr[7]==1)  //memory increment mode is on
          rg_mem_wraddr<= fn_incr_address(rg_mem_wraddr, lv_arsize, sys_mem_cr[15:8]);

        $display($time,"\tMemory starting read from accumbuff address %h",chanNum, lv_araddr);

        destAddrFs_mem.enq(rg_mem_wraddr);

        //Send read request for the first beat
        let {aindex, abufbank} = split_address_accbuf(lv_araddr);
        //$display($time, "\t Rule ABuf Firing lv_addr: %h aindex: %d abufbank: %d ", lv_addr, aindex, abufbank);
        for(Integer i=0; i<numWords/2; i=i+1)begin
          Bit#(TAdd#(accumbankbits, 1)) gbufIn = zeroExtend(abufbank) + fromInteger(i);
          Bit#(accumbankbits) m = truncate(gbufIn);
          aBuffer[m].portB.request.put(makeRequest(False, 0, aindex, ?));
        end

        rg_sysread_addr <= rg_out_addr;

        sys_mem_cntr <= fn_decr_cndtr(sys_mem_cntr, lv_arsize, sys_mem_cntr[15:8]); 

    endrule
    endrules);

    accumbufrules = rJoinDescendingUrgency(accumbufrules, rt);
    addRules(accumbufrules);

    Rules accumbufresprules = emptyRules; // set of rules that get response from accumbuffer

    for(Integer i = 0; i < vnCol; i=i+1) begin
    Rules rl = (
      rules
        //rule to get write response from accumbuffer -- is this necessary? -- Mohan
        rule rl_get_accumulator_response;
          let x <- aBuffer[i].portB.response.get();
          $display($time, "Getting Write Response for %d Value: %d", i, x);
        endrule
      endrules);
      accumbufresprules = rJoin(accumbufresprules, rl);
    end

    Rules ru = (
    rules
    // rule to get read response from accumbuf and start writing to mem
    rule rl_startMemWrite (rg_mem_wrburst_cnt == 0 && rg_finish_memwrite[1] == True);  //sending first beat to memory

        let {aindex, abufbank} = split_address_accbuf(rg_sysread_addr);

        //Read data from accum buffers
        Bit#(data_width) aVal = 0;
        for(Integer i=0; i<numWords/2; i=i+1)begin
          Bit#(accumbankbits) m = truncate(abufbank + fromInteger(i));
          let temp <- aBuffer[m].portB.response.get();
          aVal = (aVal << valueOf(mulWidth2)) | zeroExtend(temp);  // contains the required data
        end

        let lv_awaddr = destAddrFs_mem.first;
        let lv_sys_mem_cr = sys_mem_cr;
        Bit#(2) lv_tsize = 2'b11; //set the transfer size -- fixing to 8 bytes
        Bit#(1) lv_burst_type = lv_sys_mem_cr[7]; //0: Fixed, 1: INCR which is consistent with that of AXI4
        Bit#(8) lv_burst_len = lv_sys_mem_cr[15:8];  //burst length

        //write_strobe -- needs to be defined
        Bool lv_last = True;

        if(lv_burst_len != 0) begin
          lv_last = False;
          rg_mem_wrburst_cnt <= rg_mem_wrburst_cnt + 1;

          //Send new request for next burst
          let new_addr = burst_address_generator(lv_burst_len, lv_tsize,
                          lv_sys_mem_cr[6], rg_sysread_addr);
          let {newaindex, newabufbank} = split_address_accbuf(new_addr);
                  
          for(Integer i=0; i<numWords/2; i=i+1)begin
            Bit#(TAdd#(accumbankbits, 1)) gbufIn = zeroExtend(newabufbank) + fromInteger(i);
            Bit#(accumbankbits) m = truncate(gbufIn);
            aBuffer[m].portB.request.put(makeRequest(False, 0, newaindex, ?));
          end
          rg_sysread_addr <= new_addr;
        end

        let write_data = AXI4_Wr_Data { wdata: aVal, wstrb: write_strobe, wlast: lv_last, wid: 4'b0011};
        let write_addr = AXI4_Wr_Addr { awaddr: lv_awaddr, awuser: 0, awlen: lv_burst_len,
                          awsize: zeroExtend(lv_tsize), awburst: zeroExtend(lv_burst_type), awid: {4'b0011 };

        // enqueue the request.
        m_xactor.i_wr_data.enq(write_data);
        m_xactor.i_wr_addr.enq(write_addr);

        destAddrFs_mem.deq;  //dequeing this FIFO will cause startSystolicRead to fire

        rg_finish_memwrite[1] <= False;

    endrule

    //rule to send burst data from accumbuf to memory
    rule rl_Memwriteburst (rg_mem_wrburst_cnt != 0); // start burst write to memory

        let {aindex, abufbank} = split_address_accbuf(rg_sysread_addr);

        let lv_sys_mem_cr = sys_mem_cr;
        Bool lv_last == (rg_mem_wrburst_cnt == lv_sys_mem_cr[15:8]);

        //Read data from accum buffers
        Bit#(data_width) aVal = 0;
        for(Integer i=0; i<numWords/2; i=i+1)begin
          Bit#(accumbankbits) m = truncate(abufbank + fromInteger(i));
          let temp <- aBuffer[m].portB.response.get();
          aVal = (aVal << valueOf(mulWidth2)) | zeroExtend(temp);  // contains the required data
        end

        Bit#(2) lv_tsize = 2'b11; //set the transfer size -- fixing to 8 bytes
        Bit#(1) lv_burst_type = lv_sys_mem_cr[7]; //0: Fixed, 1: INCR which is consistent with that of AXI4
        Bit#(8) lv_burst_len = lv_sys_mem_cr[15:8];  //burst length

        //write_strobe -- yet to be defined

        let write_data = AXI4_Wr_Data { wdata: aVal, wstrb: write_strobe, wlast: lv_last, wid: 4'b0011};

        m_xactor.i_wr_data.enq(write_data);

        if(lv_last) begin
           $display("\tLast data sent..."); // last beat of the current burst
          rg_mem_wrburst_cnt <= 0; // rl_startMemwrite for next burst will fire
        end
        else begin
          rg_mem_wrburst_cnt <= rg_mem_wrburst_cnt + 1;`

          //Send new request for next burst
          let new_addr = burst_address_generator(lv_burst_len, lv_tsize, lv_sys_mem_cr[6], rg_sysread_addr);
          let {newaindex, newabufbank} = split_address_accbuf(new_addr);
                  
          for(Integer i=0; i<numWords/2; i=i+1)begin
            Bit#(TAdd#(accumbankbits, 1)) gbufIn = zeroExtend(newabufbank) + fromInteger(i);
            Bit#(accumbankbits) m = truncate(gbufIn);
            aBuffer[m].portB.request.put(makeRequest(False, 0, newaindex, ?));
          end
          rg_sysread_addr <= new_addr;
        end

    endrule

    rule rl_finishMemwrite((m_xactor.o_wr_resp.first.bid == 4'b0011) &&
        (m_xactor.o_wr_resp.first.bresp==AXI4_OKAY));

        let x<- pop_o(m_xactor.o_wr_resp) ;      // take the response data and finish
        $display ("Finished burst mem write, remaining bytes = %0d",sys_mem_cntr);
        rg_finish_memwrite[0] <= True;

    endrule

    endrules);

    accumbufresprules = rJoinDescendingUrgency(accumbufresprules, ru);

    /* ========================================================================================= */

    endmodule

endpackage











