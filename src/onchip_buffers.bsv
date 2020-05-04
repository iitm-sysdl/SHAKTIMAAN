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

Author: Gokulan Ravi
Email id: gokulan97@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/

package onchip_buffers;
  import GetPut::*;
  import Vector::*;
  import BRAM::*;
  import BRAMCore::*;
  import BUtils::*;
  import isa::*;

  `include "systolic.defines"
  `define SRAM_ADDR_WIDTH 26



  interface Ifc_onchip_buffers;
    interface Vector#(`NUM_IBUF, Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 1))) input_buffer;
    interface Vector#(`NUM_WBUF, Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 1))) weight_buffer;
    interface Vector#(`NUM_OBUF, Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 4))) output_buffer;
    interface Vector#(`PBUF_BANKS, BRAM2PortBE#(Bit#(`PBUF_INDEX), Bit#(`PBUF_WIDTH), 1)) param_buffer;
  endinterface

  function BRAMRequestBE#(Bit#(a), Bit#(d), n) makeRequest (Bool write, Bit#(n) wstrb, Bit#(a) addr, Bit#(d) data);
            return BRAMRequestBE{
                                writeen: wstrb ,
                                responseOnWrite: False,
                                address   : addr,
                                datain : data
                              };
  endfunction

  function Tuple2#(Bit#(`IBUF_INDEX),Bit#(`IBUF_Bankbits)) split_address_IBUF(Bit#(addr_width) addr);
        Bit#(TSub#(`SRAM_ADDR_WIDTH,TSub#(`INBYTES,1))) alignAddr = addr[`SRAM_ADDR_WIDTH-1:`INBYTES-1];
        Bit#(`IBUF_Bankbits) gbank = alignAddr[`IBUF_Bankbits-1:0];
        Bit#(`IBUF_INDEX) gindex = alignAddr[`IBUF_INDEX+`IBUF_Bankbits-1:`IBUF_Bankbits];
        return tuple2(gindex,gbank);
  endfunction

  function Tuple2#(Bit#(`WBUF_INDEX),Bit#(`WBUF_Bankbits)) split_address_WBUF(Bit#(addr_width) addr);
        Bit#(TSub#(`SRAM_ADDR_WIDTH,TSub#(`INBYTES,1))) alignAddr = addr[`SRAM_ADDR_WIDTH-1:`INBYTES-1];
        Bit#(`WBUF_Bankbits) gbank = alignAddr[`WBUF_Bankbits-1:0];
        Bit#(`WBUF_INDEX) gindex = alignAddr[`WBUF_INDEX+`WBUF_Bankbits-1:`WBUF_Bankbits];
        return tuple2(gindex,gbank);
  endfunction

  function Tuple2#(Bit#(`OBUF_INDEX),Bit#(`OBUF_Bankbits)) split_address_OBUF(Bit#(addr_width) addr);
        Bit#(TSub#(`SRAM_ADDR_WIDTH,TSub#(`OUTBYTES,1))) alignAddr = addr[`SRAM_ADDR_WIDTH-1:`OUTBYTES-1];
        Bit#(`OBUF_Bankbits) gbank = alignAddr[`OBUF_Bankbits-1:0];
        Bit#(`OBUF_INDEX) gindex = alignAddr[`OBUF_INDEX+`OBUF_Bankbits-1:`OBUF_Bankbits];
        return tuple2(gindex,gbank);
  endfunction

  function Tuple2#(Bit#(`PBUF_INDEX),Bit#(`PBUF_Bankbits)) split_address_PBUF(Bit#(addr_width) addr);
        Bit#(TSub#(`SRAM_ADDR_WIDTH,1)) alignAddr = addr[`SRAM_ADDR_WIDTH-1:0];
        Bit#(`PBUF_Bankbits) gbank = alignAddr[`PBUF_Bankbits-1:0];
        Bit#(`PBUF_INDEX) gindex = alignAddr[`PBUF_INDEX+`PBUF_Bankbits-1:`PBUF_Bankbits];
        return tuple2(gindex,gbank);
  endfunction


  module mkbuffers(Ifc_onchip_buffers)
    provisos(
    );
    

    BRAM_Configure inputBufConfig = defaultValue;
    inputBufConfig.memorySize = valueOf(`IBUF_ENTRIES);
    inputBufConfig.loadFormat = None; // can be used to load hex if needed

    BRAM_Configure weightBufConfig = defaultValue;
    weightBufConfig.memorySize = `WBUF_ENTRIES;
    weightBufConfig.loadFormat = None;

    BRAM_Configure outputBufConfig = defaultValue;
    outputBufConfig.memorySize = `OBUF_ENTRIES;
    outputBufConfig.loadFormat = None;

    Vector#(`NUM_IBUF, Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 1))) ibuf;
    Vector#(`NUM_WBUF, Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 1))) wbuf;
    Vector#(`NUM_OBUF, Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 4))) obuf;
    Vector#(`PBUF_BANKS, BRAM2PortBE#(Bit#(`PBUF_INDEX), Bit#(`PBUF_WIDTH), 1)) pbuf <- replicateM(mkBRAM2ServerBE(inputBufConfig));

    for(Integer i=0; i<`NUM_IBUF; i=i+1)begin
      // for(Integer j=0; j<`IBUF_BANKS; j=j+1)begin
        // ibuf[i][j] <- mkBRAM2ServerBE(inputBufConfig);
      // end
      ibuf[i] <- replicateM(mkBRAM2ServerBE(inputBufConfig));
    end

    for(Integer i=0; i<`NUM_WBUF; i=i+1)begin
      // for(Integer j=0; j<`WBUF_BANKS; j=j+1)begin
      //   wbuf[i][j] <- mkBRAM2ServerBE(weightBufConfig);
      // end
      wbuf[i] <- replicateM(mkBRAM2ServerBE(weightBufConfig));
    end

    for(Integer i=0; i<`NUM_OBUF; i=i+1)begin
      // for(Integer j=0; j<`OBUF_BANKS; j=j+1)begin
      //   obuf[i][j] <- mkBRAM2ServerBE(outputBufConfig);
      // end
      obuf[i] <- replicateM(mkBRAM2ServerBE(outputBufConfig));
    end

    Vector#(`NUM_IBUF, Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 1))) itemp;
    Vector#(`NUM_WBUF, Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 1))) wtemp;
    Vector#(`NUM_OBUF, Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 4))) otemp;
    Vector#(`PBUF_BANKS, BRAM2PortBE#(Bit#(`PBUF_INDEX), Bit#(`PBUF_WIDTH), 1)) ptemp;

    for(Integer i=0; i<`NUM_IBUF; i=i+1)begin
      // Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 1)) itemp2;
      // for(Integer j=0; j<`IBUF_BANKS; j=j+1)begin
      //   itemp[i][j] = ibuf[i][j];
      // end
      itemp[i] = ibuf[i];
    end

    for(Integer i=0; i<`NUM_WBUF; i=i+1)begin
      // Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 1)) wtemp2;
      // for(Integer j=0; j<`WBUF_BANKS; j=j+1)begin
      //   wtemp[i][j] = wbuf[i][j];
      // end
      // wtemp[i] = wtemp2;
      wtemp[i] = wbuf[i];
    end

    for(Integer i=0; i<`NUM_OBUF; i=i+1)begin
      // Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 4)) otemp2;
      // for(Integer j=0; j<`OBUF_BANKS; j=j+1)begin
      //   otemp[i][j] = obuf[i][j];
      // end
      // otemp[i] = otemp2;
      otemp[i] = obuf[i];
    end

    ptemp = pbuf;

    interface input_buffer = itemp;
    interface weight_buffer = wtemp;
    interface output_buffer = otemp;
    interface param_buffer = ptemp;

  endmodule

  // (*synthesize*)
  // module mkTb(Ifc_onchip_buffers);
  //   let ifc();
  //   mkbuffers buffer(ifc);
  //   return (ifc);
  // endmodule

endpackage
