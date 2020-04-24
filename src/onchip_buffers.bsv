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
  `include "systolic.defines"

  interface Ifc_onchip_buffers;
    interface Vector#(`NUM_IBUF, Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 2))) input_buffer;
    interface Vector#(`NUM_WBUF, Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 2))) weight_buffer;
    interface Vector#(`NUM_OBUF, Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 2))) output_buffer;
  endinterface

  module mkbuffers(Ifc_unified_buffer)
    provisos(
    );

    function BRAMRequestBE#(Bit#(a), Bit#(d), n) makeRequest (Bool write, Bit#(n) wstrb, Bit#(a) addr, Bit#(d) data);
            return BRAMRequestBE{
                                writeen: wstrb ,
                                responseOnWrite: True,
                                address   : addr,
                                datain : data
                              };
    endfunction

    BRAM_Configure inputBufConfig = defaultValue;
    inputBufConfig.memorySize = `IBUF_ENTRIES;
    inputBufConfig.loadFormat = None; // can be used to load hex if needed

    BRAM_Configure weightBufConfig = defaultValue;
    weightBufConfig.memorySize = `WBUF_ENTRIES;
    weightBufConfig.loadFormat = None;

    BRAM_Configure outputBufConfig = defaultValue;
    outputBufConfig.memorySize = `OBUF_ENTRIES;
    outputBufConfig.loadFormat = None;

    Vector#(`NUM_IBUF, Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 2))) ibuf;
    Vector#(`NUM_WBUF, Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 2))) wbuf;
    Vector#(`NUM_OBUF, Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 2))) obuf;

    for(Integer i=0; i<`NUM_IBUF; i=i+1)begin
      ibuf[i] <- replicateM(mkBRAM2ServerBE(inputBufConfig));
    end
    for(Integer i=0; i<`NUM_IBUF; i=i+1)begin
      wbuf[i] <- replicateM(mkBRAM2ServerBE(weightBufConfig));
    end
    for(Integer i=0; i<`NUM_IBUF; i=i+1)begin
      obuf[i] <- replicateM(mkBRAM2ServerBE(outputBufConfig));
    end

    Vector#(`NUM_IBUF, Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 2))) itemp;
    Vector#(`NUM_WBUF, Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 2))) wtemp;
    Vector#(`NUM_OBUF, Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 2))) otemp;

    for(Integer i=0; i<`NUM_IBUF; i=i+1)begin
      Vector#(`IBUF_BANKS, BRAM2PortBE#(Bit#(`IBUF_INDEX), Bit#(`INWIDTH), 2)) itemp2;
      for(Integer j=0; j<`IBUF_BANKS; j=j+1)begin
        itemp2[j] = ibuf[i][j];
      end
      itemp[i] = itemp2;
    end

    for(Integer i=0; i<`NUM_WBUF; i=i+1)begin
      Vector#(`WBUF_BANKS, BRAM2PortBE#(Bit#(`WBUF_INDEX), Bit#(`INWIDTH), 2)) wtemp2;
      for(Integer j=0; j<`WBUF_BANKS; j=j+1)begin
        wtemp2[j] = wbuf[i][j];
      end
      wtemp[i] = wtemp2;
    end

    for(Integer i=0; i<`NUM_OBUF; i=i+1)begin
      Vector#(`OBUF_BANKS, BRAM2PortBE#(Bit#(`OBUF_INDEX), Bit#(`OUTWIDTH), 2)) otemp2;
      for(Integer j=0; j<`OBUF_BANKS; j=j+1)begin
        otemp2[j] = obuf[i][j];
      end
      otemp[i] = otemp2;
    end

    interface input_buffer = itemp;
    interface weight_buffer = wtemp;
    interface output_buffer = otemp;

  endmodule

  (*synthesize*)
  module mkTb(Ifc_onchip_buffers);
    let ifc();
    mkbuffers buffer(ifc);
    return (ifc);
  endmodule

endpackage
