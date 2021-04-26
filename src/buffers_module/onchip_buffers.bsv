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

Author: Gokulan Ravi, Mohan Prasath G R
Email id: gokulan97@gmail.com, mohanprasathr@gmail.com
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

  interface Ifc_onchip_buffers#(numeric type sram_addr_width, numeric type if_index, numeric type if_bank, numeric type if_entries,
																numeric type wt_index, numeric type wt_bank, numeric type wt_entries,
																numeric type of_index, numeric type of_bank, numeric type out_entries,
																numeric type in_width, numeric type out_width);
    interface Vector#(if_bank, BRAM2Port#(Bit#(if_index), Bit#(in_width))) ibuf;
    interface Vector#(wt_bank, BRAM2Port#(Bit#(wt_index), Bit#(in_width))) wbuf;
    interface Vector#(of_bank, BRAM2Port#(Bit#(of_index), Bit#(out_width))) obuf1;
    interface Vector#(of_bank, BRAM2Port#(Bit#(of_index), Bit#(out_width))) obuf2;
  endinterface

  module mkbuffers(Ifc_onchip_buffers#(sram_addr_width, if_index, if_bank, if_entries, wt_index, wt_bank, wt_entries, of_index, of_bank, of_entries, in_width, out_width))
    provisos(
      Log#(if_bank, if_bankbits),
      Log#(wt_bank, wt_bankbits),
      Log#(of_bank, of_bankbits),
      Mul#(in_bytes, 8, in_width),
      Mul#(out_bytes, 8, out_width),
      Log#(if_entries, if_index),
      Log#(wt_entries, wt_index),
      Log#(of_entries, of_index)
    );

    BRAM_Configure inputBufConfig = defaultValue;
    inputBufConfig.memorySize = valueOf(if_entries);
    inputBufConfig.loadFormat = None; // can be used to load hex if needed

    BRAM_Configure weightBufConfig = defaultValue;
    weightBufConfig.memorySize = valueOf(wt_entries);
    weightBufConfig.loadFormat = None;

    BRAM_Configure outputBufConfig = defaultValue;
    outputBufConfig.memorySize = valueOf(of_entries);
    outputBufConfig.loadFormat = None;

    Vector#(if_bank, BRAM2Port#(Bit#(if_index), Bit#(in_width))) ibuffer;
    Vector#(wt_bank, BRAM2Port#(Bit#(wt_index), Bit#(in_width))) wbuffer;
    Vector#(of_bank, BRAM2Port#(Bit#(of_index), Bit#(out_width))) obuffer1;
    Vector#(of_bank, BRAM2Port#(Bit#(of_index), Bit#(out_width))) obuffer2;

    ibuffer <- replicateM(mkBRAM2Server(inputBufConfig));
    wbuffer <- replicateM(mkBRAM2Server(weightBufConfig));
    obuffer1 <- replicateM(mkBRAM2Server(outputBufConfig));
    obuffer2 <- replicateM(mkBRAM2Server(outputBufConfig));

    interface ibuf = ibuffer;
    interface wbuf = wbuffer;
    interface obuf1 = obuffer1;
    interface obuf2 = obuffer2;

  endmodule

endpackage
