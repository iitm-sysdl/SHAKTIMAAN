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

package unified_buffer;
  import GetPut::*;
  import Vector::*;
  import BRAM::*;
  import BRAMCore::*;
  import BUtils::*;
  `include "systolic.defines"

  interface Ifc_unified_buffer#(numeric type entry_width,
                                numeric type num_entries,
                                numeric type log_num_entries,
                                numeric type num_banks,
                                numeric type num_buffers);
    interface Vector#(num_buffers, Vector#(num_banks, BRAM2PortBE#(Bit#(log_num_entries), Bit#(entry_width), 2))) ifc_buffer;
  endinterface

  module mkunified_buffer(Ifc_unified_buffer#(entry_width, num_entries, log_num_entries, num_banks, num_buffers))
    provisos(
      Log#(num_entries, log_num_entries),
      Div#(entry_width, 2, a__),
      Mul#(a__, 2, entry_width)
    );

    let entryWidth = valueOf(entry_width);
    let numEntries = valueOf(num_entries);
    let numBanks = valueOf(num_banks);
    let numBuffers = valueOf(num_buffers);
    let bufIndexBits = valueOf(log_num_entries);

    function BRAMRequestBE#(Bit#(a), Bit#(d), n) makeRequest (Bool write, Bit#(n) wstrb, Bit#(a) addr, Bit#(d) data);
            return BRAMRequestBE{
                                writeen: wstrb ,
                                responseOnWrite: True,
                                address   : addr,
                                datain : data
                              };
    endfunction

    BRAM_Configure bufConfig = defaultValue;
    bufConfig.memorySize = numEntries;
    bufConfig.loadFormat = None; // can be used to load hex if needed

    Vector#(num_buffers, Vector#(num_banks, BRAM2PortBE#(Bit#(log_num_entries), Bit#(entry_width), 2))) buffer;

    for(Integer i=0; i<numBuffers; i=i+1)begin
      buffer[i] <- replicateM(mkBRAM2ServerBE(bufConfig));
    end

    Vector#(num_buffers, Vector#(num_banks, BRAM2PortBE#(Bit#(log_num_entries), Bit#(entry_width), 2))) temp;

    for(Integer i=0; i<numBuffers; i=i+1)begin
      Vector#(num_banks, BRAM2PortBE#(Bit#(log_num_entries), Bit#(entry_width), 2)) temp2;
      for(Integer j=0; j<numBanks; j=j+1)begin
        temp2[j] = buffer[i][j];
      end
      temp[i] = temp2;
    end

    interface ifc_buffer = temp;

  endmodule

  (*synthesize*)
  module mkTb(Ifc_unified_buffer#(16, 16, 4, 16, 2));
    let ifc();
    mkunified_buffer buffer(ifc);
    return (ifc);
  endmodule

endpackage
