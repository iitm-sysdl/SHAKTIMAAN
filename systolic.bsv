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
Details: Configurable Systolic Array Unit

Having an AXI-4 Interface for now, where the array is connected to the bus. Should change it,
like having a decoder to decode instructions and feeding to the Array.

Verification Status: Unverified
FPGA/ASIC Synthesis Status: Unsynthesized
Optimization Status : Unoptimized
--------------------------------------------------------------------------------------------------
*/
package systolic;
  import  intMul::*;
  import  AXI4_Types  ::*;
  import  AXI4_Fabric ::*;
  import  Connectable ::*;
  import  FIFOF::*;

  interface Ifc_systolic#(numeric type nRow, numeric type nCol, 
                          numeric type addr, numeric type data, 
                          numeric type nFEntries, numeric type mulWidth);
    interface AXI4_Slave_IFC#(addr,data,0) slave_systolic; //32-bit address? 4 16-bits?
  endinterface

  module mksystolic(Ifc_systolic#(nRow,nCol,addr,data))
      provisos (
               );
      let vnRow = valueOf(nRow);
      let vnCol = valueOf(nCol);
      AXI4_Slave_Xactor_IFC #(addr, data, 0)      s_xactor <- mkAXI4_Slave_Xactor;
      Vector#(vnRow, FIFOF#(Bit#(mulWidth)))      rowBuf   <- replicateM(mkSizedFIFOF(nFEntries));
      Vector#(vnCol, FIFOF#(Bit#(mulWidth)))      colBuf   <- replicateM(mkSizedFIFOF(nFEntries));
      Vector#(vnRow, Vector#(vnCol,Ifc_intMul))   intArray <- replicateM(replicateM(mkintMul)); 

      /* ==================== Systolic Array Connections ======================*/
      //Connections in the End Points alone
      for(Integer i = 0; i < vnCol-1; i=i+1) begin
        mkConnection(intArray[0][i].to_east, intArray[0][i+1].from_west);
        mkConnection(intArray[vnCol-1][i].to_east, intArray[vnCol-1][i+1].from_west);
      end

      for(Integer i = 0; i < vnRow-1; i=i+1) begin
        mkConnection(intArray[i][0].to_south, intArray[i+1][0].from_north);
        mkConnection(intArray[i][vnCol-1].to_south, intArray[i+1][vnCol-1].from_north);
      end

      //Connections to PE which are not covered in the End-Points
      for(Integer i = 1; i < vnRow-1; i=i+1) begin
        for(Integer j = 1; j < vnCol-1; j=j+1) begin
          mkConnection(intArray[i][0].to_east,intArray[i][1].from_west);
          mkConnection(intArray[0][j].to_south,intArray[1][j].from_north);
          mkConnection(intArray[i][j].to_east,intArray[i][j+1].from_west);
          mkConnection(intArray[i][j].to_sout,intArray[i+1][j].from_north);
        end
      end
     /* ============================================================================= */
  endmodule

endpackage
