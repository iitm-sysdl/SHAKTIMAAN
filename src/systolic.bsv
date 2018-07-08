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
  import  intMul_WS::*;
  import  Connectable ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;
	`include "defined_parameters.bsv"
	import defined_types::*;

  interface Ifc_RFIFO_Connections;
    method Action send_rowbuf_value(Maybe#(Bit#(16)) value); 
  endinterface

  interface Ifc_CFIFO_Connections;
    method Action send_colbuf_value(Tuple3#(Maybe#(Bit#(16)),Bit#(8),Bit#(2)) value);
  endinterface

  interface Ifc_systolic#(numeric type nRow, numeric type nCol,  
                          numeric type mulWidth);
    interface Vector#(nRow, Ifc_RFIFO_Connections) rfifo;
    interface Vector#(nCol, Ifc_CFIFO_Connections) cfifo;
  endinterface

  //(*synthesize*)
  //module mksystolic2(Ifc_systolic#(2,2,16));
  //    let ifc();
  //    mksystolic inst(ifc);
  //    return (ifc);
  //endmodule

  module mksystolic(Ifc_systolic#(nRow,nCol,mulWidth))
      provisos (
               );
      let vnRow = valueOf(nRow);
      let vnCol = valueOf(nCol);
      //let vnFEntries = valueOf(nFEntries);
      
      Ifc_intMul_WS intArray[vnRow][vnCol];
      for(Integer i = 0; i < vnRow; i=i+1) begin
        for(Integer j = 0; j < vnCol; j=j+1) begin
          intArray[i][j] <- mkintMulWS(fromInteger(i),fromInteger(j), vnRow-i+1);
        end
      end
      

      /* ==================== Systolic Array Connections ======================*/

      //West->East Connections
      for(Integer i = 0; i < vnRow; i=i+1) begin
        for(Integer j = 0; j < vnCol-1; j=j+1) begin
          mkConnection(intArray[i][j].to_east, intArray[i][j+1].from_west);
        end
      end
      
      //North->South Connections
      for(Integer i = 0; i < vnRow-1; i=i+1) begin
        for(Integer j = 0; j < vnCol; j=j+1) begin
          mkConnection(intArray[i][j].to_south, intArray[i+1][j].from_north);
        end
      end
      /* ============================================================================= */

      /* =========== TODO Code to get the Accumulators out and send it to top ======== */
      /* ============================================================================= */

      
      /* ================= Making Interface Connections ==============================  */
      /* ============================================================================= */
        Vector#(nRow, Ifc_RFIFO_Connections) vec_rfifo_ifc;
        Vector#(nCol, Ifc_CFIFO_Connections) vec_cfifo_ifc;

        for(Integer i = 0; i < vnRow; i=i+1) begin
          vec_rfifo_ifc[i] = (
            interface Ifc_RFIFO_Connections;
              method Action send_rowbuf_value(Maybe#(Bit#(16)) value);
                  intArray[i][0].from_west.put(value);
              endmethod
            endinterface
          );
        end

        for(Integer i = 0; i < vnCol; i=i+1) begin
          vec_cfifo_ifc[i] = (
             interface Ifc_CFIFO_Connections;
               method Action send_colbuf_value(Tuple3#(Maybe#(Bit#(16)),Bit#(8),Bit#(2)) value);
                 //Should Decide where this Bit#(8) comes from!! For now Keeping it from Buf
                  intArray[0][i].from_north.put(value);
               endmethod
             endinterface
          );
        end

        interface rfifo = vec_rfifo_ifc;
        interface cfifo = vec_cfifo_ifc;
        /* ================================================================ */

  endmodule
endpackage
