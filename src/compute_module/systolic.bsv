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
  import  intMul_WS::*;
  import  Connectable ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;

	/*West-East connection, moves input values*/
  interface Ifc_RFIFO_Connections#(numeric type in_width);
    method Action send_rowbuf_value(Maybe#(Bit#(in_width)) value); 
  endinterface

	/*North-south connection, moves weights and partial sums*/
  interface Ifc_CFIFO_Connections#(numeric type in_width, numeric type out_width);
    method Action send_colbuf_value(Tuple4#(Maybe#(Bit#(in_width)), Bit#(out_width), Bit#(8),Bit#(2)) value);
    method Action send_acc_value(Bit#(out_width) accinput);
    method ActionValue#(Bit#(out_width)) send_accumbuf_value;
  endinterface

  interface Ifc_systolic#(numeric type nRow, numeric type nCol,
                          numeric type in_width, numeric type out_width);
    interface Vector#(nRow, Ifc_RFIFO_Connections#(in_width)) rfifo;
    interface Vector#(nCol, Ifc_CFIFO_Connections#(in_width, out_width)) cfifo;
  endinterface

  module mksystolic(Ifc_systolic#(nRow,nCol,in_width,out_width))
      provisos (
                 Add#(a__, in_width, out_width)
               );
      let vnRow = valueOf(nRow);
      let vnCol = valueOf(nCol);
      
      Ifc_intMul_WS#(in_width, out_width) intArray[vnRow][vnCol];
      for(Integer i = 0; i < vnRow; i=i+1) begin
        for(Integer j = 0; j < vnCol; j=j+1) begin
          intArray[i][j] <- mkintMulWS(fromInteger(i),fromInteger(j), vnRow-i);
        end
      end
      
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
          mkConnection(intArray[i][j].send_acc_to_south, intArray[i+1][j].acc_from_north);
        end
      end

				Vector#(nRow, Ifc_RFIFO_Connections#(in_width)) vec_rfifo_ifc;
        Vector#(nCol, Ifc_CFIFO_Connections#(in_width, out_width)) vec_cfifo_ifc;

        for(Integer i = 0; i < vnRow; i=i+1) begin
          vec_rfifo_ifc[i] = (
            interface Ifc_RFIFO_Connections;
              method Action send_rowbuf_value(Maybe#(Bit#(in_width)) value);
                  intArray[i][0].from_west.put(value);
              endmethod
            endinterface
          );
        end

        for(Integer i = 0; i < vnCol; i=i+1) begin
          vec_cfifo_ifc[i] = (
             interface Ifc_CFIFO_Connections;
               method Action send_colbuf_value(Tuple4#(Maybe#(Bit#(in_width)), Bit#(out_width), Bit#(8),Bit#(2)) value);
                  match {.mulinput,.accinput,.counter,.val} = value;
                  intArray[0][i].from_north.put(tuple3(mulinput,counter,val));
               endmethod

               method Action send_acc_value(Bit#(out_width) accinput);
                  intArray[0][i].acc_from_north.put(accinput);
                endmethod

                method ActionValue#(Bit#(out_width)) send_accumbuf_value;
                  let x <- intArray[vnRow-1][i].send_acc_to_south.get;
                  return x; 
                endmethod
              endinterface
          );
        end

        interface rfifo = vec_rfifo_ifc;
        interface cfifo = vec_cfifo_ifc;
  endmodule

endpackage
