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

	interface Ifc_row_connections#(numeric type in_width);
		interface Put#(Bit#(in_width)) subifc_put_inp;
  endinterface

  interface Ifc_col_connections#(numeric type in_width, numeric type out_width);
    interface Put#(Tuple2#(Bit#(in_width), Bit#(8))) subifc_put_wgt;
		interface Put#(Bit#(out_width)) subifc_put_acc;
		interface Get#(Bit#(out_width)) subifc_get_acc;
  endinterface

  interface Ifc_systolic#(numeric type nRow, numeric type nCol, numeric type in_width, numeric type out_width);
    interface Vector#(nRow, Ifc_row_connections#(in_width)) subifc_rows;
    interface Vector#(nCol, Ifc_col_connections#(in_width, out_width)) subifc_cols;
  endinterface

  (*synthesize*)
  module mksystolic2(Ifc_systolic#(8,8,16, 32));
      let ifc();
      mksystolic inst(ifc);
      return (ifc);
  endmodule

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
      
      for(Integer i = 0; i < vnRow; i=i+1) begin
        for(Integer j = 0; j < vnCol-1; j=j+1) begin
          mkConnection(intArray[i][j].subifc_get_inp, intArray[i][j+1].subifc_put_inp);
        end
      end
      
      for(Integer i = 0; i < vnRow-1; i=i+1) begin
        for(Integer j = 0; j < vnCol; j=j+1) begin
          mkConnection(intArray[i][j].subifc_get_wgt, intArray[i+1][j].subifc_put_wgt);
					mkConnection(intArray[i][j].subifc_get_acc, intArray[i+1][j].subifc_put_acc);
        end
      end

      Vector#(nRow, Ifc_row_connections#(in_width)) vec_row_ifc;
      Vector#(nCol, Ifc_col_connections#(in_width, out_width)) vec_col_ifc;

      for(Integer i = 0; i < vnRow; i=i+1) begin
        vec_row_ifc[i] = (
          interface Ifc_row_connections;
						interface Put subifc_put_inp = intArray[i][0].subifc_put_inp;
				  endinterface
        );
      end

      for(Integer i = 0; i < vnCol; i=i+1) begin
        vec_col_ifc[i] = (
					interface Ifc_col_connections;
						interface subifc_put_wgt = intArray[0][i].subifc_put_wgt;
						interface subifc_put_acc = intArray[0][i].subifc_put_acc;
						interface subifc_get_acc = intArray[vnRow-1][i].subifc_get_acc;
          endinterface
        );
      end

      interface subifc_rows = vec_row_ifc;
      interface subifc_cols = vec_col_ifc;

  endmodule

endpackage
