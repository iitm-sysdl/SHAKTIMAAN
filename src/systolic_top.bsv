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
Details:

--------------------------------------------------------------------------------------------------
*/
package systolic_top;
  import  AXI4_Types  ::*;
  import  AXI4_Fabric ::*;
  import  Connectable ::*;
  import  systolic ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;

  interface Ifc_systolic_top#(numeric type nRow, numeric type nCol, 
                              numeric type addr, numeric type data,
                              numeric type nFEntries, numeric type mulWidth);
    interface AXI4_Slave_IFC#(addr,data,0) slave_systolic; //32-bit address? 4 16-bits?
  endinterface
 
    (*synthesize*)
    module mksystolic3(Ifc_systolic_top#(3,3,32,16,2,16));
        let ifc();
        mksystolic_top inst(ifc);
        return (ifc);
    endmodule

  module mksystolic_top(Ifc_systolic_top#(nRow,nCol,addr,data,nFEntries,mulWidth))
    provisos(
             Add#(a__,2,nRow),
             Add#(b__,2,nCol),
             Add#(d__,16,mulWidth),  //Change every 16 with MulWidth
             Add#(mulWidth,2,mulWidth2)
            );
    let vnFEntries = valueOf(nFEntries);
    let vnRow = valueOf(nRow);
    let vnCol = valueOf(nCol);

    Ifc_systolic#(nRow,nCol,mulWidth)                systolic_array <- mksystolic;  
    Vector#(nRow, FIFOF#(Bit#(16)))                  rowBuf    <- replicateM(mkSizedFIFOF(vnFEntries));
    Vector#(nCol, FIFOF#(Tuple2#(Bit#(16),Bit#(2)))) colBuf    <- replicateM(mkSizedFIFOF(vnFEntries));


     /* =================== Rules to Connect Row Buffers to Arrays ================== */
      for(Integer i = 0; i < vnRow; i=i+1) begin
        rule send_row_buf_value;
          Maybe#(Bit#(16)) mval = tagged Valid rowBuf[i].first;
          rowBuf[i].deq;
          systolic_array.rfifo[i].send_rowbuf_value(mval);
        endrule
      end
    /* ============================================================================== */

    /* ==================== Rules to Connect Col Buffers to Arrays  ================= */
      for(Integer i = 0; i < vnCol; i=i+1) begin
        rule send_col_buf_value;
          let val = tagged Valid colBuf[i].first;
          colBuf[i].deq;
          systolic_array.cfifo[i].send_colbuf_value(val);
        endrule
      end
    /* ============================================================================== */

    /* ================== Logic to either fetch from Bus or GBuffer ================ */


  endmodule


endpackage



