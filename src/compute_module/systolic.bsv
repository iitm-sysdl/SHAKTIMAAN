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
  //import  intMul::*;
  import  intMul_WS::*;
  import  Connectable ::*;
  import  GetPut ::*;
  import  Vector ::*;
  import  FIFOF::*;
//	`include "defined_parameters.bsv"
//	import defined_types::*;

  interface Ifc_RFIFO_Connections#(numeric type in_width);
    method Action send_rowbuf_value(Maybe#(Bit#(in_width)) value); 
  endinterface

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

  //(*synthesize*)
  //module mksystolic2(Ifc_systolic#(2,2,16));
  //    let ifc();
  //    mksystolic inst(ifc);
  //    return (ifc);
  //endmodule

  module mksystolic(Ifc_systolic#(nRow,nCol,in_width,out_width))
      provisos (
                 Add#(a__, in_width, out_width)
               );
      let vnRow = valueOf(nRow);
      let vnCol = valueOf(nCol);
      //let vnFEntries = valueOf(nFEntries);
      
      Ifc_intMul_WS#(in_width, out_width) intArray[vnRow][vnCol];
      for(Integer i = 0; i < vnRow; i=i+1) begin
        for(Integer j = 0; j < vnCol; j=j+1) begin
          intArray[i][j] <- mkintMulWS(fromInteger(i),fromInteger(j), vnRow-i);
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
          mkConnection(intArray[i][j].send_acc_to_south, intArray[i+1][j].acc_from_north);
        end
      end
      /* ============================================================================= */

     
      /* ================= Making Interface Connections ==============================  */
      /* ============================================================================= */
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
                 //Should Decide where this Bit#(8) comes from!! For now Keeping it from Buf
                  match {.mulinput,.accinput,.counter,.val} = value;
                  intArray[0][i].from_north.put(tuple3(mulinput,counter,val));
                  //intArray[0][i].acc_from_north.put(accinput); //Put the value sent from the top!!
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

        /* ==================== Extract Accum Values ================================ */
        //Need some Control Flow to regulate the values, so that random values are prevented from
        //coming out


        interface rfifo = vec_rfifo_ifc;
        interface cfifo = vec_cfifo_ifc;
        /* ================================================================ */

  endmodule

  module mkTb_systolic_regress(Empty);

  endmodule


  //TODO: the following Tb has not been updated after changes to interfaces above.
  module mkTb_systolic(Empty);
    Ifc_systolic#(3,3,16) systolic_array <- mksystolic;  
    
    Reg#(Bit#(3)) rg_state <- mkReg(0);
    Reg#(Bit#(8)) rg_counter <- mkReg(1);
    Reg#(Maybe#(Bit#(16))) rg_weight_dummy <- mkReg(tagged Valid 'd45);
    Reg#(Bit#(16)) rg_act_dummy <- mkReg('d33);

    rule load_weights(rg_state == 0);
      Bit#(16) x = fromMaybe(0,rg_weight_dummy);
      $display($time,"\t TB: Sending Weight: %d co-ordinate : %d",x, rg_counter);
      if(rg_counter == 3) begin
        rg_weight_dummy <= tagged Invalid;
        rg_state <= 'd1;
        rg_counter <= 0;
      //  $finish(0);
      end
      else begin
        rg_weight_dummy <= tagged Valid (x+3);
        rg_counter <= rg_counter + 1;
        $display("\t TB: Tag Valid");
      end
      for(Integer i = 0; i < 3; i=i+1) begin
        systolic_array.cfifo[i].send_colbuf_value(tuple4(rg_weight_dummy,5,rg_counter,2'b00));
      end
    endrule

    rule send_acts(rg_state == 1);
       rg_counter <= rg_counter+1;
       rg_act_dummy <= rg_act_dummy + 3;
       for(Integer i = 0; i <3; i=i+1) begin
         systolic_array.rfifo[i].send_rowbuf_value(tagged Valid rg_act_dummy);
       end
       if(rg_counter == 'd10)
         $finish(0);
    endrule

    rule print_activations;
      for(Integer i = 0; i < 3; i=i+1) begin
        let x <- systolic_array.cfifo[i].send_accumbuf_value;
        $display($time,"\tColNo: %d val: %d\n", i,x);
      end
    endrule

  endmodule
endpackage
