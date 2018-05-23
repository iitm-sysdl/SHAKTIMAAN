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

Author: Vinod G
Email id: g.vinod1993@gmail.com
Details: Runtime Configurable 16-bit Multiplier

Verification Status: Unverified
FPGA/ASIC Synthesis Status: Not Synthesized Yet

//TODO
Add Specific logic to flush out Accumulator when the value in Systolic is taken out
--------------------------------------------------------------------------------------------------
*/

package intMul;
  import Vector::*;
  interface Ifc_intMul;
    method Action from_north(Bit#(16) col, Bit#(2) bitWidth); 
    //bitWidth: 00 - 4-bit, 01 - 8-bit, 10 - 16-bit MAC
    method Action from_west(Bit#(16) row);
    method Maybe#(Bit#(16)) to_south;
    method Maybe#(Bit#(16)) to_east;
    method ActionValue#(Bit#(32)) acc_output;
  endinterface

  module mkintMul(Ifc_intMul);
    Reg#(Bit#(32))          rg_acc       <- mkReg(0);
    Reg#(Maybe#(Bit#(16)))  rg_north     <- mkReg(tagged Invalid);
    Reg#(Maybe#(Bit#(16)))  rg_west      <- mkReg(tagged Invalid);
    Reg#(Bit#(2))           rg_bitWidth  <- mkReg(0);


    //TODO
    //Should try out a Structural Verilog Coding Style to see if it helps in Synthesis 
    rule mult_add_phase(rg_north matches tagged Valid .north &&& rg_west matches tagged Valid .west);
      //Vector#(4,Int#(4))             vec_north     <- newVector;
      //Vector#(4,Int#(4))             vec_west      <- newVector;
      //Vector#(4,Int#(4))             vec_acc       <- newVector;
      //Vector#(4, Vector#(4,Int#(8))) vec_partial   <- newVector;
      Int#(4) vec_north[4];
      Int#(4) vec_west[4];
      Int#(4) vec_acc[4]; 
      Int#(8) vec_partial[4][4];
      Int#(16) partial_output_vector1=0;
      Int#(16) partial_output_vector2=0;
      Int#(32) output_vector = 0;
      $display("\t North : %b \n \t West: %b \n",north,west);
      for(Integer i = 0; i < 16; i=i+4) begin
        vec_north[i/4] = unpack(north[i+3:i]);
        vec_west[i/4]  = unpack(west[i+3:i]);
        vec_acc[i/4]   = unpack(rg_acc[i+3:i]);
      end
     
      //Amassing all the Partial Products?
      //Scheme is to store all partial products in local variables and do something?
      for(Integer i = 0; i < 4; i=i+1) begin
        for(Integer j = 0; j < 4 ; j = j+1) begin
          vec_partial[i][j] = extend(vec_west[i])*extend(vec_north[j]);  //4-bit partial products
          //Will this extend infer an 8-bit mul?
          //$display(" vec_west[%d]: %b vec_north[%d]: %b Product %b",i,vec_west[i],j,vec_north[j],vec_partial[i][j]);
        end
      end
      //8-bit Shifts!
      //Is this optimal? I don't think so!!!
      Integer k = 0;  
      for(Integer i=0; i<2; i=i+1)begin
        for(Integer j=0; j<2;j=j+1) begin
          partial_output_vector1 = partial_output_vector1 + extend(vec_partial[i][j] <<
          4*k);
          partial_output_vector2 = partial_output_vector2 + extend(vec_partial[i+2][j+2] <<
          4*k);
          k=k+1;
          $display("\t p1: %b, p2: %b, k: %d",partial_output_vector1,partial_output_vector2,k);
        end
      end

      //16-bit shifts
      for(Integer i=0; i<4; i=i+1)begin
        for(Integer j=0; j<4; j=j+1)begin
          output_vector = output_vector + extend(vec_partial[i][j] << (16*i+4*j));
        end
      end
      //Simplistic Case when the output required is 4 4-bit numbers
      if(rg_bitWidth == 2'b00) begin
        output_vector = unpack({ pack(vec_partial[3][3]+extend(vec_acc[3])),
                          pack(vec_partial[2][2]+extend(vec_acc[2])),
                          pack(vec_partial[1][1]+extend(vec_acc[1])),
                          pack(vec_partial[0][0]+extend(vec_acc[0]))
                        });
      end
      else if(rg_bitWidth == 2'b01) begin  //2 8-bit numbers
        output_vector = unpack({pack(partial_output_vector1),pack(partial_output_vector2)});
      end
      rg_acc <= pack(output_vector);
      rg_north <= tagged Invalid;
      rg_west  <= tagged Invalid;
      $display($time,"\t The output is : %b",pack(output_vector));
    endrule

    method Action from_north(Bit#(16) col, Bit#(2) bitWidth);
      rg_north <= tagged Valid col;
      rg_bitWidth <= bitWidth;
    endmethod
    
    method Action from_west(Bit#(16) row);
      rg_west <= tagged Valid row;
    endmethod

    method Maybe#(Bit#(16)) to_east;
      return rg_west;
    endmethod

    method Maybe#(Bit#(16)) to_south;
      return rg_north;
    endmethod

    //This is still a fanout Signal right??!!!!
    method ActionValue#(Bit#(32)) acc_output;
      rg_acc <= 0;
      return rg_acc;
    endmethod
  endmodule

  module mkTb(Empty);
    Ifc_intMul intMul <- mkintMul();
    Reg#(int) cnt <- mkReg(0);

    rule start(cnt == 0);
      intMul.from_north(16'b0101010101010101, 2'b10);
      intMul.from_west(16'b1010101010101010);
      $display($time," \t Sending Inputs to Multiplier Unit");
      cnt <= cnt+1;
    endrule

    rule next(cnt == 1);
      cnt <= cnt+1;
    endrule

    rule nextnext (cnt == 2);
        let x <- intMul.acc_output();
        $display($time," X is : %b",x);
        $finish(0);
    endrule


  endmodule


endpackage
