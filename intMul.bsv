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

Verification Status: Verified with 10000 random inputs for each case

FPGA/ASIC Synthesis Status: 
FPGA results after verification: (AC701 - Artix 7)
LUT : 800 (Area Optimization Required)
FF  : 68
Worst Negative Slack: -1.416 (10ns Clock) (Timing Optimization Required)

Estimated FPGA Target:
LUT : 600
FF  : Same
WNS : 0 (10ns Clock)

Optimization Status: Design Unoptimized
Parameterization Status: Not Parameterized Yet

//TODO
Add Specific logic to flush out Accumulator when the value in Systolic is taken out
--------------------------------------------------------------------------------------------------
*/

package intMul;
  import Vector::*;
  import LFSR::*; 
  import GetPut::*;
  interface Ifc_intMul;
    interface Put#(Bit#(16)) from_north;
    interface Put#(Bit#(2)) bitWidth;
    //bitWidth: 00 - 4-bit, 01 - 8-bit, 10 - 16-bit MAC
    interface Put#(Bit#(16)) from_west;
    interface Get#(Maybe#(Bit#(16))) to_south;
    interface Get#(Maybe#(Bit#(16))) to_east;
    interface Get#(Bit#(2)) bitSouth;

    method ActionValue#(Bit#(32)) acc_output;  //This needs to be addressed sometime soon
  endinterface

  (*synthesize*)
  module mkintMul(Ifc_intMul);
    Reg#(Bit#(32))          rg_acc       <- mkReg(0);
    Reg#(Maybe#(Bit#(16)))  rg_north     <- mkReg(tagged Invalid);
    Reg#(Maybe#(Bit#(16)))  rg_west      <- mkReg(tagged Invalid);
    Reg#(Bit#(2))           rg_bitWidth  <- mkReg(0);


    //TODO
    //Should try out a Structural Verilog Coding Style to see if it helps in Synthesis 
    rule mult_add_phase(rg_north matches tagged Valid .north &&& rg_west matches tagged Valid .west);
      Int#(4) vec_north[4];
      Int#(4) vec_west[4];
      Int#(4) vec_acc[4]; 
      Int#(8) vec_partial[4][4];
      Int#(16) partial_output_vector1=0;
      Int#(16) partial_output_vector2=0;
      Int#(32) output_vector = 0;
      Bit#(16) north_full = north;
      Bit#(16) west_full = west;
      Bit#(4) par_north[4];
      Bit#(4) par_west[4];
      bit pp1_sign = north[7]^west[7];
      bit pp2_sign = north[15]^west[15];
      bit pp3_sign = north[11]^west[11];
      bit pp4_sign = north[3]^west[3];
      $display("\t North : %b \n \t West: %b \n",north,west);
      if(rg_bitWidth == 2'b00) begin
        for(Integer i = 0; i < 16; i=i+4) begin
          par_north[i/4] = north[i+3]==1? ~north[i+3:i]+1 : north[i+3:i];
          par_west[i/4]  = west[i+3] ==1? ~west[i+3:i]+1 : west[i+3:i];
        end
        north_full = {par_north[3],par_north[2],par_north[1],par_north[0]};
        west_full  = {par_west[3],par_west[2],par_west[1],par_west[0]};
      end
      else if(rg_bitWidth == 2'b01) begin
         Bit#(8) north_upper = north[15]==1? ~north[15:8]+1 : north[15:8];
         Bit#(8) north_lower = north[7]==1? ~north[7:0]+1 : north[7:0];
         Bit#(8) west_upper  = west[15]==1? ~west[15:8]+1 : west[15:8];
         Bit#(8) west_lower  = west[7]==1? ~west[7:0]+1 : west[7:0];
         north_full = {north_upper,north_lower};
         west_full  = {west_upper, west_lower};
      end
      else if (rg_bitWidth == 2'b10) begin
        north_full = north[15]==1? ~north+1 : north;
        west_full  = west[15]==1? ~west+1 : west;
      end
      $display("\t north: %b west: %b north_full: %b west_full:%b",north,west,north_full,west_full);
      for(Integer i = 0; i < 16; i=i+4) begin
        vec_north[i/4] = unpack(north_full[i+3:i]);
        vec_west[i/4]  = unpack(west_full[i+3:i]);
        vec_acc[i/4]   = unpack(rg_acc[i+3:i]);
      end
     
      //Amassing all the Partial Products?
      //Scheme is to store all partial products in local variables and do something?
      for(Integer i = 0; i < 4; i=i+1) begin
        for(Integer j = 0; j < 4 ; j = j+1) begin
          vec_partial[i][j] = zeroExtend(vec_west[i])*zeroExtend(vec_north[j]);  //4-bit partial products
          //Will this extend infer an 8-bit mul?
          $display(" vec_west[%d]: %b vec_north[%d]: %b Product %b",i,vec_west[i],j,vec_north[j],
          vec_partial[i][j]);
        end
      end
      //8-bit Shifts!
      //Is this optimal? I don't think so!!!
      for(Integer i=0; i<2; i=i+1)begin
        for(Integer j=0; j<2;j=j+1) begin
          partial_output_vector1 = partial_output_vector1 + (zeroExtend(vec_partial[i][j]) <<
          4*i+4*j);
          partial_output_vector2 = partial_output_vector2 + (zeroExtend(vec_partial[i+2][j+2]) <<
          4*i+4*j);
          $display("\t p1: %b, p2: %b",partial_output_vector1,partial_output_vector2);
          $display("\t vec_partial[%d][%d] : %b vec_partial[%d][%d] : %b",i,j,vec_partial[i][j],i,j
          ,vec_partial[i+2][j+2]);
        end
      end

      for(Integer i=0; i<4; i=i+1)begin
        for(Integer j=0; j<4; j=j+1)begin
          output_vector = output_vector + (zeroExtend(vec_partial[i][j]) << (4*i+4*j));
          $display("output_vector: %b vec_partial[i][j] %b", output_vector,vec_partial[i][j]);
        end
      end
      output_vector = pp2_sign == 1 ? ~output_vector+1 : output_vector;
      //Simplistic Case when the output required is 4 4-bit numbers
      Int#(8) inter[4];
      if(rg_bitWidth == 2'b00) begin
        for(Integer i = 3; i >=0; i=i-1)begin
          inter[i] = vec_partial[i][i] + extend(vec_acc[i]);
        end
        inter[3] = pp2_sign==1? ~inter[3]+1 : inter[3];
        inter[2] = pp3_sign==1? ~inter[2]+1 : inter[2];
        inter[1] = pp1_sign==1? ~inter[1]+1 : inter[1];
        inter[0] = pp4_sign==1? ~inter[0]+1 : inter[0];
        output_vector = unpack({ pack(inter[3]),pack(inter[2]),pack(inter[1]),pack(inter[0])});
      end
      else if(rg_bitWidth == 2'b01) begin  //2 8-bit numbers
        output_vector =
        unpack({pp2_sign==1?pack(~partial_output_vector2+1):pack(partial_output_vector2),
                pp1_sign==1?pack(~partial_output_vector1+1):pack(partial_output_vector1)});
      end
      rg_acc <= pack(output_vector);
      rg_north <= tagged Invalid;
      rg_west  <= tagged Invalid;
      $display($time,"\t The output is : %b",pack(output_vector));
    endrule

    interface Put from_north;
      method Action put(Bit#(16) col);
        rg_north <= tagged Valid col;
      endmethod
    endinterface

    interface Put bitWidth;
      method Action put(Bit#(2) bitWidth);
        rg_bitWidth <= bitWidth;
      endmethod
    endinterface
    
    interface Put from_west;
      method Action put(Bit#(16) row);
        rg_west <= tagged Valid row;
      endmethod
    endinterface
    
    interface Get to_east;
      method ActionValue#(Maybe#(Bit#(16))) get;
        return rg_west;
      endmethod
    endinterface

    interface Get to_south;
      method ActionValue#(Maybe#(Bit#(16))) get;
        return rg_north;
      endmethod
    endinterface

    interface Get bitSouth;
      method ActionValue#(Bit#(2)) get;
        return rg_bitWidth;
      endmethod
    endinterface

    //This is still a fanout Signal right??!!!!
    method ActionValue#(Bit#(32)) acc_output;
      rg_acc <= 0;
      return rg_acc;
    endmethod

  endmodule

  /*module mkTb(Empty);
    Ifc_intMul intMul <- mkintMul();
    Reg#(int) cnt <- mkReg(0);
    Reg#(int) count <- mkReg(0);
    LFSR#(Bit#(16))  input1 <- mkLFSR_16;
    LFSR#(Bit#(16))  input2 <- mkLFSR_16;
    Reg#(Int#(32)) rg_output <- mkReg(0);
		Reg#(Bool) starting <- mkReg(True) ;
  	
		rule rl_initial (starting);
      starting <= False;
      input1.seed('h11);
			input2.seed('h14);
  	endrule

    rule start(cnt == 0);
      Int#(16) a = unpack(input1.value);
      Int#(16) b = unpack(input2.value);
		  input1.next;	
			input2.next;
      intMul.from_north(pack(a), 2'b10);
      intMul.from_west(pack(b));

			Int#(32) inter = extend(a)*extend(b);
			rg_output <= inter;

 			//8-bit multiplications
			/*Int#(16) inter[2];
			inter[1] = extend(unpack(pack(a)[15:8]))*extend(unpack(pack(b)[15:8]));
			inter[0] = extend(unpack(pack(a)[7:0]))*extend(unpack(pack(b)[7:0]));
			rg_output <= unpack({pack(inter[1]),pack(inter[0])});*/
      
			//4-bit multiplications
			/*Int#(8) inter[4];
			inter[3] = extend(unpack(pack(a)[15:12]))*extend(unpack(pack(b)[15:12]));
			inter[2] = extend(unpack(pack(a)[11:8]))*extend(unpack(pack(b)[11:8]));
			inter[1] = extend(unpack(pack(a)[7:4]))*extend(unpack(pack(b)[7:4]));
			inter[0] = extend(unpack(pack(a)[3:0]))*extend(unpack(pack(b)[3:0]));
      rg_output <= unpack({pack(inter[3]),pack(inter[2]),pack(inter[1]),pack(inter[0])});*/
			
     /* $display($time," \t Sending Inputs to Multiplier Unit, a: %b b: %b",a,b);
      cnt <= cnt+1;
    endrule

    rule next(cnt == 1);
      cnt <= cnt+1;
    endrule

    rule nextnext (cnt == 2);
        let x <- intMul.acc_output();
        if(x!=pack(rg_output)) begin
          $display($time," Don't match X is : %b rg_output : %b",x, pack(rg_output));
          $finish(0);
        end
				else
					$display($time, "Vinod, They Match");
        cnt <= 0;
    endrule

    rule rl_count;
      count <= count+1;
      if(count=='d9999)
        $finish(0);
    endrule
  endmodule*/
endpackage
