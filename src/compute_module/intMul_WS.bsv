/* 
Copyright (c) 2013, IIT Madras All rights reserved.

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

package intMul_WS;
  import Vector::*;
  import LFSR::*;
  //import functions::*;
  import GetPut::*;
  import ConfigReg::*;
  import DReg::*;

  interface Ifc_intMul_WS#(numeric type in_width, numeric type out_width);
    interface Put#(Tuple3#((Maybe#(Bit#(in_width))),Bit#(8),Bit#(2))) from_north; //What is Bit#(8)?
    interface Put#(Maybe#(Bit#(in_width))) from_west;
    interface Put#(Bit#(out_width))  acc_from_north;
    interface Get#(Bit#(out_width))  send_acc_to_south;  
    interface Get#(Tuple3#(Maybe#(Bit#(in_width)),Bit#(8),Bit#(2))) to_south;
    interface Get#(Maybe#(Bit#(in_width))) to_east;
  endinterface

//(*synthesize*)
//  module mkmac_tb(Ifc_intMul_WS#(8, 16));
//    Ifc_intMul_WS#(8) inst1 <- mkintMulWS(0, 0, 0);
//    return (inst1);
//  endmodule
 
//  (*mutually_exclusive="mult_add_phase, from_west"*)
  module mkintMulWS#(Int#(8) row, Int#(8) col, parameter Integer coord)(Ifc_intMul_WS#(in_width, out_width))
    provisos(
             Add#(a__, in_width, out_width)
            );

  Reg#(Maybe#(Bit#(in_width)))     rg_north         <- mkConfigReg(tagged Invalid);
  Reg#(Bool)                     weight_valid     <- mkReg(True);
  Reg#(Maybe#(Bit#(in_width)))     rg_west          <- mkConfigReg(tagged Invalid);
  Reg#(Bit#(2))                  rg_bitWidth      <- mkReg(0);
  Reg#(Maybe#(Bit#(out_width)))   rg_input_acc        <- mkConfigReg(tagged Invalid);
  Wire#(Bit#(out_width))           acc_output       <- mkWire();
  Reg#(Bit#(8))                  rg_coord         <- mkReg(fromInteger(coord));//Stores the y-index of the PE, delimits the flow of weights.
  Reg#(Bit#(8))                  rg_counter       <- mkReg(0);
  Reg#(Bool)                     rg_flow_ctrl     <- mkDReg(False);
  Reg#(Bool)                     rg_hor_flow_ctrl <- mkDReg(False);
  Reg#(Bool)                     rg_acc_flow_ctrl <- mkDReg(False);

  Bool check = (rg_counter >= rg_coord);

  rule mult_add_phase(rg_north matches tagged Valid .north &&& rg_west matches tagged Valid .west
    &&& rg_input_acc matches tagged Valid .input_acc);
    Bit#(1) pp_sign[4];
    Bit#(out_width) output_mul = extend(unpack(north))*extend(unpack(west)); 
    acc_output <= pack(output_mul + unpack(input_acc));
    if(col==0)begin
			$display($time,"MulAdd[%d][%d]: acc: %d input: %d weight: %d output_mul:%d \n",row,col,input_acc,west,north,output_mul);
		end
    //$display($time,"\t input_acc: %d",input_acc);
    rg_west <= tagged Invalid;
    rg_acc_flow_ctrl <= True;
  endrule

  interface Put from_north;
    method Action put(Tuple3#(Maybe#(Bit#(in_width)),Bit#(8),Bit#(2)) inp);
      rg_north     <= tpl_1(inp);
      rg_counter   <= tpl_2(inp);
      rg_bitWidth  <= tpl_3(inp);
      rg_flow_ctrl <= True;
      Bit#(in_width) x = validValue(tpl_1(inp));
			if(col==0)begin
				$display($time,"N-> Systolic[%h][%h]: Receiving Weight: %d coord:%h rg_coord: %h",row,col,x,tpl_2(inp), rg_coord); 
			end
    endmethod
  endinterface

  interface Put acc_from_north;
    method Action put(Bit#(out_width) acc);
      //$display($time,"N-> Systolic[%d][%d] Receiving acc_input: %d",row,col,acc);
      rg_input_acc <= tagged Valid acc;
    endmethod
  endinterface

  interface Get send_acc_to_south;
    method ActionValue#(Bit#(out_width)) get;
      //$display($time,"->S Systolic[%d][%d] Sending acc_output: %d",row,col,acc_output); 
      return acc_output;
    endmethod
  endinterface

  interface Put from_west;
    method Action put(Maybe#(Bit#(in_width)) rowW);
      //$display($time,"W-> Systolic[%d][%d] Receiving Valid: %d Value from West %d",row,col,isValid(rowW),fromMaybe(?,rowW));
      rg_west <= rowW;
      rg_hor_flow_ctrl <= True;
    endmethod
  endinterface
    
  interface Get to_east;
    method ActionValue#(Maybe#(Bit#(in_width))) get if(rg_hor_flow_ctrl && isValid(rg_west));
      //$display($time,"->E Systolic[%d][%d] Sending Valid: %d Value %d to East Systolic[%d][%d]",row,col,isValid(rg_west),fromMaybe(?,rg_west),row,col+1);
      return rg_west;
    endmethod
  endinterface

  interface Get to_south;
    method ActionValue#(Tuple3#(Maybe#(Bit#(in_width)),Bit#(8),Bit#(2))) get if(!check && rg_flow_ctrl);
      let send = tuple3(rg_north,rg_counter,rg_bitWidth);
      Bit#(in_width) x = fromMaybe(0,rg_north);
      //$display($time,"->S Systolic[%h][%h]: Sending weight below: %d coord:%h",row,col,x,rg_counter);
      return send;
    endmethod
  endinterface 

  endmodule

endpackage
