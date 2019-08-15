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
  import functions::*;
  import GetPut::*;
  import ConfigReg::*;
  import DReg::*;

  interface Ifc_intMul_WS#(numeric type bWidth);
    interface Put#(Tuple3#((Maybe#(Bit#(bWidth))),Bit#(8),Bit#(2))) from_north; //What is Bit#(8)?
    interface Put#(Maybe#(Bit#(bWidth))) from_west;
    interface Put#(Bit#(TMul#(2,bWidth)))  acc_from_north;
    interface Get#(Bit#(TMul#(2,bWidth)))  send_acc_to_south;  
    interface Get#(Tuple3#(Maybe#(Bit#(bWidth)),Bit#(8),Bit#(2))) to_south;
    interface Get#(Maybe#(Bit#(bWidth))) to_east;
  endinterface

//  (*mutually_exclusive="mult_add_phase, from_west"*)
  module mkintMulWS#(Int#(8) row, Int#(8) col, parameter Integer coord)(Ifc_intMul_WS#(bWidth))
    provisos(
             Mul#(bWidth, 2, twbWidth),
             Add#(a__, bWidth, twbWidth)
            );

  Reg#(Maybe#(Bit#(bWidth)))     rg_north         <- mkConfigReg(tagged Invalid);
  Reg#(Bool)                     weight_valid     <- mkReg(True);
  Reg#(Maybe#(Bit#(bWidth)))     rg_west          <- mkConfigReg(tagged Invalid);
  Reg#(Bit#(2))                  rg_bitWidth      <- mkReg(0);
  Reg#(Bit#(twbWidth))            input_acc        <- mkConfigReg(0);
  Wire#(Bit#(twbWidth))           acc_output       <- mkWire();
  Reg#(Bit#(8))                  rg_coord         <- mkReg(fromInteger(coord));//Stores the y-index of the PE, delimits the flow of weights.
  Reg#(Bit#(8))                  rg_counter       <- mkReg(0);
  Reg#(Bool)                     rg_flow_ctrl     <- mkDReg(False);
  Reg#(Bool)                     rg_hor_flow_ctrl <- mkDReg(False);
  Reg#(Bool)                     rg_acc_flow_ctrl <- mkDReg(False);

  Bool check = (rg_counter >= rg_coord);
   

  rule mult_add_phase(rg_north matches tagged Valid .north &&& rg_west matches tagged Valid .west);
    Bit#(1) pp_sign[4];
    Int#(twbWidth) output_mul = extend(unpack(north))*extend(unpack(west)); 
    acc_output <= pack(output_mul + unpack(input_acc));
    $display($time,"MulAdd[%d][%d]: acc: %d input: %d weight: %d output_mul:%d \n",row,col,input_acc,west,north,output_mul);
    //$display($time,"\t input_acc: %d",input_acc);
    rg_west <= tagged Invalid;
    rg_acc_flow_ctrl <= True;
  endrule

  interface Put from_north;
    method Action put(Tuple3#(Maybe#(Bit#(bWidth)),Bit#(8),Bit#(2)) inp);
      rg_north     <= tpl_1(inp);
      rg_counter   <= tpl_2(inp);
      rg_bitWidth  <= tpl_3(inp);
      rg_flow_ctrl <= True;
      Bit#(bWidth) x = fromMaybe(0,tpl_1(inp));
      //$display($time,"N-> Systolic[%h][%h]: Receiving Weight: %d coord:%h rg_coord: %h",row,col,x,tpl_2(inp), rg_coord); 
    endmethod
  endinterface

  interface Put acc_from_north;
    method Action put(Bit#(twbWidth) acc);
      //$display($time,"N-> Systolic[%d][%d] Receiving acc_input: %d",row,col,acc);
      input_acc <= acc;
    endmethod
  endinterface

  interface Get send_acc_to_south;
    method ActionValue#(Bit#(twbWidth)) get;
      //$display($time,"->S Systolic[%d][%d] Sending acc_output: %d",row,col,acc_output); 
      return acc_output;
    endmethod
  endinterface

  interface Put from_west;
    method Action put(Maybe#(Bit#(bWidth)) rowW);
      //$display($time,"W-> Systolic[%d][%d] Receiving Valid: %d Value from West %d",row,col,isValid(rowW),fromMaybe(?,rowW));
      rg_west <= rowW;
      rg_hor_flow_ctrl <= True;
    endmethod
  endinterface
    
  interface Get to_east;
    method ActionValue#(Maybe#(Bit#(bWidth))) get if(rg_hor_flow_ctrl && isValid(rg_west));
      //$display($time,"->E Systolic[%d][%d] Sending Valid: %d Value %d to East Systolic[%d][%d]",row,col,isValid(rg_west),fromMaybe(?,rg_west),row,col+1);
      return rg_west;
    endmethod
  endinterface

  interface Get to_south;
    method ActionValue#(Tuple3#(Maybe#(Bit#(bWidth)),Bit#(8),Bit#(2))) get if(!check && rg_flow_ctrl);
      let send = tuple3(rg_north,rg_counter,rg_bitWidth);
      Bit#(bWidth) x = fromMaybe(0,rg_north);
      //$display($time,"->S Systolic[%h][%h]: Sending weight below: %d coord:%h",row,col,x,rg_counter);
      return send;
    endmethod
  endinterface 

  endmodule


endpackage
