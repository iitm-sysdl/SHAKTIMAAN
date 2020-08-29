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

Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/

/*doc:overview:
	The module contains implementation of a single Weight Stationary PE.
*/
package intMul_WS;
  import Vector::*;
  import LFSR::*;
  import GetPut::*;
  import ConfigReg::*;
  import DReg::*;

  interface Ifc_intMul_WS#(numeric type in_width, numeric type out_width);
		/*doc:subifc: interface to receive weight from north*/
    interface Put#(Tuple2#((Maybe#(Bit#(in_width))), Bit#(8))) subifc_put_wgt;
		/*doc:subifc: interface to receive input from west*/
    interface Put#(Maybe#(Bit#(in_width))) subifc_put_inp;
		/*doc:subifc: interface to receive partial sum from north*/
    interface Put#(Bit#(out_width)) subifc_put_acc;
		/*doc:subifc: interface to send partial sum to south*/
    interface Get#(Bit#(out_width)) subifc_get_acc;
		/*doc:subifc: interface to send weight to south*/
    interface Get#(Tuple2#(Maybe#(Bit#(in_width)), Bit#(8))) subifc_get_wgt;
		/*doc:subifc: interface to send input to east*/
    interface Get#(Maybe#(Bit#(in_width))) subifc_get_inp;
  endinterface

//(*synthesize*)
//  module mkmac_tb(Ifc_intMul_WS#(8, 16));
//    Ifc_intMul_WS#(8) inst1 <- mkintMulWS(0, 0, 0);
//    return (inst1);
//  endmodule
 
  module mkintMulWS#(Int#(8) row, Int#(8) col, parameter Integer coord)(Ifc_intMul_WS#(in_width, out_width))
    provisos(
             Add#(a__, in_width, out_width)
            );

	/*doc:reg: these registers are used to hold weight, input and output values, respectively*/
  Reg#(Maybe#(Bit#(in_width)))   rg_north         <- mkConfigReg(tagged Invalid);
  Reg#(Maybe#(Bit#(in_width)))   rg_west          <- mkConfigReg(tagged Invalid);
  Reg#(Maybe#(Bit#(out_width)))  rg_input_acc     <- mkConfigReg(tagged Invalid);
	/*doc:wire: this wire is used to send computed result to next PE*/
  Wire#(Bit#(out_width))         acc_output       <- mkWire();
	/*doc:reg: register to store the coordinate of the PE*/
  Reg#(Bit#(8))                  rg_coord         <- mkReg(fromInteger(coord));
	/*doc:reg: register to hold the coord till which the weight has to flow*/
	Reg#(Bit#(8))                  rg_counter       <- mkReg(0);
	/*doc:reg: registers to check flow of weights, inputs and output values, respectively*/
  Reg#(Bool)                     rg_flow_ctrl     <- mkDReg(False);
  Reg#(Bool)                     rg_hor_flow_ctrl <- mkDReg(False);
  Reg#(Bool)                     rg_acc_flow_ctrl <- mkDReg(False);

  Bool check = (rg_counter >= rg_coord);

	/*doc:rule: rule which performs MAC operation*/
  rule rl_mult_add_phase(rg_north matches tagged Valid .north &&&
												 rg_west matches tagged Valid .west &&&
												 rg_input_acc matches tagged Valid .input_acc);
    Bit#(out_width) output_mul = extend(unpack(north))*extend(unpack(west));
		output_mul = output_mul + unpack(input_acc);
    acc_output <= pack(output_mul);
    rg_acc_flow_ctrl <= True;
		if(row==15 && col==15)
			$display($time, "MAC: [%d][%d], %d, %d, %d, %d", row, col, north, west, input_acc, output_mul);
  endrule
	
	rule rl_valid(row==15 && col==15);
		$display($time, row, col, isValid(rg_north), isValid(rg_west), isValid(rg_input_acc));
	endrule
  interface Put subifc_put_wgt;
    method Action put(Tuple2#(Maybe#(Bit#(in_width)),Bit#(8)) inp);
      rg_north     <= tpl_1(inp);
      rg_counter   <= tpl_2(inp);
      rg_flow_ctrl <= True; 
    endmethod
  endinterface

  interface Put subifc_put_acc;
    method Action put(Bit#(out_width) acc);
      rg_input_acc <= tagged Valid acc;
    endmethod
  endinterface

  interface Get subifc_get_acc;
    method ActionValue#(Bit#(out_width)) get;
			if(row==15 && col==15)
      $display($time, "[%d][%d], output acc: %d", row, col, acc_output);
			return acc_output;
    endmethod
  endinterface

  interface Put subifc_put_inp;
    method Action put(Maybe#(Bit#(in_width)) rowW);
      rg_west <= rowW;
			if(row==15)
				$display($time, "sending value %d to col %d", validValue(rowW), col);
      rg_hor_flow_ctrl <= True;
    endmethod
  endinterface
    
  interface Get subifc_get_inp;
    method ActionValue#(Maybe#(Bit#(in_width))) get if(rg_hor_flow_ctrl && isValid(rg_west));
      return rg_west;
    endmethod
  endinterface

  interface Get subifc_get_wgt;
    method ActionValue#(Tuple2#(Maybe#(Bit#(in_width)), Bit#(8))) get if(!check && rg_flow_ctrl);
      let send = tuple2(rg_north, rg_counter);
      return send;
    endmethod
  endinterface 

  endmodule

endpackage
