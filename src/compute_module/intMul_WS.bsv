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
	import FIFOF::*;

  interface Ifc_intMul_WS#(numeric type in_width, numeric type out_width);
		/*doc:subifc: interface to receive weight from north*/
    interface Put#(Tuple2#((Bit#(in_width)), Bit#(8))) subifc_put_wgt;
		/*doc:subifc: interface to receive input from west*/
    interface Put#(Bit#(in_width)) subifc_put_inp;
		/*doc:subifc: interface to receive partial sum from north*/
    interface Put#(Bit#(out_width)) subifc_put_acc;
		/*doc:subifc: interface to send partial sum to south*/
    interface Get#(Bit#(out_width)) subifc_get_acc;
		/*doc:subifc: interface to send weight to south*/
    interface Get#(Tuple2#(Bit#(in_width), Bit#(8))) subifc_get_wgt;
		/*doc:subifc: interface to send input to east*/
    interface Get#(Bit#(in_width)) subifc_get_inp;
  endinterface
	

	interface RegMod#(type t);
		method ActionValue#(t) _read;
		method Action _write(t x);
	endinterface 

	//Custom Register type to solve the issue
	function RegMod#(Maybe#(Bit#(n))) configDReg(Reg#(Maybe#(Bit#(n))) r);
		return (interface RegMod;
					method ActionValue#(Maybe#(Bit#(n))) _read;
						r._write(tagged Invalid);
						return r._read;
					endmethod

					method Action _write(Maybe#(Bit#(n)) x);
						r._write(x);
					endmethod
				endinterface);
	endfunction 

(*synthesize*)
  module mkmac_tb(Ifc_intMul_WS#(8, 32));
    Ifc_intMul_WS#(8, 32) inst1 <- mkintMulWS(0, 0, 1);
    return (inst1);
  endmodule
 
  module mkintMulWS#(Int#(8) row, Int#(8) col, parameter Integer coord)(Ifc_intMul_WS#(in_width, out_width))
    provisos(
             Add#(a__, in_width, out_width)
            );

	/*doc:reg: these registers are used to hold weight, input and output values, respectively*/
  Reg#(Bit#(in_width)) rg_north <- mkReg(0);
	FIFOF#(Bit#(in_width)) ff_west  <- mkSizedFIFOF(2);
	FIFOF#(Bit#(out_width)) ff_input_acc <- mkSizedFIFOF(2);
	Wire#(Bit#(in_width)) wr_west <- mkWire(); 
  //Reg#(Maybe#(Bit#(in_width)))   rg_n             <- mkConfigReg(tagged Invalid);
  //RegMod#(Maybe#(Bit#(in_width)))        rg_north         =  configDReg(rg_n);
  //Reg#(Maybe#(Bit#(in_width)))   rg_w             <- mkConfigReg(tagged Invalid);
  //RegMod#(Maybe#(Bit#(in_width)))        rg_west          =  configDReg(rg_w);
  //Reg#(Maybe#(Bit#(out_width)))  rg_input_acc     <- mkConfigReg(tagged Invalid);
	/*doc:wire: this wire is used to send computed result to next PE*/
  Wire#(Bit#(out_width))         acc_output       <- mkWire();
	/*doc:reg: register to store the coordinate of the PE*/
  Reg#(Bit#(8))                  rg_coord         <- mkReg(fromInteger(coord));
	/*doc:reg: register to hold the coord till which the weight has to flow*/
	Reg#(Bit#(8))                  rg_counter       <- mkReg(0);
  Bool check = (rg_counter >= rg_coord);

	/*doc:rule: rule which performs MAC operation*/
  rule rl_mult_add_phase;
		let north = rg_north;
		let west  = ff_west.first;
		wr_west <= ff_west.first;
		ff_west.deq();
    let input_acc = ff_input_acc.first;
    ff_input_acc.deq;
    Bit#(out_width) output_mul = extend(north)*extend(west);
		output_mul = output_mul + unpack(input_acc);
    acc_output <= pack(output_mul);
  endrule
	
  interface Put subifc_put_wgt;
    method Action put(Tuple2#(Bit#(in_width),Bit#(8)) inp);
      rg_north <= tpl_1(inp);
      rg_counter   <= tpl_2(inp);
    endmethod
  endinterface

  interface Put subifc_put_acc;
    method Action put(Bit#(out_width) acc);
			ff_input_acc.enq(acc);
    endmethod
  endinterface

  interface Get subifc_get_acc;
    method ActionValue#(Bit#(out_width)) get;
			return acc_output;
    endmethod
  endinterface

  interface Put subifc_put_inp;
    method Action put(Bit#(in_width) rowW);
			ff_west.enq(rowW);
    endmethod
  endinterface
    
  interface Get subifc_get_inp;
    method ActionValue#(Bit#(in_width)) get;
      return wr_west;
    endmethod
  endinterface

  interface Get subifc_get_wgt;
    method ActionValue#(Tuple2#(Bit#(in_width), Bit#(8))) get if(!check);
      let send = tuple2(rg_north,rg_counter);
      return send;
    endmethod
  endinterface 

  endmodule

endpackage
