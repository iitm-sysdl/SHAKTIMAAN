package pe;

	import GetPut::*;
	import ConfigReg::*;

	interface Ifc_pe#(numeric type in_width, numeric type out_width);
		interface Put#(Tuple2#(Bit#(in_width), Int#(8))) ifc_get_wgt;
		interface Get#(Tuple2#(Bit#(in_width), Int#(8))) ifc_put_wgt;
		interface Get#(Bit#(out_width)) ifc_get_out;
		interface Put#(Bit#(in_width)) ifc_get_inp;
		interface Get#(Bit#(in_width)) ifc_put_inp;
	endinterface

	(*synthesize*)
  module mk_mac(Ifc_pe#(8, 16));
    Ifc_pe#(8, 16) inst1 <- mk_pe(0, 0, 1);
    return (inst1);
  endmodule
 
	module mk_pe#(Int#(8) row, Int#(8) col, parameter Integer coord)(Ifc_pe#(in_width, out_width))
		provisos(Add#(a__, in_width, out_width));

		Reg#(Maybe#(Bit#(in_width))) rg_wgt <- mkConfigReg(tagged Invalid);
		Reg#(Int#(8))								 rg_idx <- mkConfigReg(0);
		Reg#(Maybe#(Bit#(in_width))) rg_inp <- mkConfigReg(tagged Invalid);

		interface Get ifc_get_out;
			method ActionValue#(Bit#(out_width)) get if(rg_wgt matches tagged Valid .wgt &&& rg_inp matches tagged Valid .inp);
				Bit#(out_width) out = zeroExtend(wgt * inp);
				return out;
			endmethod
		endinterface

		interface Put ifc_get_wgt;
			method Action put(Tuple2#(Bit#(in_width), Int#(8)) val);
				let wgt = tpl_1(val);
				let idx = tpl_2(val);
				rg_wgt <= tagged Valid wgt;
				rg_idx <= idx;
			endmethod
		endinterface

		interface Get ifc_put_wgt;
			method ActionValue#(Tuple2#(Bit#(in_width), Int#(8))) get if(rg_idx < fromInteger(coord));
				return tuple2(validValue(rg_wgt), rg_idx);
			endmethod
		endinterface

		interface Put ifc_get_inp;
			method Action put(Bit#(in_width) val);
				rg_inp <= tagged Valid val;
			endmethod
		endinterface
		
		interface Get ifc_put_inp;
			method ActionValue#(Bit#(in_width)) get;
				return validValue(rg_inp);
			endmethod
		endinterface
	endmodule
endpackage
