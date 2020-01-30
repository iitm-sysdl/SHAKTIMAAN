package systolic_template;

	import systolic::*;
	import systolic_top::*;

	(*synthesize*)
	module mksystolic_array(Ifc_systolic#(4,16,16));
		let ifc();
		mksystolic inst(ifc);
		return (ifc);
	endmodule

	(*synthesize*)
	module mksystolic_top(Ifc_systolic_top_axi4#(32,64,0,2,4,16,1024,4,1024,2,16));
		let ifc();
		mksystolic_top_axi4 inst(ifc);
		return (ifc);
	endmodule

endpackage
