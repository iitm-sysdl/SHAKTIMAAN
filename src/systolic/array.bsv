package array;
	import pe::*;
	import GetPut::*;
	import Vector::*;
	import DReg::*;
	import ConfigReg::*;
	import Connectable::*;

	interface Ifc_array#(numeric type nRow, numeric type nCol, numeric type in_width, numeric type out_width);
		interface Vector#(nRow, Put#(Bit#(in_width))) ifc_put_inp;
		interface Vector#(nCol, Get#(Bit#(out_width))) ifc_get_out;
		interface Vector#(nCol, Put#(Tuple2#(Bit#(in_width), Int#(8)))) ifc_put_wgt;
	endinterface

	(*synthesize*)
  module mk_arr(Ifc_array#(4, 4, 16, 32));
    Ifc_array#(4, 4, 16, 32) inst1 <- mk_array;
    return (inst1);
  endmodule

	module mk_array(Ifc_array#(nRow, nCol, in_width, out_width))
		provisos(Add#(a__, in_width, out_width));
		
		let vnRow = valueOf(nRow);
		let vnCol = valueOf(nCol);

		Ifc_pe#(in_width, out_width) grid [vnRow][vnCol];
		for(Integer i=0; i<vnRow; i=i+1)begin
			for(Integer j=0; j<vnCol; j=j+1)begin
				grid[i][j] <- mk_pe(fromInteger(i), fromInteger(j), vnRow-1-i);
			end
		end

		for(Integer i=0; i<vnRow-1; i=i+1)begin
			for(Integer j=0; j<vnCol; j=j+1)begin
				mkConnection(grid[i][j].ifc_get_wgt, grid[i+1][j].ifc_put_wgt);
			end
		end

		for(Integer i=0; i<vnRow; i=i+1)begin
			for(Integer j=0; j<vnCol-1; j=j+1)begin
				mkConnection(grid[i][j].ifc_get_inp, grid[i][j+1].ifc_put_inp);
			end
		end

		Reg#(Bit#(out_width)) accs [2*vnRow-1][vnCol];
		for(Integer i=0; i<vnRow*2-1; i=i+1)begin
			for(Integer j=0; j<vnCol; j=j+1)begin
				accs[i][j] <- mkDReg(0);
			end
		end

		for(Integer i=0; i<vnRow; i=i+1)begin
			for(Integer j=0; j<vnCol; j=j+1)begin
				rule rl_level_1;
					let val <- grid[i][j].ifc_get_out.get();
					accs[i][j] <= val;
				endrule
			end
		end

		Integer base;
		for(Integer i=vnRow/2, base=0; i>=1; base=base+2*i, i=i/2)begin
			for(Integer k=0; k<i; k=k+1)begin
				for(Integer j=0; j<vnCol; j=j+1)begin
					rule rl_lvls;
						let val1 = accs[base+2*k+0][j];
						let val2 = accs[base+2*k+1][j];
						accs[base+k][j] <= val1 + val2;
					endrule
				end
			end
		end

		Vector#(nRow, Put#(Bit#(in_width))) v_ifc_put_inp;
		Vector#(nCol, Get#(Bit#(out_width))) v_ifc_get_out;
		Vector#(nCol, Put#(Tuple2#(Bit#(in_width), Int#(8)))) v_ifc_put_wgt;

		for(Integer i=0; i<vnRow; i=i+1)begin
			v_ifc_put_inp[i] = grid[i][0].ifc_get_inp;
		end

		for(Integer i=0; i<vnCol; i=i+1)begin
			v_ifc_put_wgt[i] = grid[0][i].ifc_get_wgt;
			v_ifc_get_out[i] = (interface Get;
														method ActionValue#(Bit#(out_width)) get;
															return accs[2*vnRow-3][i];
														endmethod
												 endinterface);
		end

		interface ifc_put_inp = v_ifc_put_inp;
		interface ifc_get_out = v_ifc_get_out;
		interface ifc_put_wgt = v_ifc_put_wgt;
	endmodule
endpackage
