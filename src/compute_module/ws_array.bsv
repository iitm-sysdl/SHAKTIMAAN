/* 
Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details: Configurable Systolic Array Unit
*/

package ws_array;
  import ws_pe::*;
  import Connectable::*;
  import GetPut::*;
  import Vector::*;
  import FIFOF::*;
	import TxRx::*;

	interface Ifc_row#(numeric type in_width);
		interface RXe#(Bit#(in_width)) rx_input;
	endinterface

	interface Ifc_col_in#(numeric type in_width, numeric type out_width);
		interface RXe#(Tuple2#(Bit#(in_width), Bit#(8))) rx_weight;
		interface RXe#(Bit#(out_width)) rx_output;
	endinterface

	interface Ifc_col_out#(numeric type out_width);
		interface TXe#(Bit#(out_width)) tx_output;
	endinterface

	interface Ifc_ws_array#(numeric type nRow, numeric type nCol, numeric type in_width, numeric type out_width);
    interface Vector#(nRow, Ifc_row#(in_width)) subifc_rows;
    interface Vector#(nCol, Ifc_col_in#(in_width, out_width)) subifc_cols_in;
		interface Vector#(nCol, Ifc_col_out#(out_width)) subifc_cols_out;
  endinterface

  (*synthesize*)
  module mksystolic2(Ifc_ws_array#(2,2,4,8));
      let ifc();
      mk_ws_array inst(ifc);
      return (ifc);
  endmodule

	module mk_ws_array(Ifc_ws_array#(nRow, nCol, in_width, out_width))
		provisos(Add#(a__, in_width, out_width));
		
		let vnRow = valueOf(nRow);
		let vnCol = valueOf(nCol);

		Ifc_ws_pe#(in_width, out_width) array[vnRow][vnCol];

		for(Integer i=0; i<vnRow; i=i+1)begin
			for(Integer j=0; j<vnCol; j=j+1)begin
				array[i][j] <- mk_ws_pe(fromInteger(i), fromInteger(j), vnRow-i);
			end
		end

		FIFOF#(Tuple2#(Bit#(in_width), Bit#(8))) ff_weight[vnRow-1][vnCol];
		FIFOF#(Bit#(out_width)) ff_output[vnRow-1][vnCol];

		FIFOF#(Bit#(in_width)) ff_input[vnRow][vnCol-1];

		for(Integer i=0; i<vnRow-1; i=i+1)begin
			for(Integer j=0; j<vnCol; j=j+1)begin
				
				ff_weight[i][j] <- mkFIFOF;
				mkChan(ff_weight[i][j], array[i][j].subifc_tx_stat, array[i+1][j].subifc_rx_stat);
				//mkConnection(ff_weight[i][j], array[i][j].subifc_tx_stat);
				//mkConnection(array[i+1][j].subifc_rx_stat, ff_weight[i][j]);

				ff_output[i][j] <- mkFIFOF;
				mkChan(ff_output[i][j], array[i][j].subifc_tx_output, array[i+1][j].subifc_rx_output);
				//mkConnection(ff_output[i][j], array[i][j].subifc_tx_output);
				//mkConnection(array[i+1][j].subifc_rx_output, ff_output[i][j]);
			end
		end

		for(Integer i=0; i<vnRow; i=i+1)begin
			for(Integer j=0; j<vnCol-1; j=j+1)begin

				ff_input[i][j] <- mkFIFOF;
				mkChan(ff_input[i][j], array[i][j].subifc_tx_input, array[i][j+1].subifc_rx_input);
				//mkConnection(ff_input[i][j], array[i][j].subifc_tx_input);
				//mkConnection(array[i][j+1].subifc_rx_input, ff_input[i][j]);
			end
		end

		Vector#(nRow, Ifc_row#(in_width)) vec_row_ifc;
		Vector#(nCol, Ifc_col_in#(in_width, out_width)) vec_col_in_ifc;
		Vector#(nCol, Ifc_col_out#(out_width)) vec_col_out_ifc;

		for(Integer i=0; i<vnRow; i=i+1)begin
			vec_row_ifc[i] = (
				interface Ifc_row;
					interface rx_input = array[i][0].subifc_rx_input;
				endinterface);
		end

		for(Integer i=0; i<vnCol; i=i+1)begin
			vec_col_in_ifc[i] = (
				interface Ifc_col_in;
					interface rx_weight = array[0][i].subifc_rx_stat;
					interface rx_output = array[0][i].subifc_rx_output;
				endinterface);
			vec_col_out_ifc[i] = (
				interface Ifc_col_out;
					interface tx_output = array[vnRow-1][i].subifc_tx_output;
				endinterface);
		end

		interface subifc_rows = vec_row_ifc;
    interface subifc_cols_in = vec_col_in_ifc;
		interface subifc_cols_out = vec_col_out_ifc;

  endmodule

endpackage
