/* 
Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:
*/
package ws_pe;
  import ConfigReg::*;
	import TxRx::*;

  interface Ifc_ws_pe#(numeric type in_width, numeric type out_width);
		interface RXe#(Tuple2#(Bit#(in_width), Bit#(8))) subifc_rx_stat;
		interface RXe#(Bit#(in_width)) subifc_rx_input;
		interface RXe#(Bit#(out_width)) subifc_rx_output;
	
		interface TXe#(Tuple2#(Bit#(in_width), Bit#(8))) subifc_tx_stat;
		interface TXe#(Bit#(in_width)) subifc_tx_input;
		interface TXe#(Bit#(out_width)) subifc_tx_output;
  endinterface
	
	(*synthesize*)
  module mkmac_tb(Ifc_ws_pe#(8, 16));
    Ifc_ws_pe#(8, 16) inst1 <- mk_ws_pe(0, 0, 0);
    return (inst1);
  endmodule

	module mk_ws_pe#(Int#(8) row, Int#(8) col, parameter Integer coord)(Ifc_ws_pe#(in_width, out_width))
		provisos(Add#(a__, in_width, out_width));

		Reg#(Bit#(in_width)) rg_stat_value <- mkConfigReg(0);

		Reg#(Bit#(8)) rg_stat_coord <- mkConfigReg(fromInteger(coord));

		RX#(Tuple2#(Bit#(in_width), Bit#(8))) rx_stat <- mkRX;
		RX#(Bit#(in_width)) rx_input <- mkRX;
		RX#(Bit#(out_width)) rx_output <- mkRX;

		TX#(Tuple2#(Bit#(in_width), Bit#(8))) tx_stat <- mkTX;
		TX#(Bit#(in_width)) tx_input <- mkTX;
		TX#(Bit#(out_width)) tx_output <- mkTX;

		rule rl_mul_add;
			let lv_inp = rx_input.u.first;
			let lv_out = rx_output.u.first;
			rx_input.u.deq();
			rx_output.u.deq();
			Bit#(out_width) lv_psum = lv_out + extend(lv_inp * rg_stat_value);
			tx_input.u.enq(lv_inp);
			tx_output.u.enq(lv_psum);
		endrule

		rule rl_get_weight;
			let {val, crd} = rx_stat.u.first;
			rx_stat.u.deq();
			rg_stat_value <= val;
			rg_stat_coord <= crd;
		endrule
		
		rule rl_send_weight(rg_stat_coord < fromInteger(coord));
			tx_stat.u.enq(tuple2(rg_stat_value, rg_stat_coord));
		endrule

		interface subifc_rx_stat = rx_stat.e;
		interface subifc_rx_input = rx_input.e;
		interface subifc_rx_output = rx_output.e;
		interface subifc_tx_stat = tx_stat.e;
		interface subifc_tx_input = tx_input.e;
		interface subifc_tx_output = tx_output.e;
 
	endmodule

endpackage
