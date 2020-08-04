/*
Author: Vinod Ganesan, Gokulan Ravi, Sadhana
Email ID: g.vinod1993@gmail.com, gokulan97@gmail.com
Details: Top Module of Vector ALU
*/

package tensor_alu;
import vector_alu::*;
import GetPut::*;
import Vector::*;
import isa::*;
`include "Logger.bsv"

//Assumptions:
//1. Each ALU instruction is aligned to the vector width. If there are C channels and Vector length
// is VLEN, where C >> VLEN. Compiler will generate C/VLEN ALU instructions to perform the task. 

/*
  Each ALU instruction performs the following pseudocode.
  A 3D slice of feature map is read as input, and another 3D slice of feature map is generated as output.

  OH - output_height
  OW - output_width
  R - window_hegiht
  S - window_width
  S_OW - mem_stride_OW - mem stride which gives the address difference of two consective output elements(output is stored in row major order)
  S_R - mem_stride_R - mem stride which gives the address difference of two consecutive elements in the input row
  S_S - mem_stride_S - mem stride which gives the address difference of two consecutive elements in the input column

  --------------------------------
*/

/* TODO
  1. Stride fields in the ISA needs to be standardised
	1.1 Updating input address
  2. Added output mask; num_of_filters is taken from ALUParams when this moved to other place correponding changes should be made
  3. Only ALU complete signal is sent to dependency resolver
  ...
*/
interface Ifc_tensor_alu#(numeric type alu_width, numeric type num_col, numeric type of_index,
						  numeric type alu_pad);
	interface Put#(ALU_params#(of_index, alu_pad)) subifc_put_alu_params;
	method SRAM_address mv_send_req_op;
	method Action ma_recv_op(Vector#(num_col, Bit#(alu_width)) vec_data);
	method ActionValue#(Tuple3#(SRAM_address, Vector#(num_col, Bit#(alu_width)), Bit#(TLog#(num_col)))) mav_put_result;
	interface Get#(Bool) subifc_get_alu_complete;
endinterface

module mk_tensor_alu(Ifc_tensor_alu#(alu_width, num_col, of_index, alu_pad))
	provisos(Bits#(Dim1,a),
			  Add#(b,a,alu_width),
			  Add#(c,TLog#(num_col),a)
			);

	Integer vnum_col = valueOf(num_col);

	Reg#(Maybe#(ALU_params#(of_index, alu_pad)) rg_alu_packet <- mkReg(tagged Invalid);
	Reg#(SRAM_index#(of_index)) rg_irow_addr <- mkReg(0);
	Reg#(SRAM_index#(of_index)) rg_icol_addr <- mkReg(0);
	Reg#(SRAM_index#(of_index)) rg_srow_addr <- mkReg(0);
	Reg#(SRAM_index#(of_index)) rg_scol_addr <- mkReg(0);
	Reg#(SRAM_index#(of_index)) rg_output_addr <- mkReg(0);
	
	Wire#(SRAM_address#(of_index)) wr_send_req <- mkWire();
	
	Vector#(num_col, Reg#(Bit#(alu_width))) v_operand_out <- replicateM(mkReg(0));
	Vector#(num_col, Wire#(Bit#(alu_width))) wr_v_operand <- replicateM(mkWire());
	Vector#(num_col, Wire#(Bit#(alu_width))) wr_v_operand_out <- replicateM(mkWire());

	Reg#(Dim1) rg_i_var <- mkReg(1);
	Reg#(Dim1) rg_j_var <- mkReg(1);
	Reg#(Dim2) rg_k_var <- mkReg(1);
	Reg#(Dim2) rg_l_var <- mkReg(0);

	Reg#(Bool) rg_alu_complete <- mkReg(False);

	Reg#(Dim1) rg_mask_counter <- mkReg(0);

	Ifc_vector_alu#(alu_width, num_col) vector_alu <- mk_vector_alu();

	//Innermost loop, for l = 1 to S, l < R
	rule rl_send_req(rg_alu_packet matches tagged Valid .alu_packet &&& 
				  rg_l_var < alu_packet.window_width);
		rg_scol_addr <= rg_scol_addr + 1;
		rg_l_var <= rg_l_var+1;
		wr_send_req <= rg_scol_addr;
	endrule
  
	// out = op(out, in)
	rule rl_perform_computation(rg_alu_packet matches tagged Valid .alu_packet);
		for(Integer i = 0; i < vnum_col; i=i+1) begin
			if(fromInteger(i) < alu_packet.num_active)begin
				let x <- vector_alu.subifc_send_col_value[i].mav_send_operands(v_operand_out[i], wr_v_operand[i], alu_packet.alu_opcode);
				if(rg_l_var == alu_packet.window_height && rg_k_var == alu_packet.window_width) begin
					wr_v_operand_out[i] <= x;
					v_operand_out[i] <= alu_packet.use_immediate ? extend(alu_packet.immediate_value) : 0;
				end
			end
		end
	endrule
 
	//3rd nested loop, l = S, input_address = input_address + stride_R, k = k + 1
	rule rl_end_loop_l(rg_alu_packet matches tagged Valid .alu_packet &&& 
						rg_l_var == alu_packet.window_width &&&
						rg_k_var < alu_packet.window_height);
	   let op1_base = rg_srow_addr + zeroExtend(alu_packet.mem_stride_S);
	   rg_srow_addr <= op1_base; 
	   rg_scol_addr <= op1_base;
	   rg_k_var <= rg_k_var + 1; // k = k + 1
	   rg_l_var <= 1; // l = 1
		 wr_send_req <= op1_base;
	endrule
   
	//2nd nested loop, k = R, l = S, input_address = rg_icol_addr + Sy*mem_stride_S, j = j + 1
	rule rl_end_loop_k(rg_alu_packet matches tagged Valid .alu_packet &&& 
					   rg_l_var == alu_packet.window_width &&& 
					   rg_k_var == alu_packet.window_height &&& 
					   rg_j_var < alu_packet.output_width);
		let op1_base = rg_icol_addr + zeroExtend(alu_packet.mem_stride_R);
		rg_icol_addr <= op1_base;
		rg_srow_addr <= op1_base;
		rg_scol_addr <= op1_base;
		rg_output_addr <= rg_output_addr + 1;
		rg_j_var <= rg_j_var + 1;
		rg_k_var <= 1;
		rg_l_var <= 1;
		wr_send_req <= op1_base;
	endrule

	//Outermost loop, j = OW, k = R, l = S, input_address = rg_irow_addr + Sx*mem_stride_R, i = i + 1
	rule rl_end_loop_j(rg_alu_packet matches tagged Valid .alu_packet &&& 
					rg_l_var == alu_packet.window_width &&&
					rg_k_var == alu_packet.window_height &&&
					rg_j_var == alu_packet.output_width &&& 
					rg_i_var < alu_packet.output_height);
		let op1_base = rg_irow_addr + zeroExtend(alu_packet.mem_stride_OW);
		rg_icol_addr <= op1_base;
		rg_irow_addr <= op1_base;
		rg_srow_addr <= op1_base;
		rg_scol_addr <= op1_base;
		rg_output_addr <= rg_output_addr + 1;
		rg_i_var <= rg_i_var + 1;
		rg_l_var <= 1;
		rg_k_var <= 1;
		rg_j_var <= 1;
		wr_send_req <= op1_base;
	endrule

	//End of pseudocode, send tokens to dependency module if applicable
	rule rl_end_loop_i(rg_alu_packet matches tagged Valid .alu_packet &&& 
						 rg_l_var == alu_packet.window_width &&& 
						 rg_k_var == alu_packet.window_height &&&
						 rg_j_var == alu_packet.output_width &&& 
						 rg_i_var == alu_packet.output_height);
		  rg_l_var <= 1;
		  rg_k_var <= 1;
		  rg_j_var <= 1;
		  rg_i_var <= 1;
		  rg_alu_packet <= tagged Invalid;
		  rg_alu_complete <= True;
	endrule

	interface Put subifc_put_alu_params;
	  method Action ma_get_params(ALU_params params) if(rg_alu_packet matches tagged Invalid);
		rg_alu_packet <= tagged Valid params;
		let lv_in_base_addr = params.input_address;
		rg_irow_addr <= lv_in_base_addr;
		rg_icol_addr <= lv_in_base_addr;
		rg_scol_addr <= lv_in_base_addr;
		rg_srow_addr <= lv_in_base_addr;
		rg_output_addr <= params.output_address;
		rg_alu_complete <= False;
		if(params.use_immediate)
		  for(Integer i=0; i<vnum_col; i=i+1) 
			v_operand_out[i] <= extend(params.immediate_value);
		end
	  endmethod
	endinterface

	method Tuple2#(SRAM_index#(of_index), Dim1) mv_send_req_op if(rg_alu_packet matches tagged Valid .alu_packet);
	  return tuple2(wr_send_req, params.num_active);
	endmethod

	method Action ma_recv_op(Vector#(num_col, Bit#(alu_width)) vec_data) if(rg_alu_packet matches tagged Valid .alu_packet);
	  for(Integer i = 0; i < vnum_col; i=i+1) begin
			if(fromInteger(i) < alu_packet.num_active)begin
				wr_v_operand[i] <= extend(vec_data[i]);
			end
	  end
	endmethod

	method ActionValue#(Tuple3#(SRAM_index#(of_index), Vector#(num_col, Bit#(alu_width)), Dim1)) mav_put_result
				if(rg_alu_packet matches tagged Valid .alu_packet);
	  Vector#(num_col,Bit#(alu_width)) lv_temp;
	  for(Integer i=0; i< vnum_col; i= i+1) begin
			if(fromInteger(i) < alu_packet.num_active)begin
				lv_temp[i] = wr_v_operand_out[i];
			end
			else
				lv_temp[i] = 'b0;
	  end
	  return tuple3(rg_output_addr, lv_temp, params.num_active);
	endmethod

	interface Get subifc_get_alu_complete;
		method ActionValue#(Bool) get if(rg_alu_packet matches tagged Invalid &&& rg_alu_complete);
			rg_alu_complete <= False;
			return True;
		endmethod
	endinterface
	endmethod

endmodule

endpackage
