/*
Author: Vinod Ganesan, Gokulan Ravi, Sadhana
Email ID: g.vinod1993@gmail.com, gokulan97@gmail.com
Details: Top Module of Vector ALU
*/

package tensor_alu;
	import GetPut::*;
	import Vector::*;
	import isa::*;
	`include "Logger.bsv"
	`include "systolic.defines"
	
	interface Ifc_tensor_alu#(numeric type alu_width, numeric type num_col, numeric type of_index,
							  numeric type alu_pad);
		interface Put#(ALU_params#(of_index, alu_pad)) subifc_put_alu_params;
		method ActionValue#(TALUOpReq#(of_index)) mv_send_req_op;
		method Action ma_recv_op(Vector#(num_col, Bit#(alu_width)) vec_data);
		method ActionValue#(TALUOutReq#(of_index, alu_width, num_col)) mav_put_result;
		interface Get#(Bool) subifc_get_alu_complete;
	endinterface
	
	//(*synthesize*)
	//module mktalu_Tb(Ifc_tensor_alu#(16, 4, 8, 23));
	//  let ifc();
	//  mk_tensor_alu inst1(ifc);
	//  return (ifc);
	//endmodule
	
	module mk_tensor_alu(Ifc_tensor_alu#(alu_width, num_col, of_index, alu_pad))
		provisos(
					Add#(8, a__, of_index),
					Add#(8, b__, alu_width)
				);
	
		Integer vnum_col = valueOf(num_col);
	
		Reg#(Maybe#(ALU_params#(of_index, alu_pad))) rg_alu_packet <- mkReg(tagged Invalid);
		Reg#(SRAM_index#(of_index)) rg_irow_addr <- mkReg(0);
		Reg#(SRAM_index#(of_index)) rg_icol_addr <- mkReg(0);
		Reg#(SRAM_index#(of_index)) rg_srow_addr <- mkReg(0);
		Reg#(SRAM_index#(of_index)) rg_scol_addr <- mkReg(0);
		Reg#(SRAM_index#(of_index)) rg_output_addr <- mkReg(0);
		
		Wire#(SRAM_index#(of_index)) wr_send_req <- mkWire();
		
		Vector#(num_col, Reg#(Bit#(alu_width))) rg_operand_out <- replicateM(mkReg(0));
		Vector#(num_col, Wire#(Bit#(alu_width))) wr_operand <- replicateM(mkWire());
		Vector#(num_col, Wire#(Bit#(alu_width))) wr_operand_out <- replicateM(mkWire());
	
		Reg#(Dim1) rg_i_var <- mkReg(1);
		Reg#(Dim1) rg_j_var <- mkReg(1);
		Reg#(Dim2) rg_k_var <- mkReg(1);
		Reg#(Dim2) rg_l_var <- mkReg(1);

		Reg#(Dim2) rg_k_out <- mkReg(1);
		Reg#(Dim2) rg_l_out <- mkReg(1);

		Reg#(Bool) rg_req_complete <- mkReg(False);
		Reg#(Bool) rg_alu_complete <- mkReg(False);
		Reg#(Bool) rg_which_buffer <- mkReg(False);
	
		function Bit#(alu_width) fn_alu(Bit#(alu_width) op1, Bit#(alu_width) op2, ALU_Opcode opcode);
		  Int#(alu_width) lv_signed_op1 = unpack(op1);
	    Int#(alu_width) lv_signed_op2 = unpack(op2);
	    
	    let lv_max_out = max(lv_signed_op1, lv_signed_op2);
	    let lv_min_out = min(lv_signed_op1, lv_signed_op2);
	
	    Int#(alu_width) lv_signed_output = lv_signed_op1 + lv_signed_op2;
	    Bit#(alu_width) lv_shifted_output = op1 << op2;
	    
	    Bit#(alu_width) lv_outp = 0; 
	    if(opcode == Max)
	      lv_outp = pack(lv_max_out);
	    else if(opcode == Min)
	      lv_outp = pack(lv_min_out);
	    else if(opcode == Add)
	      lv_outp = pack(lv_signed_output);
	    else if(opcode == Shift)
	      lv_outp = lv_shifted_output;
	    `logLevel(toptensoralu, 0, $format("opcode %d; operand_1 %d; operand_2 %d; output %d; \n", opcode, op1, op2, lv_outp))
	    return lv_outp;
		endfunction
		
		// out = op(out, in)
		rule rl_perform_computation(rg_alu_packet matches tagged Valid .alu_packet);
			for(Integer i = 0; i < vnum_col; i=i+1) begin
				if(fromInteger(i) < alu_packet.num_active)begin
					let x = fn_alu(rg_operand_out[i], wr_operand[i], alu_packet.alu_opcode);
					if(rg_k_out == alu_packet.window_height && rg_l_out == alu_packet.window_width)begin
						wr_operand_out[i] <= x;
						rg_operand_out[i] <= alu_packet.use_immediate ? extend(alu_packet.immediate_value) : 0;
					end
				end
			end
			if(rg_k_out == alu_packet.window_height && rg_l_out == alu_packet.window_width)begin
				rg_l_out <= 1;
				rg_k_out <= 1;
				rg_output_addr <= rg_output_addr + 1;
				if(rg_req_complete)begin
					rg_alu_complete <= True;
				end
			end
			else if(rg_l_out == alu_packet.window_width)begin
				rg_l_out <= 1;
				rg_k_out <= rg_k_out + 1;
			end
			else begin
				rg_l_out <= rg_l_out + 1;
			end
		endrule
	
		interface Put subifc_put_alu_params;
		  method Action put(ALU_params#(of_index, alu_pad) params) if(rg_alu_packet matches tagged Invalid);
			rg_alu_packet <= tagged Valid params;
			let lv_in_base_addr = params.input_address;
			rg_irow_addr <= lv_in_base_addr;
			rg_icol_addr <= lv_in_base_addr;
			rg_scol_addr <= lv_in_base_addr;
			rg_srow_addr <= lv_in_base_addr;
			rg_i_var <= 1;
			rg_j_var <= 1;
			rg_k_var <= 1;
			rg_l_var <= 1;
			rg_output_addr <= params.output_address;
			rg_req_complete <= False;
			rg_alu_complete <= False;
			rg_which_buffer <= unpack(params.output_address[valueOf(of_index)-1]);//MSB of index
			if(params.use_immediate)begin
			  for(Integer i=0; i<vnum_col; i=i+1)begin
					rg_operand_out[i] <= extend(params.immediate_value);
				end
			end
		  endmethod
		endinterface
	
		method ActionValue#(TALUOpReq#(of_index)) mv_send_req_op if(rg_alu_packet matches tagged Valid .alu_packet
																																&&& !rg_req_complete);
		  SRAM_index#(of_index) addr;

			if(rg_l_var == alu_packet.window_width)begin
				rg_l_var <= 1;
				addr = rg_srow_addr + zeroExtend(alu_packet.mem_stride_S);
				
				if(rg_k_var == alu_packet.window_height)begin
					rg_k_var <= 1;
					addr = rg_icol_addr + zeroExtend(alu_packet.mem_stride_R);
					
					if(rg_j_var == alu_packet.output_width)begin
						rg_j_var <= 1;
						addr = rg_irow_addr + zeroExtend(alu_packet.mem_stride_OW);
						
						if(rg_i_var == alu_packet.output_height)begin
							rg_i_var <= 1;
							rg_req_complete <= True;
						end
						
						else begin
							rg_i_var <= rg_i_var + 1;
						end
						rg_irow_addr <= addr;
					
					end
					
					else begin
						rg_j_var <= rg_j_var + 1;
					end
					rg_icol_addr <= addr;
				
				end
				else begin
					rg_k_var <= rg_k_var + 1;
				end
				rg_srow_addr <= addr;
			
			end
			else begin
				rg_l_var <= rg_l_var + 1;
				addr = rg_scol_addr + 1;
			end
			rg_scol_addr <= addr;

			return TALUOpReq{ index: rg_scol_addr, num_valid: alu_packet.num_active, buffer: rg_which_buffer};
		endmethod
	
		method Action ma_recv_op(Vector#(num_col, Bit#(alu_width)) vec_data) if(rg_alu_packet matches tagged Valid .alu_packet);
		  for(Integer i = 0; i < vnum_col; i=i+1) begin
				if(fromInteger(i) < alu_packet.num_active)begin
					wr_operand[i] <= extend(vec_data[i]);
				end
		  end
		endmethod
	
		method ActionValue#(TALUOutReq#(of_index, alu_width, num_col)) mav_put_result
					if(rg_alu_packet matches tagged Valid .alu_packet);
		  Vector#(nCol, Bit#(alu_width)) lv_temp = replicate(0);
		  for(Integer i=0; i< vnum_col; i= i+1) begin
				if(fromInteger(i) < alu_packet.num_active)begin
					lv_temp[i] = wr_operand_out[i];
				end
		  end
			return TALUOutReq{index: rg_output_addr, values: lv_temp, num_valid: alu_packet.num_active, buffer: rg_which_buffer};
		endmethod
	
		interface Get subifc_get_alu_complete;
			method ActionValue#(Bool) get if(isValid(rg_alu_packet) &&& rg_alu_complete);
				rg_alu_complete <= False;
				rg_alu_packet <= tagged Invalid;
				return True;
			endmethod
		endinterface
	
	endmodule

endpackage
