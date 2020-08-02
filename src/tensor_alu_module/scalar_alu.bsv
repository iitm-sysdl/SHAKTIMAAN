/*
Author: Vinod Ganesan
Email ID: g.vinod1993@gmail.com
Details: Simple Scalar ALU
*/

/*
  TODO: 
  1. Add shift left operation, if required
*/

package scalar_alu;

  import isa::*;
  `include "Logger.bsv"

interface Ifc_scalar_alu#(numeric type alu_width);
  method ActionValue#(Bit#(alu_width)) mav_inp_operands(Bit#(alu_width) op1, Bit#(alu_width) op2, ALU_Opcode op_type);
endinterface
  
module mk_scalar_alu(Ifc_scalar_alu#(alu_width));
  
  method ActionValue#(Bit#(alu_width)) mav_inp_operands(Bit#(alu_width) op1, Bit#(alu_width) op2, ALU_Opcode op_type);
    Int#(alu_width) lv_signed_op1 = unpack(op1);
    Int#(alu_width) lv_signed_op2 = unpack(op2);
    
    Bool lv_cmp = lv_signed_op1>lv_signed_op2;
    let lv_max_out = max(lv_signed_op1, lv_signed_op2);
    let lv_min_out = min(lv_signed_op1, lv_signed_op2);

    Int#(alu_width) lv_signed_output = lv_signed_op1 + lv_signed_op2;
    Bit#(alu_width) lv_shifted_output = op1 << op2;
    
    Bit#(alu_width) lv_outp = 0; 
    if(op_type == Max)
      lv_outp = pack(lv_max_out);
    else if(op_type == Min)
      lv_outp = pack(lv_min_out);
    else if(op_type == Add)
      lv_outp = pack(lv_signed_output);
    else if(op_type == Shift)
      lv_outp = lv_shifted_output;
      `logLevel(toptensoralu, 0, $format(" Scalar ALU : Received ALU instruction: opcode %d; operand_1 %d; operand_2 %d; output %d; \n", op_type, op1, op2, lv_outp))
    return lv_outp;
  endmethod

endmodule

endpackage
