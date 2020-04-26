/*
Author: Vinod Ganesan
Email ID: g.vinod1993@gmail.com
Details: Simple Scalar ALU
*/

/*
  TODO: 
  1. Add shift left operation, if required
*/

package scalarALU;

  import isa::*;
  `include "Logger.bsv"

interface Ifc_scalarALU#(numeric type aluWidth);
  method ActionValue#(Bit#(aluWidth)) inp_operands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, ALU_Opcode op_type);
endinterface
  
//A Dummy module to simply compile
(*synthesize*)
module mkscalardummy(Ifc_scalarALU#(32));
    let ifc();
    mkscalarALU inst(ifc);
    return (ifc);
endmodule

module mkscalarALU(Ifc_scalarALU#(aluWidth));
  
  let dLEN = valueOf(aluWidth);

  method ActionValue#(Bit#(aluWidth)) inp_operands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, ALU_Opcode op_type);
    Int#(aluWidth) signed_op1 = unpack(op1);
    Int#(aluWidth) signed_op2 = unpack(op2);
    
    Bool cmp = signed_op1>signed_op2;
    let max_out = max(signed_op1, signed_op2);
    let min_out = min(signed_op1, signed_op2);

    Int#(aluWidth) signed_output = signed_op1 + signed_op2;
    Bit#(aluWidth) shifted_output = op1 << op2;
    
    Bit#(aluWidth) outp = 0; 
    if(op_type == Max)
      outp = pack(max_out);
    else if(op_type == Min)
      outp = pack(min_out);
    else if(op_type == Add)
      outp = pack(signed_output);
    else if(op_type == Shift)
      outp = shifted_output;
      `logLevel(toptensoralu, 0, $format(" Scalar ALU : Received ALU instruction: opcode %d; operand_1 %d; operand_2 %d; output %d; \n", op_type, op1, op2, outp))
    return outp;
  endmethod

endmodule

endpackage