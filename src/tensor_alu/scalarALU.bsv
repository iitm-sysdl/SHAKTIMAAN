/*
Author: Vinod Ganesan
Email ID: g.vinod1993@gmail.com
Details: Simple Scalar ALU
*/

/*
  TODO: 
  1. If need be, add shift functions with op2 being the immediate value to shift op1 either 
  left or right
  2. Should the size of output be aluwidth+1 or aluwidth?
  3. Add shift left operation
*/

package scalarALU;

  import isa::*;
  `include "Logger.bsv"

interface Ifc_scalarALU#(numeric type aluWidth);
  method ActionValue#(Bit#(TAdd#(aluWidth, 1))) inp_operands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, ALU_Opcode op_type);
endinterface
  
//A Dummy module to simply compile
(*synthesize*)
module mkscalardummy(Ifc_scalarALU#(32));
    let ifc();
    mkscalarALU inst(ifc);
    return (ifc);
endmodule

module mkscalarALU(Ifc_scalarALU#(aluWidth))
    provisos(
              Add#(aluWidth, 1, aluWidth2)
            );
  
  let dLEN = valueOf(aluWidth);

  method ActionValue#(Bit#(TAdd#(aluWidth,1))) inp_operands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, ALU_Opcode op_type);
    Bool cmp = op1>op2;
    let max_out = cmp? op1 : op2;
    let min_out = cmp? op2 : op1;
    
    Int#(aluWidth) signed_op1 = unpack(op1);
    Int#(aluWidth) signed_op2 = unpack(op2);

    Int#(aluWidth2) signed_output = extend(signed_op1) + extend(signed_op2);
    Bit#(aluWidth) shifted_output = op1 << op2;
    
    Bit#(aluWidth2) outp = 0; 
    if(op_type == Max)
      outp = extend(max_out);
    else if(op_type == Min)
      outp = extend(min_out);
    else if(op_type == Add)
      outp = pack(signed_output);
    else if(op_type == Shift)
      outp = extend(shifted_output);
      `logLevel(toptensoralu, 0, $format(" Scalar ALU : Received ALU instruction: opcode %d; operand_1 %d; operand_2 %d; output %d; \n", op_type, op1, op2, outp))
    return outp;
  endmethod

endmodule

endpackage
