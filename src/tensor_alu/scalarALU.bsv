/*
Author: Vinod Ganesan
Email ID: g.vinod1993@gmail.com
Details: Simple Scalar ALU
*/

package scalarALU;

//Temporary -- Should standardize and add it to the common
`define ADD 'h1
`define MAX 'h2
`define MIN 'h3

interface Ifc_scalarALU#(numeric type aluWidth);
  method ActionValue#(Bit#(TAdd#(aluWidth, 1))) inp_operands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, Bit#(2) op_type);
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

  method ActionValue#(Bit#(TAdd#(aluWidth,1))) inp_operands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, Bit#(2) op_type);
    Bool cmp = op1>op2;
    let max_out = cmp? op1 : op2;
    let min_out = cmp? op2 : op1;
    
    Int#(aluWidth) signed_op1 = unpack(op1);
    Int#(aluWidth) signed_op2 = unpack(op2);

    //TODO: If need be, add shift functions with op2 being the immediate value to shift op1 either 
    //left or right

    Int#(aluWidth2) signed_output = extend(signed_op1) + extend(signed_op2);

    Bit#(aluWidth2) outp = case(op_type)
    `ADD : pack(signed_output);
    `MAX : extend(max_out);
    `MIN : extend(min_out);
    endcase; 
    return outp;

  endmethod

endmodule



endpackage
