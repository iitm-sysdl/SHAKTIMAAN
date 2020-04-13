/*
Author: Vinod Ganesan
Email ID: g.vinod1993@gmail.com
Details: Simple Scalar ALU
*/

package vectorALU;

import scalarALU::*;
import  Vector ::*;

interface Ifc_sendALU#(numeric type aluWidth);
  method ActionValue#(Bit#(TAdd#(aluWidth, 1))) sendoperands(Bit#(aluWidth) op1, Bit#(aluWidth) op2, Bit#(2) op_type);
endinterface

interface Ifc_vectorALU#(numeric type aluWidth, numeric type nCol);
  interface Vector#(nCol, Ifc_sendALU#(aluWidth)) sendcolValue;
endinterface 

//A Dummy module to simply compile
(*synthesize*)
module mkvectordummy(Ifc_vectorALU#(32, 16));
    let ifc();
    mkvectorALU inst(ifc);
    return (ifc);
endmodule


module mkvectorALU(Ifc_vectorALU#(aluWidth, nCol))
  provisos(
          Add#(aluWidth, 1, aluWidth2)
          );

  let vnCol = valueOf(nCol);
  
  Ifc_scalarALU#(aluWidth) scalarALU[vnCol];
  
  for(Integer i = 0; i < vnCol; i=i+1) begin
    scalarALU[i] <- mkscalarALU();
  end
    
  Vector#(nCol, Ifc_sendALU#(aluWidth)) vec_colValue;
  
  for(Integer i = 0; i < vnCol; i=i+1) begin
    vec_colValue[i] = ( 
      interface Ifc_sendALU#(aluWidth); 
        method ActionValue#(Bit#(TAdd#(aluWidth,1))) sendoperands(Bit#(aluWidth) op1,Bit#(aluWidth) op2, 
                                                                  Bit#(2) op_type);
          let x <- scalarALU[i].inp_operands(op1, op2, op_type);
          return x;
        endmethod
      endinterface
    );
  end
  
  interface sendcolValue = vec_colValue;

endmodule
endpackage 
