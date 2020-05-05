/*
Author: Vinod Ganesan
Email ID: g.vinod1993@gmail.com
Details: Simple Scalar ALU
*/

package vector_alu;

import scalar_alu::*;
import  Vector ::*;
import isa::*;
`include "Logger.bsv"

interface Ifc_send_alu#(numeric type alu_width);
  method ActionValue#(Bit#(alu_width)) mav_send_operands(Bit#(alu_width) op1, Bit#(alu_width) op2, ALU_Opcode op_type);
endinterface

interface Ifc_vector_alu#(numeric type alu_width, numeric type num_col);
  interface Vector#(num_col, Ifc_send_alu#(alu_width)) subifc_send_col_value;
endinterface 

module mk_vector_alu(Ifc_vector_alu#(alu_width, num_col));

  let vnum_col = valueOf(num_col);
  
  Ifc_scalar_alu#(alu_width) scalar_alu[vnum_col];
  
  for(Integer i = 0; i < vnum_col; i=i+1) begin
    scalar_alu[i] <- mk_scalar_alu();
  end
    
  Vector#(num_col, Ifc_send_alu#(alu_width)) v_col_value;
  
  for(Integer i = 0; i < vnum_col; i=i+1) begin
    v_col_value[i] = ( 
      interface Ifc_send_alu#(alu_width); 
        method ActionValue#(Bit#(alu_width)) mav_send_operands(Bit#(alu_width) op1,Bit#(alu_width) op2, 
                                                                  ALU_Opcode op_type);
          let lv_out <- scalar_alu[i].mav_inp_operands(op1, op2, op_type);
          `logLevel(vectoralu, 0, $format(" Vector ALU : %d Received ALU instruction \n",i))
          return lv_out;
        endmethod
      endinterface
    );
  end
  
  interface subifc_send_col_value = v_col_value;

endmodule
endpackage 