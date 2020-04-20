package pipe_mul;
  import Vector::*;

  interface Ifc_pipe_mul#(numeric type w1, numeric type w2, numeric type num_stages);
    method Action inp(Bit#(w1) i1, Bit#(w2) i2);
    method Bit#(TAdd#(w1, w2)) outp;
  endinterface

  module mkpipe_mul(Ifc_pipe_mul#(w1, w2, num_stages))
  provisos(Add#(w1, w2, wo),
           Log#(num_stages, num_stages_bits));
    let v_num_stages= valueOf(num_stages);

    Vector#(num_stages, Reg#(Bit#(w1)))  rg_op1   <- replicateM(mkReg(0));
    Vector#(num_stages, Reg#(Bit#(w2)))  rg_op2   <- replicateM(mkReg(0));
    Vector#(num_stages, Reg#(Bool))      rg_valid <- replicateM(mkReg(False));

    Wire#(Bit#(TAdd#(w1,w2))) wr_result <- mkWire();

    rule rl_perform_mult;
      for(Integer i=0; i<v_num_stages-2; i=i+1) begin
        rg_op1[i+1]<= rg_op1[i];
        rg_op2[i+1]<= rg_op2[i];
        rg_valid[i+1]<= rg_valid[i];
      end
      
      wr_result<= primMul(rg_op1[v_num_stages-1], rg_op2[v_num_stages-1]);
    endrule

    method Action inp(Bit#(w1) i1, Bit#(w2) i2);
      rg_op1[0]<= i1;
      rg_op2[0]<= i2;
      rg_valid[0]<= True;
    endmethod
    
    method Bit#(TAdd#(w1, w2)) outp if(rg_valid[v_num_stages-1]);
      return wr_result;
    endmethod
    
  endmodule
endpackage
    
