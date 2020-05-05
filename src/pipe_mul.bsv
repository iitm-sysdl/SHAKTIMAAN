package pipe_mul;
  import Vector::*;
  import DReg::*;

  interface Ifc_pipe_mul#(numeric type w1, numeric type w2, numeric type num_stages);
    method Action inp(Bit#(w1) i1, Bit#(w2) i2);
    method Bit#(TAdd#(w1, w2)) outp;
  endinterface

  module mkpipe_mul(Ifc_pipe_mul#(w1, w2, num_stages))
  provisos(Add#(w1, w2, wo),
           Log#(num_stages, num_stages_bits));
    let v_num_stages= valueOf(num_stages);

    Vector#(num_stages, Reg#(Bit#(w1)))  rg_op1   <- replicateM(mkRegU());
    Vector#(num_stages, Reg#(Bit#(w2)))  rg_op2   <- replicateM(mkRegU());
    Reg#(Bool) rg_valid0 <- mkDReg(False);
    Vector#(num_stages, Reg#(Bool))      rg_valid <- replicateM(mkReg(False));

    Reg#(Bit#(TAdd#(w1,w2))) rg_result <- mkRegU();

    rule rl_perform_mult;
      rg_valid[0]<= rg_valid0;
      for(Integer i=0; i<v_num_stages-1; i=i+1) begin
        rg_op1[i+1]<= rg_op1[i];
        rg_op2[i+1]<= rg_op2[i];
        rg_valid[i+1]<= rg_valid[i];
      end
      rg_result<= primMul(rg_op1[v_num_stages-1], rg_op2[v_num_stages-1]);
    endrule

    method Action inp(Bit#(w1) i1, Bit#(w2) i2);
      rg_op1[0]<= i1;
      rg_op2[0]<= i2;
      rg_valid0<= True;
    endmethod
    
    method Bit#(TAdd#(w1, w2)) outp if(rg_valid[v_num_stages-1]);
      return rg_result;
    endmethod
    
  endmodule

  module mkTb_pipe_mul(Empty);
    Reg#(Bit#(32)) rg1 <- mkReg(0);
    Ifc_pipe_mul#(32,32,4) mul_inst <- mkpipe_mul;

    rule rl_give_inp(rg1==0);
      mul_inst.inp('d23,'d11);
      rg1<= 1;
    endrule

    rule rl_take_outp(rg1==1);
      let res= mul_inst.outp;
      $display("Res: %d",res);
      $finish(0);
    endrule
  endmodule
endpackage
 
