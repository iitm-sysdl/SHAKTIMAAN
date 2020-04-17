/*
Author: Vinod Ganesan, Gokulan Ravi
Email ID: g.vinod1993@gmail.com, gokulan97@gmail.com
Details: Top Module of Vector ALU
*/

package toptensorALU;
import vectorALU::*;
import GetPut::*;
import Vector::*;

//Assumptions:
//1. Each ALU instruction is aligned to the vector width. If there are C channels and Vector length
// is VLEN, where C >> VLEN. Compiler will generate C/VLEN ALU instructions to perform the task. 

/*
  Each ALU instruction performs the following pseudocode.
  A 3D slice of feature map is read as input, and another 3D slice of feature map is generated as output.

  PSEUDOCODE:
  --------------------------------
  # input: base address of input, output: base address of output
  
  basecopy = input
  for i = 1 to OH
      for j = 1 to OW
          out = 0
          for k = 1 to R
              for l = 1 to S
                  out = alu_op(out, *input)
              endfor
              input += mem_stride_S
          endfor
          input += mem_stride_R
          *output = out
          output += 1
      endfor
      input += mem_stride_OW
  endfor

  --------------------------------
*/

/* TODO
  1. Stride fields in the ISA needs to be standardised
    1.1 Updating input address
  2. Updating output address
  3. Writing back output address to SRAM
  ...
*/
interface Ifc_toptensorALU#(numeric type aluWidth, numeric type nCol);
    method Action getParams(ALU_params params);
    method SRAM_address send_req_op;
    method ActionValue#(Vector#(nCol, Bit#(aluWidth2))) putResult();
endinterface

module mktoptensorALU(Ifc_toptensorALU#(aluWidth, nCol))
    provisos(
        Add#(aluWidth, 1, aluWidth2)
    );

    Reg#(Maybe#(ALU_params)) rg_aluPacket <- mkReg(tagged Invalid);
    Reg#(SRAM_address) rg_baseaddr_copy <- mkReg(0);
    
    Wire#(Tuple2#(SRAM_address, SRAM_address)) wr_send_req <- mkWire();
    
    Vector#(nCol, Reg#(Bit#(aluWidth2))) vec_operand_out <- replicateM(mkReg(0));
    Vector#(nCol, Wire#(Bit#(aluWidth2))) wr_vec_operand <- replicateM(mkWire());
    Vector#(nCol, Wire#(Bit#(aluWidth2))) wr_vec_operand_out <- replicateM(mkWire());

    Reg#(Dim1) rg_i_var <- mkReg(0);
    Reg#(Dim1) rg_j_var <- mkReg(0);
    Reg#(Dim1) rg_k_var <- mkReg(0);
    Reg#(Dim1) rg_l_var <- mkReg(0);

    Ifc_vectorALU#(aluWidth, nCol) vectorALU <- mkvectorALU();

    //Innermost loop, for l = 1 to S, l < R
    rule rl_send_req(rg_aluPacket matches tagged Valid .rgalu &&& 
                  rg_l_var < extend(rgalu.window_width));
        let op1_base = rgalu.input_address;
        let opsend  = op1_base+rg_l_var;
        rg_l_var <= rg_l_var+1;
        wr_send_req <= opsend;
    endrule
  
    // out = op(out, in)
    rule rl_perform_computation(rg_aluPacket matches tagged Valid .rgalu);
        for(Integer i = 0; i < nCol; i=i+1) begin
            let x <- vectorALU.sendcolValue[i].sendoperands(vec_operand_out[i], wr_vec_operand[i], rgalu.alu_opcode);
            vec_operand_out[i] <= x;
        end
    endrule
 
    //3rd nested loop, l = S, input_address = input_address + stride_S, k = k + 1
    rule rl_end_loop_l(rg_aluPacket matches tagged Valid .rgalu &&& 
                        rg_l_var == rgalu.window_width &&&
                        rg_j_var < rgalu.window_height);
       let op1_base = rg_aluPacket.input_address + rgalu.mem_stride_S; 
       let opsend = op1_base;
       wr_send_req <= opsend;
       rg_aluPacket.input_address <= op1_base; // TODO: updating Maybe#
       rg_k_var <= rg_k_var + 1; // k = k + 1
       rg_l_var <= 0; // l = 0
    endrule
    
    //2nd nested loop, k = R, l = S, input_address = base_copy + stride_R, j = j + 1
    rule rl_end_loop_k(rg_aluPacket matches tagged Valid .rgalu &&& 
                       rg_l_var == rgalu.window_width &&& 
                       rg_k_var == rgalu.window_height &&& 
                       rg_j_var < rgalu.output_width);
      let op1_base = rg_baseaddr_copy + rgalu.mem_stride_R;
      rg_aluPacket.input_address <= op1_base; // TODO: updating Maybe#
      rg_baseaddr_copy <= op1_base;
      rg_j_var <= rg_j_var + 1;
      rg_k_var <= 0; 
      rg_l_var <= 0;
      wr_send_req <= opsend;
    endrule

    //Outermost loop, j = OW, k = R, l = S, input_address = base_copy + stride_OW, i = i + 1
    rule rl_end_loop_j(rg_aluPacket matches tagged Valid .rgalu &&& 
                    rg_l_var == rgalu.window_width &&&
                    rg_k_var == rgalu.window_height &&&
                    rg_j_var == rgalu.output_width &&& 
                    rg_i_var < rgalu.output_height);

      let op1_base = rg_baseaddr_copy + rgalu.mem_stride_OW;
      wr_send_req <= op1_base;
      rg_aluPacket.input_address <= op1_base; 
      rg_i_var <= rg_i_var + 1;
      rg_l_var <= 0;
      rg_k_var <= 0;
      rg_j_var <= 0;
    endrule

    //End of pseudocode, send tokens to dependency module if applicable
    rule rl_end_loop_i(rg_aluPacket matches tagged Valid .rgalu &&& 
                         rg_l_var == rgalu.window_width &&& 
                         rg_k_var == rgalu.window_height &&&
                         rg_j_var == rgalu.output_width &&& 
                         rg_i_var == rgalu.output_height);
          rg_l_var <= 0;
          rg_k_var <= 0;
          rg_j_var <= 0;
          rg_i_var <= 0;
          rg_aluPacket <= tagged Invalid;
          //TODO: Send appropriate tokens to dependency module
    endrule

    method Action getParams(ALU_params params);
        rg_aluPacket <= tagged Valid params;
        rg_baseaddr_copy <= params.input_address;
    endmethod

    method Tuple2#(Bool, Maybe#(SRAM_address)) send_req_op;
        return wr_send_req;
    endmethod

    method Action recvOp(Vector#(nCol, Bit#(aluWidth)) vec_data);
          for(Integer i = 0; i < nCol; i=i+1)
            wr_vec_operand[i] <= extend(vec_data[i]);
    endmethod

    method ActionValue#(Tuple2#(SRAM_address, Vector#(nCol, Bit#(aluWidth2)))) putResult();
          //Calculate the output SRAM address to be sent
           let out = rg_aluPacket.address_output; // + XXX
           return tuple2(out, wr_vec_operand_out);
    endmethod

endmodule

endpackage
