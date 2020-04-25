/*
Author: Vinod Ganesan, Gokulan Ravi
Email ID: g.vinod1993@gmail.com, gokulan97@gmail.com
Details: Top Module of Vector ALU
*/

package toptensorALU;
import vectorALU::*;
import GetPut::*;
import Vector::*;
import isa::*;
`include "Logger.bsv"

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
  2. Add mask when writing back to memory where the number of valid ALU elements should be from instruction
  ...
*/
interface Ifc_toptensorALU#(numeric type aluWidth, numeric type nCol);
    method Action getParams(ALU_params params);
    method SRAM_address send_req_op;
    method Action recvOp(Vector#(nCol, Bit#(aluWidth)) vec_data);
    method ActionValue#(Tuple2#(SRAM_address, Vector#(nCol, Bit#(TAdd#(aluWidth,1))))) putResult;
endinterface

module mktoptensorALU(Ifc_toptensorALU#(aluWidth, nCol))
    provisos(
        Add#(aluWidth, 1, aluWidth2)
    );

    Integer vnCol = valueOf(nCol);

    Reg#(Maybe#(ALU_params)) rg_aluPacket <- mkReg(tagged Invalid);
    Reg#(SRAM_address) rg_irow_addr <- mkReg(0);
    Reg#(SRAM_address) rg_icol_addr <- mkReg(0);
    Reg#(SRAM_address) rg_srow_addr <- mkReg(0);
    Reg#(SRAM_address) rg_scol_addr <- mkReg(0);
    Reg#(SRAM_address) rg_output_addr <- mkReg(0);
    
    Wire#(SRAM_address) wr_send_req <- mkWire();
    
    Vector#(nCol, Reg#(Bit#(aluWidth2))) vec_operand_out <- replicateM(mkReg(0));
    Vector#(nCol, Wire#(Bit#(aluWidth))) wr_vec_operand <- replicateM(mkWire());
    Vector#(nCol, Wire#(Bit#(aluWidth2))) wr_vec_operand_out <- replicateM(mkWire());

    Reg#(Dim1) rg_i_var <- mkReg(1);
    Reg#(Dim1) rg_j_var <- mkReg(1);
    Reg#(Dim2) rg_k_var <- mkReg(1);
    Reg#(Dim2) rg_l_var <- mkReg(0);

    Reg#(Dim2) rg_mem_count <- mkReg(0);

    Ifc_vectorALU#(aluWidth, nCol) vectorALU <- mkvectorALU();

    //Innermost loop, for l = 1 to S, l < R
    rule rl_send_req(rg_aluPacket matches tagged Valid .rgalu &&& 
                  rg_l_var < rgalu.window_width);
        rg_scol_addr <= rg_scol_addr + zeroExtend(rgalu.mem_stride_S);
        rg_l_var <= rg_l_var+1;
        wr_send_req <= rg_scol_addr;
        `logLevel(toptensoralu, 1, $format(" toptensoralu : l = %d; l loop input address %d \n", rg_l_var, rg_scol_addr))
    endrule
  
    // out = op(out, in)
    rule rl_perform_computation(rg_aluPacket matches tagged Valid .rgalu);
        for(Integer i = 0; i < vnCol; i=i+1) begin
            let x <- vectorALU.sendcolValue[i].sendoperands(truncate(vec_operand_out[i]), wr_vec_operand[i], rgalu.alu_opcode);
            if(rg_l_var == rgalu.window_height && rg_k_var == rgalu.window_width) begin
              wr_vec_operand_out[i] <= x;
              vec_operand_out[i] <= 0;
            end
        end
        `logLevel(toptensoralu, 0, $format(" toptensoralu : Sending operands to vectors : i = %d; j = %d; k = %d; l = %d \n", rg_i_var, rg_j_var, rg_k_var, rg_l_var))
    endrule
 
    //3rd nested loop, l = S, input_address = input_address + stride_R, k = k + 1
    rule rl_end_loop_l(rg_aluPacket matches tagged Valid .rgalu &&& 
                        rg_l_var == rgalu.window_width &&&
                        rg_k_var < rgalu.window_height);
       let op1_base = rg_srow_addr + zeroExtend(rgalu.mem_stride_R);
       rg_srow_addr <= op1_base; 
       rg_scol_addr <= op1_base;
       rg_k_var <= rg_k_var + 1; // k = k + 1
       rg_l_var <= 0; // l = 0
       rg_mem_count <= 0;
       `logLevel(toptensoralu, 1, $format(" toptensoralu : k = %d; l = %d; k loop input address %d \n", rg_k_var, rg_l_var, rg_srow_addr))
    endrule
   
    //2nd nested loop, k = R, l = S, input_address = rg_icol_addr + Sy*mem_stride_S, j = j + 1
    rule rl_end_loop_k(rg_aluPacket matches tagged Valid .rgalu &&& 
                       rg_l_var == rgalu.window_width &&& 
                       rg_k_var == rgalu.window_height &&& 
                       rg_j_var < rgalu.output_width &&& 
                       rg_mem_count < rgalu.stride_w);
      if(rg_mem_count == rgalu.stride_w - 1) begin
        let op1_base = rg_icol_addr + zeroExtend(rgalu.mem_stride_S);
        rg_icol_addr <= op1_base;
        rg_srow_addr <= op1_base;
        rg_scol_addr <= op1_base;
        rg_output_addr <= rg_output_addr + zeroExtend(rgalu.mem_stride_OW);
        rg_j_var <= rg_j_var + 1;
        rg_k_var <= 1; 
        rg_l_var <= 0;
      end
      else begin
        rg_mem_count <= rg_mem_count + 1;
        rg_icol_addr <= rg_icol_addr + zeroExtend(rgalu.mem_stride_S);
      end
      `logLevel(toptensoralu, 1, $format(" toptensoralu : j = %d; k = %d; l = %d; j loop input address %d mem_count %d \n", rg_j_var, rg_k_var, rg_l_var, rg_icol_addr, rg_mem_count))
    endrule

    //Outermost loop, j = OW, k = R, l = S, input_address = rg_irow_addr + Sx*mem_stride_R, i = i + 1
    rule rl_end_loop_j(rg_aluPacket matches tagged Valid .rgalu &&& 
                    rg_l_var == rgalu.window_width &&&
                    rg_k_var == rgalu.window_height &&&
                    rg_j_var == rgalu.output_width &&& 
                    rg_i_var < rgalu.output_height &&& 
                    rg_mem_count < rgalu.stride_h);
      if(rg_mem_count == rgalu.stride_h - 1) begin
        let op1_base = rg_irow_addr + zeroExtend(rgalu.mem_stride_R);
        rg_icol_addr <= op1_base;
        rg_irow_addr <= op1_base;
        rg_srow_addr <= op1_base;
        rg_scol_addr <= op1_base;
        rg_output_addr <= rg_output_addr + zeroExtend(rgalu.mem_stride_OW);
        rg_i_var <= rg_i_var + 1;
        rg_l_var <= 0;
        rg_k_var <= 1;
        rg_j_var <= 1;
        rg_mem_count <= 0;
      end
      else begin
        rg_mem_count <= rg_mem_count + 1;
        rg_irow_addr <= rg_irow_addr + zeroExtend(rgalu.mem_stride_R);
      end
      `logLevel(toptensoralu, 1, $format(" toptensoralu : i = %d, j = %d; k = %d; l = %d; i loop input address %d mem count %d \n", rg_i_var, rg_j_var, rg_k_var, rg_l_var, rg_irow_addr, rg_mem_count))
    endrule

    //End of pseudocode, send tokens to dependency module if applicable
    rule rl_end_loop_i(rg_aluPacket matches tagged Valid .rgalu &&& 
                         rg_l_var == rgalu.window_width &&& 
                         rg_k_var == rgalu.window_height &&&
                         rg_j_var == rgalu.output_width &&& 
                         rg_i_var == rgalu.output_height);
          rg_l_var <= 1;
          rg_k_var <= 1;
          rg_j_var <= 1;
          rg_i_var <= 1;
          rg_aluPacket <= tagged Invalid;
          `logLevel(toptensoralu, 1, $format(" toptensoralu : End of 2D operation i = %d; j = %d; k = %d; l = %d; \n", rg_i_var, rg_j_var, rg_k_var, rg_l_var))
          //TODO: Send appropriate tokens to dependency module
    endrule

    method Action getParams(ALU_params params) if(rg_aluPacket matches tagged Invalid);
        rg_aluPacket <= tagged Valid params;
        let lv_in_base_addr = params.input_address;
        rg_irow_addr <= lv_in_base_addr;
        rg_icol_addr <= lv_in_base_addr;
        rg_scol_addr <= lv_in_base_addr;
        rg_srow_addr <= lv_in_base_addr;
        rg_output_addr <= params.output_address;
        `logLevel(toptensoralu, 0, $format(" toptensoralu : Received ALU instruction : OPcode %d; input address %d; output address %d; output height %d \n", params.alu_opcode, params.input_address, params.output_address, params.output_height, params.output_width))
        `logLevel(toptensoralu, 0, $format(" toptensoralu : R = %d; S = %d; S_OW = %d; S_R = %d; S_S = %d; Sx = %d; Sy = %d \n", params.window_height, params.window_width, params.mem_stride_OW, params.mem_stride_R, params.mem_stride_S, params.stride_h, params.stride_w))
      endmethod

    method SRAM_address send_req_op;
        return wr_send_req;
    endmethod

    method Action recvOp(Vector#(nCol, Bit#(aluWidth)) vec_data);
          for(Integer i = 0; i < vnCol; i=i+1) begin
            wr_vec_operand[i] <= extend(vec_data[i]);
            `logLevel(toptensoralu, 0, $format(" toptensoralu : %d : Receiving operands from SRAM  %d \n", i, vec_data[i]))
          end
    endmethod

    method ActionValue#(Tuple2#(SRAM_address, Vector#(nCol, Bit#(TAdd#(aluWidth,1))))) putResult;     
        Vector#(nCol,Bit#(TAdd#(aluWidth,1))) lv_temp;
        for(Integer i=0; i< vnCol; i= i+1) begin
          lv_temp[i] = wr_vec_operand_out[i];
        `logLevel(toptensoralu, 0, $format(" toptensoralu : %d : Writing to SRAM %d \n", i, lv_temp[i]))
        end
        return tuple2(rg_output_addr, lv_temp);
    endmethod

endmodule

endpackage
