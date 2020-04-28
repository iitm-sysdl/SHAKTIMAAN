/*
Author: Vinod Ganesan, Gokulan Ravi
Email ID: g.vinod1993@gmail.com, gokulan97@gmail.com
Details: Top Module of Vector ALU
*/

package tensor_alu;
import vector_alu::*;
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

  OH - output_height
  OW - output_width
  R - window_hegiht
  S - window_width
  Sx - stride_h - window stride along the rows
  Sy - stride_w - window stride along the columns
  S_OW - mem_stride_OW - mem stride which gives the address difference of two consective output elements(output is stored in row major order)
  S_R - mem_stride_R - mem stride which gives the address difference of two consecutive elements in the input row
  S_S - mem_stride_S - mem stride which gives the address difference of two consecutive elements in the input column

  PSEUDOCODE:
  --------------------------------
  # input: base address of input, output: base address of output
  
  irow_addr = input_address
  for i = 1 to OH
      icol_addr = irow_addr
      for j = 1 to OW
          out = if_immediate ? immediate_value : 0
          srow_addr = icol_addr
          for k = 1 to R
              scol_addr = srow_addr
              for l = 1 to S
                  out = alu_op(out, *scol_addr)
                  scol_addr += mem_Stride_S
              endfor
              srow_addr += mem_stride_R
          endfor
          icol_addr += Sy*mem_stride_S
          *output_address = out
          output_address += mem_stride_OW
      endfor
      irow_addr += Sx*mem_stride_R
  endfor

  --------------------------------
*/

/* TODO
  1. Stride fields in the ISA needs to be standardised
    1.1 Updating input address
  2. Added output mask; num_of_filters is taken from ALUParams when this moved to other place correponding changes should be made
  3. Only ALU complete signal is sent to dependency resolver
  ...
*/
interface Ifc_tensor_alu#(numeric type alu_width, numeric type num_col);
    method Action ma_get_params(ALU_params params);
    method SRAM_address mv_send_req_op;
    method Action ma_recv_op(Vector#(num_col, Bit#(alu_width)) vec_data);
    method ActionValue#(Tuple3#(SRAM_address, Vector#(num_col, Bit#(alu_width)), Bit#(TLog#(num_col)))) mav_put_result;
    method Bool mv_alu_complete;
endinterface

module mk_tensor_alu(Ifc_tensor_alu#(alu_width, num_col))
    provisos(Bits#(Dim1,a),
              Add#(b,a,alu_width),
              Add#(c,TLog#(num_col),a)
            );

    Integer vnum_col = valueOf(num_col);

    Reg#(Maybe#(ALU_params)) rg_alu_packet <- mkReg(tagged Invalid);
    Reg#(SRAM_address) rg_irow_addr <- mkReg(0);
    Reg#(SRAM_address) rg_icol_addr <- mkReg(0);
    Reg#(SRAM_address) rg_srow_addr <- mkReg(0);
    Reg#(SRAM_address) rg_scol_addr <- mkReg(0);
    Reg#(SRAM_address) rg_output_addr <- mkReg(0);
    
    Wire#(SRAM_address) wr_send_req <- mkWire();
    
    Vector#(num_col, Reg#(Bit#(alu_width))) v_operand_out <- replicateM(mkReg(0));
    Vector#(num_col, Wire#(Bit#(alu_width))) wr_v_operand <- replicateM(mkWire());
    Vector#(num_col, Wire#(Bit#(alu_width))) wr_v_operand_out <- replicateM(mkWire());

    Reg#(Dim1) rg_i_var <- mkReg(1);
    Reg#(Dim1) rg_j_var <- mkReg(1);
    Reg#(Dim2) rg_k_var <- mkReg(1);
    Reg#(Dim2) rg_l_var <- mkReg(0);

    Reg#(Bool) rg_alu_complete <- mkReg(False);

    Reg#(Dim1) rg_mask_counter <- mkReg(0);
    Reg#(Bit#(TLog#(num_col))) rg_output_mask <- mkReg(0);

    Reg#(Dim2) rg_mem_count <- mkReg(0);

    Ifc_vector_alu#(alu_width, num_col) vector_alu <- mk_vector_alu();

    //Innermost loop, for l = 1 to S, l < R
    rule rl_send_req(rg_alu_packet matches tagged Valid .alu_packet &&& 
                  rg_l_var < alu_packet.window_width);
        rg_scol_addr <= rg_scol_addr + zeroExtend(alu_packet.mem_stride_S);
        rg_l_var <= rg_l_var+1;
        wr_send_req <= rg_scol_addr;
        `logLevel(toptensoralu, 1, $format(" toptensoralu : l = %d; l loop input address %d \n", rg_l_var, rg_scol_addr))
    endrule
  
    // out = op(out, in)
    rule rl_perform_computation(rg_alu_packet matches tagged Valid .alu_packet);
        for(Integer i = 0; i < vnum_col; i=i+1) begin
            let x <- vector_alu.subifc_send_col_value[i].mav_send_operands(v_operand_out[i], wr_v_operand[i], alu_packet.alu_opcode);
            if(rg_l_var == alu_packet.window_height && rg_k_var == alu_packet.window_width) begin
              wr_v_operand_out[i] <= x;
              v_operand_out[i] <= alu_packet.use_immediate ? extend(alu_packet.immediate_value) : 0;
            end
        end
        `logLevel(toptensoralu, 0, $format(" toptensoralu : Sending operands to vectors : i = %d; j = %d; k = %d; l = %d \n", rg_i_var, rg_j_var, rg_k_var, rg_l_var))
    endrule
 
    //3rd nested loop, l = S, input_address = input_address + stride_R, k = k + 1
    rule rl_end_loop_l(rg_alu_packet matches tagged Valid .alu_packet &&& 
                        rg_l_var == alu_packet.window_width &&&
                        rg_k_var < alu_packet.window_height);
       let op1_base = rg_srow_addr + zeroExtend(alu_packet.mem_stride_R);
       rg_srow_addr <= op1_base; 
       rg_scol_addr <= op1_base;
       rg_k_var <= rg_k_var + 1; // k = k + 1
       rg_l_var <= 0; // l = 0
       rg_mem_count <= 0;
       `logLevel(toptensoralu, 1, $format(" toptensoralu : k = %d; l = %d; k loop input address %d \n", rg_k_var, rg_l_var, rg_srow_addr))
    endrule
   
    //2nd nested loop, k = R, l = S, input_address = rg_icol_addr + Sy*mem_stride_S, j = j + 1
    rule rl_end_loop_k(rg_alu_packet matches tagged Valid .alu_packet &&& 
                       rg_l_var == alu_packet.window_width &&& 
                       rg_k_var == alu_packet.window_height &&& 
                       rg_j_var < alu_packet.output_width &&& 
                       rg_mem_count < alu_packet.stride_w);
      if(rg_mem_count == alu_packet.stride_w - 1) begin
        let op1_base = rg_icol_addr + zeroExtend(alu_packet.mem_stride_S);
        rg_icol_addr <= op1_base;
        rg_srow_addr <= op1_base;
        rg_scol_addr <= op1_base;
        rg_output_addr <= rg_output_addr + zeroExtend(alu_packet.mem_stride_OW);
        rg_j_var <= rg_j_var + 1;
        rg_k_var <= 1; 
        rg_l_var <= 0;
      end
      else begin
        rg_mem_count <= rg_mem_count + 1;
        rg_icol_addr <= rg_icol_addr + zeroExtend(alu_packet.mem_stride_S);
      end
      `logLevel(toptensoralu, 1, $format(" toptensoralu : j = %d; k = %d; l = %d; j loop input address %d mem_count %d \n", rg_j_var, rg_k_var, rg_l_var, rg_icol_addr, rg_mem_count))
    endrule

    //Outermost loop, j = OW, k = R, l = S, input_address = rg_irow_addr + Sx*mem_stride_R, i = i + 1
    rule rl_end_loop_j(rg_alu_packet matches tagged Valid .alu_packet &&& 
                    rg_l_var == alu_packet.window_width &&&
                    rg_k_var == alu_packet.window_height &&&
                    rg_j_var == alu_packet.output_width &&& 
                    rg_i_var < alu_packet.output_height &&& 
                    rg_mem_count < alu_packet.stride_h);
      if(rg_mem_count == alu_packet.stride_h - 1) begin
        let op1_base = rg_irow_addr + zeroExtend(alu_packet.mem_stride_R);
        rg_icol_addr <= op1_base;
        rg_irow_addr <= op1_base;
        rg_srow_addr <= op1_base;
        rg_scol_addr <= op1_base;
        rg_output_addr <= rg_output_addr + zeroExtend(alu_packet.mem_stride_OW);
        rg_i_var <= rg_i_var + 1;
        rg_l_var <= 0;
        rg_k_var <= 1;
        rg_j_var <= 1;
        rg_mem_count <= 0;
      end
      else begin
        rg_mem_count <= rg_mem_count + 1;
        rg_irow_addr <= rg_irow_addr + zeroExtend(alu_packet.mem_stride_R);
      end
      `logLevel(toptensoralu, 1, $format(" toptensoralu : i = %d, j = %d; k = %d; l = %d; i loop input address %d mem count %d \n", rg_i_var, rg_j_var, rg_k_var, rg_l_var, rg_irow_addr, rg_mem_count))
    endrule

    //End of pseudocode, send tokens to dependency module if applicable
    rule rl_end_loop_i(rg_alu_packet matches tagged Valid .alu_packet &&& 
                         rg_l_var == alu_packet.window_width &&& 
                         rg_k_var == alu_packet.window_height &&&
                         rg_j_var == alu_packet.output_width &&& 
                         rg_i_var == alu_packet.output_height);
          rg_l_var <= 1;
          rg_k_var <= 1;
          rg_j_var <= 1;
          rg_i_var <= 1;
          rg_alu_packet <= tagged Invalid;
          rg_alu_complete <= True;
          `logLevel(toptensoralu, 1, $format(" toptensoralu : End of 2D operation i = %d; j = %d; k = %d; l = %d; \n", rg_i_var, rg_j_var, rg_k_var, rg_l_var))
    endrule

    method Action ma_get_params(ALU_params params) if(rg_alu_packet matches tagged Invalid);
        rg_alu_packet <= tagged Valid params;
        let lv_in_base_addr = params.input_address;
        rg_irow_addr <= lv_in_base_addr;
        rg_icol_addr <= lv_in_base_addr;
        rg_scol_addr <= lv_in_base_addr;
        rg_srow_addr <= lv_in_base_addr;
        rg_output_addr <= params.output_address;
        rg_alu_complete <= False;
        if(params.use_immediate)
          for(Integer i=0; i<vnum_col; i=i+1) 
            v_operand_out[i] <= extend(params.immediate_value);
        Dim1 lv_temp;
        if(rg_mask_counter == 0)
          lv_temp = params.num_of_filters;
        else
          lv_temp = rg_mask_counter;
        if(lv_temp <= fromInteger(vnum_col)) begin
          rg_mask_counter <= 0;
          rg_output_mask <= truncate(lv_temp - 1);
          `logLevel(toptensoralu, 1, $format(" Calculating last mask %d \n", lv_temp))
        end
        else begin
          rg_mask_counter <= lv_temp - fromInteger(vnum_col);
          rg_output_mask <= fromInteger(vnum_col - 1);
          `logLevel(toptensoralu, 1, $format(" Calculating mask %d \n", lv_temp))
        end
        `logLevel(toptensoralu, 0, $format(" toptensoralu : Received ALU instruction : OPcode %d; input address %d; output address %d; output height %d \n", params.alu_opcode, params.input_address, params.output_address, params.output_height, params.output_width))
        `logLevel(toptensoralu, 0, $format(" toptensoralu : R = %d; S = %d; S_OW = %d; S_R = %d; S_S = %d; Sx = %d; Sy = %d Immediate %d Value %d filter %d \n", params.window_height, params.window_width, params.mem_stride_OW, params.mem_stride_R, params.mem_stride_S, params.stride_h, params.stride_w, params.use_immediate, params.immediate_value, params.num_of_filters))
      endmethod

    method SRAM_address mv_send_req_op;
        return wr_send_req;
    endmethod

    method Action ma_recv_op(Vector#(num_col, Bit#(alu_width)) vec_data);
          for(Integer i = 0; i < vnum_col; i=i+1) begin
            wr_v_operand[i] <= extend(vec_data[i]);
            `logLevel(toptensoralu, 0, $format(" toptensoralu : %d : Receiving operands from SRAM  %d \n", i, vec_data[i]))
          end
    endmethod

    method ActionValue#(Tuple3#(SRAM_address, Vector#(num_col, Bit#(alu_width)), Bit#(TLog#(num_col)))) mav_put_result;     
        Vector#(num_col,Bit#(alu_width)) lv_temp;
        for(Integer i=0; i< vnum_col; i= i+1) begin
          lv_temp[i] = wr_v_operand_out[i];
          `logLevel(toptensoralu, 0, $format(" toptensoralu : %d : Writing to SRAM %d mask %d \n", i, lv_temp[i], rg_output_mask))
        end
        return tuple3(rg_output_addr, lv_temp, rg_output_mask);
    endmethod

    method Bool mv_alu_complete if(rg_alu_packet matches tagged Invalid);
        return rg_alu_complete;
    endmethod

endmodule

endpackage
