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
    Reg#(TAdd#(Dim1,1)) rg_counter <- mkReg(0);
    Reg#(SRAM_address) rg_baseaddr_copy <- mkReg(0);
    Wire#(Tuple2#(SRAM_address, SRAM_address)) wr_send_req <- mkWire();
    // Reg#(SRAM_address) rg_op2_address <- 
    Vector#(nCol, Reg#(Bit#(aluWidth2))) vec_operand_out <- replicateM(mkReg(0));
    Vector#(nCol, Wire#(Bit#(aluWidth2))) wr_vec_operand <- replicateM(mkWire());
    Vector#(nCol, Wire#(Bit#(aluWidth2))) wr_vec_operand_out <- replicateM(mkWire());

    Reg#(Dim1) rg_stride_counter <- mkReg(0);
    Reg#(Dim1) rg_window_counter <- mkReg(0);

    Ifc_vectorALU#(aluWidth, nCol) vectorALU <- mkvectorALU();

    //Both operand 1 and operand 2 will be in the same set of banks, creating conflicts
    //Hence sending one request at a time, using a ping-pong 1-bit token 
    rule send_req(rg_aluPacket matches tagged Valid .rgalu &&& rg_counter < extend(rgalu.loop_extent));
        let op1_base = rgalu.address_operand;
        let opsend  = op1_base+rg_counter;
        rg_counter <= rg_counter+1;
        wr_send_req <= opsend;
    endrule
  
    rule perform_computation;
        for(Integer i = 0; i < nCol; i=i+1) begin
            let x <- vectorALU.sendcolValue[i].sendoperands(vec_operand_out[i], wr_vec_operand[i], rg_aluPacket.alu_opcode);
            vec_operand_out[i] <= x;
        end
    endrule
 
    //Start of i+1th row in a window
    rule update_extents(rg_counter == extend(rg_aluPacket.loop_extent) && rg_stride_counter < window_size);
       let op1_base = rg_aluPacket.address_operand + output_X_stride; 
       let opsend = op1_base;
       wr_send_req <= opsend;
       rg_aluPacket.address_operand <= op1_base;
       rg_stride_counter <= rg_stride_counter + 1; 
       rg_counter <= 0; 
    endrule
    
    //Start of i+1th window -- define cond
    rule update_window(rg_counter == extend(rg_aluPacket.loop_extent) && rg_stride_counter == window_size 
          && rg_window_counter < window_loop_extent);
      let op1_base = rg_baseaddr_copy + window_X_stride;
      rg_aluPacket.address_operand <= op1_base;
      rg_baseaddr_copy <= op1_base;
      rg_stride_counter <= 0; 
      rg_counter <= 0;
      wr_send_req <= opsend;
    endrule

    //Start of i+1th row in the output
    rule update_row(rg_counter == extend(rg_aluPacket.loop_extent) && rg_stride_counter == window_size
          && rg_window_counter == rg_aluPacket.window_loop_extent && rg_output_counter < rg_aluPacket.output_loop_extent));
      let op1_base = rg_baseaddr_copy + rg_aluPacket.output_Y_stride;
      wr_send_req <= op1_base;
      rg_aluPacket.address_operand <= op1_base;
      rg_counter <= 0;
      rg_stride_counter <= 0;
      rg_window_counter <= 0;
    endrule

    rule end_instruction(rg_counter == extend(rg_aluPacket.loop_extent) && rg_stride_counter == window_size
          && rg_window_counter == rg_aluPacket.window_loop_extent && rg_output_counter == rg_aluPacket.output_loop_extent);
          rg_counter <= 0;
          rg_stride_counter <= 0;
          rg_window_counter <= 0;
          rg_output_counter <= 0;
          rg_aluPacket <= tagged Invalid;
          //TODO: Send appropriate tokens to dependency module
    endrule

    //Someone should make this tagged invalid somewhere right?
    //TODO - figure out where to make this tagged Invalid -- some last bit should be given - taken care in the above rule

    method Action getParams(ALU_params params);
        rg_aluPacket <= tagged Valid params;
        rg_baseaddr_copy <= params.address_operand;
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
