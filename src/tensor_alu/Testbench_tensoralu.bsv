import toptensorALU::*;
import BRAMCore::*;
import Vector::*;
import isa::*;
import DReg::*;
`include "Logger.bsv"

module mkTestbench_tensoralu();

    Integer mem_per_bank = valueOf(TExp#(TSub#(26,TLog#(64))));
    Vector#(64,BRAM_PORT#(Bit#(TSub#(26,TLog#(64))), Bit#(16))) output_buffer <- replicateM(mkBRAMCore1(mem_per_bank,False));

    Ifc_toptensorALU#(16, 64) tensoralu <- mktoptensorALU;

    Reg#(Bit#(3)) rg_flag <- mkReg(0);
    Reg#(ALU_params) rg_alu_instr <- mkReg(unpack(0));

    Reg#(Bit#(6)) rg_counter <- mkReg(0);
    Reg#(Bool) rg_sram_read <- mkDReg(False);

    rule rl_dump_vars;
        $dumpvars();
    endrule

    rule rl_write_to_memory(rg_flag ==0 && rg_counter < 36);
        for(Integer i=0; i<64; i=i+1) begin
            output_buffer[i].put(True, zeroExtend(rg_counter), fromInteger(i) + zeroExtend(rg_counter));
        end
        rg_counter <= rg_counter + 1;
    endrule

    rule rl_initialise_alu_instruction(rg_flag == 0 && rg_counter == 36);
        ALU_params lv_instr = unpack(0);
        lv_instr.alu_opcode = Max;
        lv_instr.input_address = 0;
        lv_instr.output_address = 40;
        lv_instr.output_height = 6;
        lv_instr.output_width = 6;
        lv_instr.window_height = 1;
        lv_instr.window_width = 1;
        lv_instr.mem_stride_OW = 1;
        lv_instr.mem_stride_R = 6;
        lv_instr.mem_stride_S = 1;
        lv_instr.stride_h = 1;
        lv_instr.stride_w = 1;
        lv_instr.use_immediate = True;
        lv_instr.immediate_value = 0;
        rg_alu_instr <= lv_instr;
        tensoralu.getParams(lv_instr);
        `logLevel(testbench, 0, $format("From testbench, Assigned ALU instruction"))
        rg_flag <= 1;
        rg_counter <= 0;
    endrule

    rule rl_get_sram_request;
        let lv_addr = tensoralu.send_req_op;
        for(Integer i=0; i<64; i=i+1) begin
            output_buffer[i].put(False, truncate(lv_addr), ?);
        end
        rg_sram_read <= True;
        `logLevel(testbench, 0, $format("From testbench: received SRAM read reuqest %d \n", lv_addr))
    endrule

    rule rl_send_sram_response(rg_sram_read == True);
        Vector#(64, Bit#(16)) lv_data = unpack(0);
        for(Integer i=0; i<64; i=i+1) begin
            lv_data[i] = truncate(output_buffer[i].read);
        end
        tensoralu.recvOp(lv_data);
        `logLevel(testbench, 0, $format("From testbench: sending SRAM read response \n"))
    endrule

    rule rl_get_alu_output(rg_counter < 36);
        let lv_temp <- tensoralu.putResult;
        let lv_addr = tpl_1(lv_temp);
        let lv_data = tpl_2(lv_temp);
        let lv_mask;
        for(Integer i=0; i<64; i=i+1) begin
            output_buffer[i].put(True, truncate(lv_addr), lv_data[i]);
        end 
        `logLevel(testbench, 0, $format("From testbench: sending SRAM write request %d \n",lv_addr))
        rg_counter <= rg_counter + 1;
        if(rg_counter == 35)
            $finish();
    endrule


endmodule