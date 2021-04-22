/*
Author   : Sadhana S
email id : sadhashan118@gmail.com
*/

import load_module::*;
import isa::*;
import BRAMCore::*;
import GetPut::*;
import FIFOF::*;
import Semi_FIFOF:: *;
import AXI4_Types:: *;
import AXI4_Fabric:: *;
import Connectable::*;
import FIFOF::*;
import Vector::*;
import device_common::*;
`include "systolic.defines"

function Bit#(0) fn_slave_map (Bit#(`DRAM_ADDR_WIDTH) addr);
    Bit#(0) slave_num = 0;
    if(addr >= 'h00000000 && addr<= 'hFFFFFFFF)
      slave_num = 0;
    return slave_num;
endfunction

typedef 128 Datawidth;
typedef TLog#(TDiv#(Datawidth,8)) Addr_index; 
typedef TMax#(TDiv#(Datawidth,`INWIDTH),TDiv#(Datawidth,`OUTWIDTH)) Max_words;
typedef TMax#(`INWIDTH,`OUTWIDTH) Max_data;
typedef TMax#(TMax#(`WBUF_INDEX,`IBUF_INDEX),`OBUF_INDEX) Max_index;
typedef TMax#(TMax#(`WBUF_Bankbits,`IBUF_Bankbits),`OBUF_Bankbits) Max_bank;


module mkload_module_test();

    Reg#(Bit#(4)) rg_flag <- mkReg(0);

    BRAM_PORT#(Bit#(TSub#(`DRAM_ADDR_WIDTH,Addr_index)), Bit#(Datawidth)) dram <- mkBRAMCore1(valueOf(TExp#(TSub#(`DRAM_ADDR_WIDTH,Addr_index))),False);
    Reg#(DRAM_address) rg_dram_address <- mkReg(0);
    rule rl_write_count_value_to_dram(rg_flag == 0);
        dram.put(True,truncateLSB(rg_dram_address),zeroExtend(rg_dram_address));
        if(rg_dram_address == 1023) begin
            rg_flag <= 1;
            rg_dram_address <= 0;
        end
        else
            rg_dram_address <= rg_dram_address + 1;
    endrule

    rule rl_dump_vars;
        $dumpvars;
    endrule

    Ifc_load_Module#(`DRAM_ADDR_WIDTH, Datawidth, `SRAM_ADDR_WIDTH, 
                    `WBUF_INDEX, `WBUF_BANKS, `INWIDTH,
                    `IBUF_INDEX, `IBUF_BANKS, `INWIDTH,
                    `OBUF_INDEX, `OBUF_BANKS, `OUTWIDTH, 
                    Max_index, Max_bank, Max_data, Max_words, 20) ld_module <- mk_load_Module;

    //let ld_module <- mkload_Tb;

    Reg#(SRAM_address) rg_sram_address <- mkReg(0);
    Reg#(Dim1) rg_x_size <- mkReg(0);
    Reg#(Dim1) rg_y_size <- mkReg(0);
    Reg#(Dim1) rg_z_size <- mkReg(0);
    Reg#(Dim1) rg_y_stride <- mkReg(0);
    Reg#(Dim1) rg_z_stride <- mkReg(0);
    Reg#(Bool) rg_is_reset <- mkReg(False);
    Reg#(Bool) rg_bitwidth <- mkReg(False);
    Reg#(Bit#(20)) rg_padding <- mkReg(0);


    rule rl_modify_load_parameters(rg_flag == 1);
        rg_dram_address <= 32'h00000000;
        rg_sram_address <= 26'h3000000;
        rg_x_size <= 1;
        rg_y_size <= 2;
        rg_y_stride <= 128;
        rg_z_size <= 2;
        rg_z_stride <= 128;
        rg_is_reset <= True;
        rg_bitwidth <= True;
        rg_padding <= 0;
        rg_flag <= 2;
    endrule

    rule rl_send_load_instruction(rg_flag == 2);
        Bit#(120) temp_1 = {rg_dram_address,rg_sram_address,rg_x_size,rg_y_size,rg_z_size,rg_z_stride,rg_y_stride,pack(rg_is_reset),pack(rg_bitwidth),rg_padding};
        Load_params#(20) temp_2 = unpack(temp_1);
        ld_module.subifc_put_loadparams.put(temp_2);
        rg_flag <= 3;

    endrule

    rule rl_sram_requests(rg_flag == 3);
        let sram_request <- ld_module.write_data;
        $display($time, " Write request to SRAM : Buffer %d Index %d Bank %d Data %d Valid %d \n",sram_request.buffer,sram_request.index,sram_request.bank,sram_request.data,sram_request.num_valid);
    endrule

    AXI4_Fabric_IFC #(1, 1, `DRAM_ADDR_WIDTH, Datawidth, 0) axi4_fabric <- mkAXI4_Fabric(fn_slave_map);
    AXI4_Slave_Xactor_IFC#(`DRAM_ADDR_WIDTH, Datawidth, 0) s_xactor  <- mkAXI4_Slave_Xactor;
    mkConnection(ld_module.master, axi4_fabric.v_from_masters[0]);
    mkConnection(axi4_fabric.v_to_slaves[0], s_xactor.axi_side);

    // Read request to DRAM
    Reg#(Bool) rg_read_request <- mkReg(False);
    Reg#(Bool) rg_burst_request <- mkReg(False);
    Reg#(AXI4_Rd_Addr#(`DRAM_ADDR_WIDTH,0)) rg_read_burst_address  <- mkRegA(?);
    Reg#(Bit#(8)) rg_read_burst_counter <- mkReg(0);
    rule rl_read_request(rg_read_request == False && rg_flag > 0);
        let req <- pop_o (s_xactor.o_rd_addr);
        dram.put(False,truncateLSB(req.araddr),?);
        if(req.arlen != 0) 
            rg_burst_request <= True;
        rg_read_burst_address <= req;
        rg_read_request <= True;
        $display($time, " DRAM Read request : addr %x arsize %x arburst %x arlen %x \n",req.araddr, req.arsize, req.arburst, req.arlen);
    endrule

    rule rl_read_burst_request(rg_read_request == False && rg_burst_request == True && rg_flag > 0);
        if(rg_read_burst_counter == rg_read_burst_address.arlen) begin
            rg_burst_request <= False;
            rg_read_burst_counter <= 0;
        end
        else begin
            rg_read_burst_counter <= rg_read_burst_counter + 1;	
            let address = axi4burst_addrgen(rg_read_burst_address.arlen,rg_read_burst_address.arsize,
                                                                    rg_read_burst_address.arburst,rg_read_burst_address.araddr);
            rg_read_burst_address.araddr <= address;
            dram.put(False,truncateLSB(address),?);
            rg_read_request <= True;
            $display($time, " DRAM Read request burst : %x arsize %x arburst %x arlen %x \n",address,rg_read_burst_address.arsize, rg_read_burst_address.arburst, rg_read_burst_address.arlen);
        end
    endrule

    rule rl_read_response(rg_read_request == True && rg_flag > 0);
        let data = dram.read;
        let resp = AXI4_Rd_Data{rresp:AXI4_OKAY, rdata:data, rlast:rg_read_burst_counter==rg_read_burst_address.arlen,ruser:0,rid:rg_read_burst_address.arid};
        s_xactor.i_rd_data.enq(resp);
        rg_read_request <= False;
        $display($time, " DRAM Read response : %x \n", data);
    endrule

    rule rl_load_instruction_finish;
        let temp <- ld_module.subifc_send_loadfinish.get;
        $display($time, " Loading values done %d \n",temp);
        if(temp == True)
            $finish;
    endrule

endmodule

(*synthesize*)
module mkload_Tb(Ifc_load_Module#(`DRAM_ADDR_WIDTH, Datawidth, `SRAM_ADDR_WIDTH, 
                                    `WBUF_INDEX, `WBUF_Bankbits, `INWIDTH,
                                    `IBUF_INDEX, `IBUF_Bankbits, `INWIDTH,
                                    `OBUF_INDEX, `OBUF_Bankbits, `OUTWIDTH, 
                                    Max_index, Max_bank, Max_data, Max_words, 20));
  let ifc();
  mk_load_Module inst1(ifc);
  return (ifc);
endmodule
