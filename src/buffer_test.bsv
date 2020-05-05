package buffer_test;

import onchip_buffers::*;
import isa::*;
import Vector::*;
import BRAM::*;
import BRAMCore::*;
`include "systolic.defines"

module mkbuffer_test(Empty);

Ifc_onchip_buffers buffers <- mkbuffers;

Reg#(Bit#(4)) cntr1 <- mkReg(0);
Reg#(SRAM_address) rg_sram_addr1 <- mkReg(0);
Reg#(SRAM_address) rg_sram_addr2 <- mkReg(0);
Reg#(SRAM_address) rg_sram_addr3 <- mkReg(0);


rule rl_write_buf(cntr1 < 5);

	Bit#(64) lv_data = 64'h8765432187654321;
	
	let {inp_index, bufferbank} = split_address_IBUF(rg_sram_addr1);

	for(Integer i=0; i<8; i=i+1)begin
     Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
     Bit#(`IBUF_INDEX) index = inp_index;
     Bit#(`IBUF_Bankbits) bank = truncate(m);
     if(bank < bufferbank)begin 
       index = index + 1;
     end
     Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
     $display($time, "sending write to ibuf bank %d index %h data %h \n",bank,index,data);
     buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
    end

    rg_sram_addr1 <= rg_sram_addr1 + 8;
    cntr1 <= cntr1 + 1;

endrule

rule rl_read_req(cntr1 >= 5 && cntr1 <7);

    let {inp_index, bufferbank} = split_address_IBUF(rg_sram_addr2);

    for(Integer i=0; i<8; i=i+1)begin
     Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
     Bit#(`IBUF_INDEX) index = inp_index;
     Bit#(`IBUF_Bankbits) bank = truncate(m);
     if(bank < bufferbank)begin 
       index = index + 1;
     end
     $display($time, "sending read to ibuf bank %d index %h \n",bank,index);
     buffers.input_buffer[0][bank].portB.request.put(makeRequest(False, 0 , index, ?));
    end

    rg_sram_addr2 <= rg_sram_addr2 + 8;

    cntr1 <= cntr1 + 1;
    
endrule

rule rl_get_val(cntr1 >5);

    let {inp_index, bufferbank} = split_address_IBUF(rg_sram_addr3);

    Bit#(64) val = 0;
    for(Integer i=0; i<8; i=i+1)begin
     Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
     Bit#(`IBUF_INDEX) index = inp_index;
     Bit#(`IBUF_Bankbits) bank = truncate(m);
     if(bank < bufferbank)begin 
       index = index + 1;
     end
     let temp <- buffers.input_buffer[0][bank].portB.response.get();
     $display($time,"data recieved %h bank %d index %h \n ",temp,bank,index);
     val = (val << `INWIDTH) | zeroExtend(temp);
    end

    $display($time,"data recieved %h\n",val);

    // let lv_addr = rg_sram_addr2 + 8;

    // let {inp_index2, bufferbank2} = split_address_IBUF(lv_addr);

    // for(Integer i=0; i<8; i=i+1)begin
    //  Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank2) + fromInteger(i);
    //  Bit#(`IBUF_INDEX) index = inp_index2;
    //  Bit#(`IBUF_Bankbits) bank = truncate(m);
    //  if(bank < bufferbank2)begin 
    //    index = index + 1;
    //  end
    //  $display($time, "sending read to ibuf bank %d index %h \n",bank,index);
    //  buffers.input_buffer[0][bank].portB.request.put(makeRequest(False, 0 , index, ?));
    // end


    rg_sram_addr3 <= rg_sram_addr3 + 8;

endrule


endmodule

endpackage

	