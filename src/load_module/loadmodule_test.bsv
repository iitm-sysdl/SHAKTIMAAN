/* 
Author: Mohan Prasath G R
Email id: mohanprasathr@gmail.com
Details: Verifying loadmodule. Added a AXI Fabric and instantiated a AXI slave which acts as a DRAM
 and responds to the read requests from the AXI master(load module)
*/


package loadmodule_test;

import Semi_FIFOF:: *;
import AXI4_Types:: *;
import AXI4_Fabric:: *;
`include "systolic.defines"
import Connectable::*;
import isa::*;
import FIFOF::*;
import onchip_buffers::*;
import BRAM::*;
import BRAMCore::*;



`define addr_width 32
// typedef 32 addr_width;
`define data_width 64
`define Num_Slaves 1
`define rd_req_id 5
`define DIM_WIDTH1 12
`define DIM_WIDTH3 36
`define numWords `data_width/`INWIDTH

typedef struct {
  DRAM_address dram_address;
  SRAM_address sram_address;
  Dim1 x_size; Dim1 y_size; Dim1 z_size;
  Dim1 z_stride; Dim1 y_stride;
  Bool is_reset;
} Mem_params deriving(Bits,Eq);
typedef Mem_params Load_params;


function Bit#(TLog#(`Num_Slaves)) fn_slave_map (Bit#(`addr_width) addr);
  Bit#(TLog#(`Num_Slaves)) slave_num = 0;
  if(addr >= 'h20000 && addr<= 'h30000)
    slave_num = 0;
  return slave_num;
endfunction

(*synthesize*)
module mkloadmodule_test(Empty)
		provisos(//Literal#(loadmodule_test::Mem_params)
			);

	AXI4_Fabric_IFC #(1, 1, `addr_width, `data_width, 0) fabric <- mkAXI4_Fabric(fn_slave_map);

    AXI4_Slave_Xactor_IFC#(`addr_width, `data_width, 0) s_xactor  <- mkAXI4_Slave_Xactor;
  	AXI4_Master_Xactor_IFC #(`addr_width, `data_width, 0) m_xactor <- mkAXI4_Master_Xactor;

    mkConnection(m_xactor.axi_side, fabric.v_from_masters[0]);
    mkConnection(fabric.v_to_slaves[0], s_xactor.axi_side);

   	FIFOF#(Bit#(`addr_width)) ff_dest_addr <- mkSizedFIFOF(3) ;

	Ifc_onchip_buffers buffers <- mkbuffers;

    //parameters registers

	Reg#(Bit#(3)) rg_bitwidth <- mkReg(0); // in bytes
	Reg#(SRAM_address) rg_zy_size <- mkReg(0); //zsize * ysize
	Reg#(SRAM_address) rg_xyz_size <- mkReg(0);
	Reg#(Dim1) rg_x_size <- mkReg(0); 
	Reg#(Dim1) rg_y_size <- mkReg(0);
	Reg#(Dim1) rg_z_size <- mkReg(0);
	// Reg#(Load_params) rg_load_params <- mkReg(0);

    //local registers
	Reg#(DRAM_address) rg_dram_addr_a <- mkReg(0);
	Reg#(DRAM_address) rg_dram_addr_b <- mkReg(0);
	Reg#(SRAM_address) rg_sram_addr_a <- mkReg(0);
	Reg#(SRAM_address) rg_sram_addr_b <- mkReg(0);
	Reg#(Dim1) rg_y_cntr <- mkReg(0);


	Reg#(Bit#(8)) rg_burst_len <- mkReg(0); // (total no.of bytes per request) / (axidatawidth) - 1
	Reg#(Bool) rg_burst <- mkReg(False);
	Reg#(Bit#(`addr_width)) rg_burst_addr <- mkReg(0);
	Reg#(Bool) rg_finish_load <- mkReg(True);

	//Registers for verification
	Reg#(Bool) rg_status1 <- mkReg(True);
	Reg#(Bool) rg_sl_status <- mkReg(True);
	Reg#(Bit#(8)) rg_slave_burst <- mkReg(0);
	Reg#(Bit#(8)) rg_slave_burstlen <- mkReg(0);
	Reg#(Bit#(`addr_width)) rg_slave_addr <- mkReg(0);
	Reg#(Bit#(4)) rg_slave_rid <- mkReg(0);

	Reg#(Bool) rg_isreset <- mkReg(False);
	Reg#(Dim1) rg_z_stride <- mkReg(0);
	Reg#(Dim1) rg_y_stride <- mkReg(0);
	Reg#(DRAM_address) rg_dramaddr <- mkReg(0);
	Reg#(SRAM_address) rg_sramaddr <- mkReg(0);




	rule rl_get_parambuffer(rg_status1); 

		let temp = 128'h00000100000020000008008080180C00;  //Test load instruction
		// rg_load_params <= truncate(temp);

		rg_bitwidth <= `INBYTES; //yet to be defined based on sram address
		rg_sram_addr_b <= temp[86:61]; //sram base address
		rg_dram_addr_b <= temp[118:87]; //dram base address
		rg_burst_len <= truncate(((temp[36:25] *(`INBYTES)) >> 3)-1); //temp[24:17] - z_size 
		rg_zy_size <= zeroExtend(temp[36:25] * temp[48:37]); //z_size * y_size
		rg_x_size <= temp[60:49];
		rg_y_size <= temp[48:37];
		rg_z_size <= temp[36:25];
		rg_xyz_size <= zeroExtend(temp[36:25] * temp[48:37] * temp[60:49]);
		rg_isreset <= temp[0]==1'b1;
		rg_sramaddr <= temp[86:61];
		rg_dramaddr <= temp[118:87];
		rg_z_stride <= temp[24:13];
		rg_y_stride <= temp[12:1];
		rg_status1 <= False;
		$display($time,"sram %h dram %h burstlen %d x_size %d y_size %d z_size %d",temp[86:61],temp[118:87],(((temp[36:25] *(`INBYTES)) >> 3)-1),temp[60:49],temp[48:37],temp[36:25]);

	endrule

	// rule rl_load_immediate(rg_isreset && rg_xyz_size !=0 ); 

	// 	// load immediate into input buffer -- have to do similarly for weight and output if necessary
	// 	let {inp_index, inp_bufferbank} = split_address_IBUF(rg_sram_addr_b);

	// 	Bit#(26) looplimit = min(`IBUF_BANKS, rg_xyz_size);
	// 	let loop = valueOf(looplimit);

	// 	for(Integer i=0; i<loop; i=i+1)begin
 //             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(inp_bufferbank) + fromInteger(i);
 //             Bit#(`IBUF_INDEX) index = inp_index;
 //             Bit#(`IBUF_Bankbits) bank = truncate(m);
 //             if(bank < inp_bufferbank)begin 
 //               index = index + 1;
 //             end
 //             Bit#(`INWIDTH) data = truncate(rg_dram_addr_b);
 //             buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));  //buffer yet to be instantiated
 //        end

 //        rg_xyz_size <= rg_xyz_size - (looplimit);
 //        rg_sram_addr_b <= rg_sram_addr_b + truncate(looplimit * `INBYTES);

 //        if(looplimit == rg_xyz_size) rg_finish_load <= True; //end of load immmediate

	// endrule

	//LOAD from DRAM to SRAM
	rule rl_start_dram_Read( !(rg_x_size ==0 && rg_y_cntr==0) && !rg_isreset);  

		if(rg_y_cntr == (rg_y_size-1)) begin //----- end of a 2D slice -----
			
			rg_dram_addr_a <= rg_dram_addr_a + zeroExtend(rg_y_stride * zeroExtend(rg_bitwidth));
			rg_dram_addr_b <= rg_dramaddr;
			rg_sram_addr_a <= rg_sram_addr_a + truncate(rg_zy_size * zeroExtend(rg_bitwidth));
			rg_sram_addr_b <= rg_sramaddr;
			rg_y_cntr <= 0;
			rg_x_size <= rg_x_size - 1;
		end
		else begin
			rg_dram_addr_b <= rg_dram_addr_b + zeroExtend(rg_z_stride * zeroExtend(rg_bitwidth));
			rg_sram_addr_b <= rg_sram_addr_b + zeroExtend(rg_z_size * zeroExtend(rg_bitwidth));
			rg_y_cntr <= rg_y_cntr + 1;
		end

		Bit#(`addr_width) lv_dram_addr = rg_dram_addr_a + rg_dram_addr_b;
		Bit#(`addr_width) lv_sram_addr = zeroExtend(rg_sram_addr_a + rg_sram_addr_b);

		ff_dest_addr.enq(lv_sram_addr);

		$display($time,"sending req dramaddr %d sramaddr %d",lv_dram_addr,lv_sram_addr);

		let read_request = AXI4_Rd_Addr {araddr: lv_dram_addr, 
                         arid: {`rd_req_id}, arlen: rg_burst_len,
                         arsize: (3'b011), arburst: 'b01, //arburst: 00-FIXED 01-INCR 10-WRAP
                         aruser: 0 };  

        m_xactor.i_rd_addr.enq(read_request);

	endrule

	rule rl_start_write(m_xactor.o_rd_data.first.rid == `rd_req_id && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                         && !rg_burst);

		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		let lv_sram_addr = ff_dest_addr.first;

		/* ----code for loading into buffer----*/

			let {inp_index, bufferbank} = split_address_IBUF(lv_sram_addr);

			for(Integer i=0; i<`numWords; i=i+1)begin
             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
             Bit#(`IBUF_INDEX) index = inp_index;
             Bit#(`IBUF_Bankbits) bank = truncate(m);
             if(bank < bufferbank)begin 
               index = index + 1;
             end
             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
             buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
            end

		$display($time,"recieved data %h sramaddr %d rlast %b",lv_data,lv_sram_addr,lv_resp.rlast);
		
		if(!lv_resp.rlast) begin
			rg_burst <= True;
			let lv_burst_addr = lv_sram_addr + (`data_width>>3); 
			rg_burst_addr <= lv_burst_addr;
		end

		ff_dest_addr.deq;  //dequeing this FIFO will cause start_Mem_Read to fire.

	endrule

	rule rl_burst_load(m_xactor.o_rd_data.first.rid == `rd_req_id && m_xactor.o_rd_data.first.rresp==AXI4_OKAY
                        && rg_burst);

		let lv_resp <- pop_o(m_xactor.o_rd_data);
		let lv_data = lv_resp.rdata;
		let lv_sram_addr = rg_burst_addr;

		$display($time,"recieved data %h sramaddr %d rlast %b",lv_data,lv_sram_addr,lv_resp.rlast);


		/* ----code for loading into buffer----*/

			let {inp_index, bufferbank} = split_address_IBUF(lv_sram_addr);

			for(Integer i=0; i<`numWords; i=i+1)begin
             Bit#(TAdd#(`IBUF_Bankbits,1)) m = zeroExtend(bufferbank) + fromInteger(i);
             Bit#(`IBUF_INDEX) index = inp_index;
             Bit#(`IBUF_Bankbits) bank = truncate(m);
             if(bank < bufferbank)begin 
               index = index + 1;
             end
             Bit#(`INWIDTH) data = lv_data[(i+1)*`INWIDTH-1:i*`INWIDTH];
             buffers.input_buffer[0][bank].portB.request.put(makeRequest(True, 'b1 , index, data));
            end


		let lv_burst_addr = lv_sram_addr + (`data_width>>3); // or (data_width)/(bitwidth)?
		rg_burst_addr <= lv_burst_addr;

		if(lv_resp.rlast) begin // last beat of current burst
			rg_burst <= False;
			if(rg_x_size==0 && rg_y_cntr==0) begin // end of current load instruction
				rg_finish_load <= True;
				rg_sl_status <= True;
			end
		end
		
	endrule

	rule rl_slave_write(rg_sl_status);

		let ar <- pop_o(s_xactor.o_rd_addr);
		Bit#(`addr_width) lv_addr = ar.araddr;
		Bit#(`data_width) val = (zeroExtend(lv_addr) << 32);

		let wr_data = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: val, rlast:
      				ar.arlen==0, ruser: 0, rid: ar.arid};
      	$display($time, "sending data %h dram_addr %d",val, lv_addr);

      	s_xactor.i_rd_data.enq(wr_data);		

		rg_slave_addr <= lv_addr + (`data_width >> 3);
		rg_slave_burstlen <= ar.arlen;
		rg_slave_rid <= ar.arid;
		
		if(ar.arlen!=0) begin
			rg_slave_burst <= 'd1;
			rg_sl_status <= False;
		end

	endrule

	rule rl_slave_burst(!rg_sl_status);

		let lv_last = (rg_slave_burst == rg_slave_burstlen);

		Bit#(`data_width) val = (zeroExtend(rg_slave_addr) << 32);

		let wr_data = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: val, rlast:
      				lv_last, ruser: 0, rid: rg_slave_rid};
      	$display($time, "sending data %h dram_addr %d",val, rg_slave_addr);

      	s_xactor.i_rd_data.enq(wr_data);		

		rg_slave_addr <= rg_slave_addr + (`data_width>> 3);
		rg_slave_burst <= rg_slave_burst + 1;

		if(lv_last) begin
			rg_sl_status <= True;
		end

	endrule

endmodule

endpackage




