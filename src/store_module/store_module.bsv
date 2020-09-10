/* 
Author: Arjun Menon, Gokulan Ravi
Email id: c.arjunmenon@gmail.com, gokulan97@gmail.com
*/

package store_module;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import Semi_FIFOF::*;
  import ConfigReg::*;
  import UniqueWrappers::*;
  import FIFOF::*;
  import Vector::*;
  import GetPut::*;
  import pipe_mul::*;
  import isa::*;
  `include "systolic.defines"

  interface Ifc_col2im#(numeric type addr_width, numeric type data_width, 
                        numeric type sram_width, numeric type of_index,
                        numeric type of_banks, numeric type of_data,
                        numeric type of_values, numeric type st_pad);
    
    method ActionValue#(SRAMRdReq#(of_index, of_banks)) send_sram_req;
		method Action recv_sram_resp(Vector#(of_values, Bit#(of_data)) response);
    interface Put#(Store_params#(st_pad)) subifc_put_storeparams;
    interface Get#(Bool) subifc_send_store_finish;
    interface AXI4_Master_IFC#(addr_width, data_width, 0) master;
  endinterface

  module mkcol2im(Ifc_col2im#(addr_width, data_width, sram_width, of_index, of_banks, of_data, of_values, st_pad))
    provisos(
      Mul#(obuf_bytes, 8, of_data),
      Mul#(of_values, of_data, data_width),
			Log#(obuf_bytes, ofsz),
			Log#(TDiv#(data_width,8), awsz),
      Add#(addr_width, 0, `DRAM_ADDR_WIDTH),
      Add#(sram_width, 0, `SRAM_ADDR_WIDTH),
      Add#(a__, of_data, data_width),
			Add#(b__, of_index, 26)
    );
    
    let obuf_index = valueOf(of_index);
    let obuf_bankbits = valueOf(of_banks);
    let oBits = valueOf(of_data);
    let oBytes = valueOf(obuf_bytes);
    let oValues = valueOf(of_values);
    let burst_size = valueOf(awsz);

    function Tuple2#(Bit#(of_index),Bit#(of_banks)) split_address_OBUF(Bit#(sram_width) addr);
		  Bit#(of_banks) gbank = addr[obuf_bankbits-1:0];
		  Bit#(of_index) gindex = addr[obuf_index+obuf_bankbits-1:obuf_bankbits];
		  return tuple2(gindex,gbank);
  	endfunction

		function Bool is_address_within_range(SRAM_address start_addr, SRAM_address end_addr, SRAM_address query);
			return (start_addr <= query && query <= end_addr);
		endfunction

    Reg#(Maybe#(Store_params#(st_pad))) rg_params <- mkReg(tagged Invalid);
    Reg#(Bool) rg_finish_store <- mkReg(False);

		Reg#(Bool) rg_send_req <- mkReg(False);
    
    Reg#(Dim1) rg_z_cntr <- mkReg(0);
    Reg#(Dim1) rg_x_cntr <- mkReg(0);
    Reg#(Dim1) rg_y_cntr <- mkReg(0);

    FIFOF#(Tuple3#(Bool, DRAM_address, Bool)) ff_beat_len <- mkFIFOF();

    Reg#(DRAM_address) rg_dram_address <- mkReg(0);
    Reg#(SRAM_address) rg_sram_address <- mkReg(0);

    AXI4_Master_Xactor_IFC#(addr_width, data_width, 0) memory_xactor <- mkAXI4_Master_Xactor;

		rule rl_pop_write_resp(memory_xactor.o_wr_resp.first.bid == `Buffer_wreq_id && 
													 memory_xactor.o_wr_resp.first.bresp == AXI4_OKAY);
			let x<- pop_o(memory_xactor.o_wr_resp);
		endrule

		method ActionValue#(SRAMRdReq#(of_index, of_banks)) send_sram_req
      if(rg_params matches tagged Valid .params &&& rg_send_req);

      Bool first = (rg_z_cntr == truncate(params.z_size));
      Bool last = (rg_z_cntr <= fromInteger(oValues));
 
			Bit#(of_index) o_index;
      Bit#(of_banks) o_banks;
      {o_index, o_banks} = split_address_OBUF(rg_sram_address);

			Bool which_buf = is_address_within_range(`OBUF1_START, `OBUF1_END, rg_sram_address);
			Dim2 num_valid = truncate(min(fromInteger(oValues), rg_z_cntr));
    
      if(last)begin
        rg_z_cntr <= params.z_size;
        
        if(rg_y_cntr == 1 && rg_x_cntr == 1)begin
          //end of sending SRAM requests
					rg_send_req <= False;
        end
        else if (rg_y_cntr == 1) begin
          rg_x_cntr <= rg_x_cntr - 1;
          rg_y_cntr <= params.y_size;
          rg_dram_address <= rg_dram_address + zeroExtend(unpack(params.y_stride));
        end
        else begin
          rg_y_cntr <= rg_y_cntr - 1;
          rg_dram_address <= rg_dram_address + zeroExtend(unpack(params.z_stride));
        end
				//Increment index, set bank index to 0
				rg_sram_address <= ((unpack(rg_sram_address) >> (obuf_index + obuf_bankbits)) << (obuf_index + obuf_bankbits)) | zeroExtend((o_index+1) << obuf_bankbits);
      end
      else begin
        rg_z_cntr <= rg_z_cntr - fromInteger(oValues);
        rg_dram_address <= rg_dram_address + fromInteger(oValues) * fromInteger(oBytes);
				rg_sram_address <= rg_sram_address + fromInteger(oValues);//Increment bank bits
      end

      ff_beat_len.enq(tuple3(first, rg_dram_address, last));
      return SRAMRdReq{index: o_index, bank: o_banks, buffer: which_buf ? OutputBuffer1 : OutputBuffer2, num_valid: num_valid};
    endmethod

    method Action recv_sram_resp(Vector#(of_values, Bit#(of_data)) response)
      if(rg_params matches tagged Valid .params);

      Bit#(data_width) lv_data = 'b0;
      for(Integer i=0; i<oValues; i=i+1)begin
        lv_data[(i+1)*oBits-1:i*oBits] = response[i];
      end

      let {first, dram_addr, last} = ff_beat_len.first;
      ff_beat_len.deq();

      if(first) begin
        //send address packet
        AXI4_Wr_Addr#(addr_width, 0) write_addr = 
																			AXI4_Wr_Addr {awaddr: dram_addr,
																										awuser: 0,
																										awlen: ((params.z_size << valueOf(ofsz)) >> valueOf(awsz)) - 1,
																										awsize: fromInteger(burst_size),
																										awburst: 'b01,
																										awid: `Buffer_wreq_id,
																										awprot: ?};
        memory_xactor.i_wr_addr.enq(write_addr);
      end

      AXI4_Wr_Data#(data_width) write_data = AXI4_Wr_Data {wdata: lv_data, wstrb: 'b1, wlast: last, wid: `Buffer_wreq_id};
      memory_xactor.i_wr_data.enq(write_data);
    endmethod

    interface Put subifc_put_storeparams;
      method Action put(Store_params#(st_pad) params) if(rg_params matches tagged Invalid);
        rg_params <= tagged Valid params;
        rg_finish_store <= False;
        rg_send_req <= True;
				rg_dram_address <= params.dram_address;
        rg_sram_address <= params.sram_address;
        rg_z_cntr <= params.z_size;
        rg_y_cntr <= params.y_size;
        rg_x_cntr <= params.x_size;
      endmethod
    endinterface
    
    interface Get subifc_send_store_finish;
      method ActionValue#(Bool) get if(rg_params matches tagged Valid .params &&&
																			 rg_x_cntr == 1 &&& rg_y_cntr == 1 &&&
																			 !ff_beat_len.notEmpty());
        rg_params <= tagged Invalid;
        return True;
      endmethod
    endinterface
    
    interface master = memory_xactor.axi_side;
  endmodule

  (*synthesize*)
  module mkinst_col2im(Ifc_col2im#(32, 64, 26, 15, 6, 16, 4, 20));
    let ifc();
    mkcol2im _temp(ifc);
    return ifc;
  endmodule

endpackage
