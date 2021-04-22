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
                        numeric type sram_width, numeric type if_data, numeric type of_index,
                        numeric type of_banks, numeric type of_data,
                        numeric type of_values, numeric type st_pad);
    
    method ActionValue#(SRAMRdReq#(of_index, of_banks)) send_sram_req;
		method Action recv_sram_resp(Vector#(of_values, Bit#(of_data)) response);
    interface Put#(Store_params#(st_pad)) subifc_put_storeparams;
    interface Get#(Bool) subifc_send_store_finish;
    interface AXI4_Master_IFC#(addr_width, data_width, 0) master;
		method Bool send_interrupt;
  endinterface

  module mkcol2im(Ifc_col2im#(addr_width, data_width, sram_width,if_data, of_index, of_banks, of_data, of_values, st_pad))
    provisos(
      Mul#(obuf_bytes, 8, of_data),
      Mul#(of_values, of_data, data_width),
			Log#(obuf_bytes, ofsz),
			Log#(TDiv#(data_width,8), awsz),
      Mul#(data_bytes,8,data_width),
      Add#(addr_width, 0, `DRAM_ADDR_WIDTH),
      Add#(sram_width, 0, `SRAM_ADDR_WIDTH),
      Add#(a__, of_data, data_width),
			Add#(b__, of_index, 26),
      Mul#(ibuf_bytes, 8, if_data),
      Mul#(if_values, if_data, data_width),
      Log#(ibuf_bytes, ifsz),
      Div#(of_data, if_data, io_ratio),
      Mul#(of_values,io_ratio,if_values),
      Mul#(of_values,if_data,if_length),
      Mul#(if_length,io_ratio,data_width),
      Add#(c__,if_data,of_data),
      Add#(d__,if_data,if_length),
      Add#(e__,if_data,data_width),
      Add#(f__,if_length,data_width),
      Mul#(ibuf_bytes,of_values,if_data_bytes),
      Add#(g__, ibuf_bytes, if_data_bytes),
      Mul#(io_ratio, if_data_bytes, TDiv#(data_width, 8)),
      Mul#(of_values, obuf_bytes, TDiv#(data_width, 8))
    );

    
    let obuf_index = valueOf(of_index);
    let obuf_bankbits = valueOf(of_banks);
    let oBytes = valueOf(obuf_bytes);
		let oShift = valueOf(ofsz);
    let oValues = valueOf(of_values);
    let burst_size = valueOf(awsz);
    let io_ratio_val = valueOf(io_ratio);
    let iValues = valueOf(if_values);
    let iShift = valueOf(ifsz);
    let iBytes = valueOf(ibuf_bytes);

    Vector#(io_ratio,Reg#(Bit#(if_length))) rg_truncated_ifmap <- replicateM(mkReg(0));
    Reg#(Bit#(TAdd#(TLog#(io_ratio),1))) rg_truncate_count <- mkReg(0);
 
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
		
		Wire#(Bool) wr_last <- mkWire();
		Reg#(Bool) rg_interrupt <- mkReg(False);

    FIFOF#(Tuple4#(Bool, DRAM_address, Bool,Vector#(of_values, Bit#(obuf_bytes)))) ff_beat_len <- mkFIFOF();
    Vector#(io_ratio,Reg#(Bit#(if_data_bytes))) rg_wr_data_strb <- replicateM(mkReg(0));

    Reg#(DRAM_address) rg_dram_address <- mkReg(0);
    Reg#(SRAM_address) rg_sram_address <- mkReg(0);

    AXI4_Master_Xactor_IFC#(addr_width, data_width, 0) memory_xactor <- mkAXI4_Master_Xactor;

		rule rl_pop_write_resp(memory_xactor.o_wr_resp.first.bid == `Store_master); 
											//		 memory_xactor.o_wr_resp.first.bresp == AXI4_OKAY);
			let x <- pop_o(memory_xactor.o_wr_resp);
			
			//if the write response is AXI4_OKAY, do nothing. Otherwise, send a slave-error signal 
			//to the top.
			if(x.bresp == AXI4_SLVERR) begin
				rg_interrupt <= True;
			end

		endrule

		method ActionValue#(SRAMRdReq#(of_index, of_banks)) send_sram_req
      if(rg_params matches tagged Valid .params &&& rg_send_req);

      Bool first = (rg_z_cntr == truncate(params.z_size));
      Bool last = (rg_z_cntr <= fromInteger(oValues));
      Vector#(of_values,Bit#(obuf_bytes)) data_strobe = unpack(0);
      for(Integer i=0; i<oValues; i=i+1) begin
        if(rg_z_cntr > fromInteger(i))
          data_strobe[i] = -1;
        else
          data_strobe[i] = 0;
      end
 
			Bit#(of_index) o_index;
      Bit#(of_banks) o_banks;
      {o_index, o_banks} = split_address_OBUF(rg_sram_address);

			Bool which_buf = is_address_within_range(`OBUF1_START, `OBUF1_END, rg_sram_address);
			Sram_valid num_valid = truncate(min(fromInteger(oValues), rg_z_cntr));
    
      if(last)begin
        rg_z_cntr <= params.z_size;
        
        if(rg_y_cntr == 1 && rg_x_cntr == 1)begin
          //end of sending SRAM requests
					rg_send_req <= False;
        end
        else if (rg_y_cntr == 1) begin
          rg_x_cntr <= rg_x_cntr - 1;
          rg_y_cntr <= params.y_size;
          rg_dram_address <= rg_dram_address + zeroExtend(unpack(params.y_stride) << (params.bitwidth ? iShift : oShift));
        end
        else begin
          rg_y_cntr <= rg_y_cntr - 1;
          rg_dram_address <= rg_dram_address + zeroExtend(unpack(params.z_stride) << (params.bitwidth ? iShift : oShift));
        end
				//Increment index, set bank index to 0
				rg_sram_address <= ((unpack(rg_sram_address) >> (obuf_index + obuf_bankbits)) << (obuf_index + obuf_bankbits)) | zeroExtend((o_index+1) << obuf_bankbits);
      end
      else begin
        rg_z_cntr <= rg_z_cntr - fromInteger(oValues);
        rg_dram_address <= rg_dram_address + fromInteger(oValues) << (params.bitwidth ? iShift : oShift);
				rg_sram_address <= rg_sram_address + fromInteger(oValues);
      end

      ff_beat_len.enq(tuple4(first, rg_dram_address, last, data_strobe));
      return SRAMRdReq{index: o_index, bank: o_banks, buffer: which_buf ? OutputBuffer1 : OutputBuffer2, num_valid: num_valid};
    endmethod

    method Action recv_sram_resp(Vector#(of_values, Bit#(of_data)) response)
      if(rg_params matches tagged Valid .params);


      let {first, dram_addr, last, data_strobe} = ff_beat_len.first;
			wr_last <= last;
      ff_beat_len.deq();

      if(first) begin
        //send address packet
        Integer shift_len = params.bitwidth ? iShift : oShift;
        Bit#(TAdd#(TAdd#(ofsz,1),`DIM_WIDTH1)) lv_shift = zeroExtend(params.z_size) << shift_len;
        Bit#(8) shift_op = truncate(lv_shift >> valueOf(awsz));
        Bit#(8) burst_len = shift_op >= fromInteger(valueOf(data_bytes))  ? (shift_op - 1) : 0;
        AXI4_Wr_Addr#(addr_width, 0) write_addr = 
																			AXI4_Wr_Addr {awaddr: dram_addr,
																										awuser: 0,
																										awlen: burst_len,
																										awsize: fromInteger(burst_size),
																										awburst: 'b01,
																										awid: `Buffer_wreq_id,
																										awprot: ?};
        memory_xactor.i_wr_addr.enq(write_addr);
      end
      Bit#(data_width) lv_data = 'b0;
      if(params.bitwidth) begin
        Bit#(if_data_bytes) lv_data_strobe = 'b0;
        for(Integer i=0; i<oValues; i=i+1)begin
          Bit#(if_data) lv_truncate_data = truncate(response[i]);
          lv_truncate_data[valueOf(if_data)-1] = lv_truncate_data[valueOf(if_data)-1] | response[i][valueOf(of_data)-1];
          lv_data[(i+1)*valueOf(if_data)-1:i*valueOf(if_data)] = lv_truncate_data;
          Bit#(ibuf_bytes) lv_temp = (data_strobe[i] == 0) ? 0 : -1;
          lv_data_strobe[(i+1)*valueOf(ibuf_bytes)-1:i*valueOf(ibuf_bytes)] = lv_temp;
        end
        if(rg_truncate_count < fromInteger(io_ratio_val) || !last) begin
          rg_truncated_ifmap[rg_truncate_count] <= truncate(lv_data);
          rg_wr_data_strb[rg_truncate_count] <= lv_data_strobe;
        end
        else begin
          rg_truncate_count <= 0;
          Vector#(io_ratio,Bit#(if_length)) lv_resp;
          Vector#(io_ratio,Bit#(if_data_bytes)) lv_wr_strb;
          for(Integer i=0; i<io_ratio_val; i=i+1) begin
            lv_resp[i] = rg_truncated_ifmap[i];
            lv_wr_strb[i] = rg_wr_data_strb[i];
            rg_truncated_ifmap[i] <= 0;
            rg_wr_data_strb[i] <= 0;
          end
          lv_resp[rg_truncate_count] = truncate(lv_data);
          lv_data = pack(lv_resp);
          lv_wr_strb[rg_truncate_count] = lv_data_strobe;
          AXI4_Wr_Data#(data_width) write_data = AXI4_Wr_Data {wdata: lv_data, wstrb: pack(lv_wr_strb), wlast: last, wid: `Buffer_wreq_id};
          memory_xactor.i_wr_data.enq(write_data);    
        end
      end
      else begin
        lv_data = pack(response);
        AXI4_Wr_Data#(data_width) write_data = AXI4_Wr_Data {wdata: lv_data, wstrb: pack(data_strobe), wlast: last, wid: `Buffer_wreq_id};
        memory_xactor.i_wr_data.enq(write_data);  
      end

    endmethod

		method Bool send_interrupt = rg_interrupt;

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
																			 wr_last);
        rg_params <= tagged Invalid;
        return True;
      endmethod
    endinterface
    
    interface master = memory_xactor.axi_side;
  endmodule

  //(*synthesize*)
  //module mkinst_col2im(Ifc_col2im#(32, 128, 26, 8, 6, 4, 32, 4, 20));
  //  let ifc();
  //  mkcol2im _temp(ifc);
  //  return ifc;
  //endmodule

endpackage
