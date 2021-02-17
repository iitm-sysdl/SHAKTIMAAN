package compute;

  import FIFOF::*;
  import Vector::*;
  import isa::*;
  import GetPut::*;
  import systolic::*;
	import DReg::*;
	import array::*;
	`include "systolic.defines"
	
  (*synthesize*)
  module mkgemm_Tb(Ifc_compute_module#(32,26,8,16,4,4,5,6,7,18));
    let ifc();
    mkgemm inst1(ifc);
    return (ifc);
  endmodule

  interface Ifc_compute_module#(numeric type dram_addr_width, numeric type sram_addr_width,
                               numeric type in_width, numeric type out_width,
                               numeric type nRow, numeric type nCol,
                               numeric type if_index,
                               numeric type wt_index,
                               numeric type of_index, numeric type cp_pad);
    interface Put#(Compute_params#(if_index, of_index, wt_index, cp_pad)) subifc_put_compute_params;//
		interface Get#(Bool) subifc_get_compute_finish;
		interface Get#(Tuple2#(SRAM_index#(if_index), Dim1)) get_inp_addr;
    interface Vector#(nRow, Put#(Bit#(in_width))) put_inp_resp;
    interface Get#(Tuple2#(Bit#(wt_index), Dim1)) get_wt_addr;//
    method Action put_wt_resp(Vector#(nCol, Bit#(in_width)) weights);//
    interface Get#(Tuple2#(Bit#(of_index), Dim1)) get_old_out_addr;
    interface Vector#(nCol, Put#(Bit#(out_width))) put_old_out_resp;
		interface Get#(Tuple3#(Vector#(nCol, Bit#(out_width)), Bit#(of_index), Dim1)) get_new_output_data;
  endinterface

  module mkgemm(Ifc_compute_module#(dram_addr_width, sram_addr_width,
                                   in_width, out_width,
                                   nRow, nCol, if_index, wt_index, of_index, cp_pad))
    provisos(//Add#(dram_addr_width, 0, `DRAM_ADDR_WIDTH),
             //Add#(sram_addr_width, 0, `SRAM_ADDR_WIDTH),
             Mul#(ibytes, 8, in_width),
             Mul#(wbytes, 8, in_width),
             Mul#(obytes, 8, out_width),
             //Add#(a__, in_width, TMul#(in_width, 2),
						 Add#(b__, in_width, out_width),
						 //provisos for compiler
						 Add#(4, a__, if_index),
						 Add#(8, c__, wt_index)
             );

    let ibuf_index = valueOf(if_index);
    
    let wbuf_index = valueOf(wt_index);

    let obuf_index = valueOf(of_index);

    let rows = valueOf(nRow);
    let cols = valueOf(nCol);

    let iBytes = valueOf(ibytes);
    let wBytes = valueOf(wbytes);
    let oBytes = valueOf(obytes);
  
		Ifc_array#(nRow, nCol, in_width, out_width) systolic <- mk_array;

		Reg#(Compute_params#(if_index, of_index, wt_index, cp_pad)) rg_params <- mkReg(?);
		Reg#(Bool) rg_valid <- mkReg(False);

		Reg#(Bool) rg_wgt_req <- mkReg(False);
		Reg#(Bool) rg_wgt_res <- mkReg(False);
		Reg#(Dim1) rg_wgt_cntr <- mkReg(0);
		
		Reg#(Bool) rg_inp_req <- mkReg(False);
		Reg#(Bool) rg_pad_zro <- mkDReg(False);

		Reg#(Bit#(wt_index)) rg_wgt_addr <- mkReg(?);
	
		Reg#(Dim1) rg_h_cntr <- mkReg(0);
		Reg#(Dim1) rg_w_cntr <- mkReg(0);

		Reg#(Bit#(if_index)) rg_inp_row_addr <- mkReg(?);
		Reg#(Bit#(if_index)) rg_inp_col_addr <- mkReg(?);

		Reg#(Bit#(of_index)) rg_old_out_addr <- mkReg(?);
		Reg#(Bit#(of_index)) rg_new_out_addr <- mkReg(?);

		Vector#(nCol, FIFOF#(Bit#(out_width))) ff_old_outputs <- replicateM(mkFIFOF());
		Vector#(nCol, FIFOF#(Bit#(out_width))) ff_new_outputs <- replicateM(mkFIFOF());
	
		rule rl_get_outputs;
			for(Integer i=0; i<cols; i=i+1)begin
				let value <- systolic.ifc_get_out[i].get();
				ff_new_outputs[i].enq(value);
			end
		endrule

		Vector#(nRow, Put#(Bit#(in_width))) ifc_put_input;
    for(Integer i=0; i<rows; i=i+1)begin
      ifc_put_input[i] = (
        interface Put;
          method Action put(Bit#(in_width) value) if(!rg_pad_zro);
            systolic.ifc_put_inp[i].put(value);
          endmethod
        endinterface
      );
    end

		Vector#(nCol, Put#(Bit#(out_width))) ifc_put_old_out;
		for(Integer i=0; i<cols; i=i+1)begin
			ifc_put_old_out[i] = (
				interface Put;
					method Action put(Bit#(out_width) value);
						ff_old_outputs[i].enq(value);
					endmethod
				endinterface
			);
		end
	
		//Vector#(nCol, Get#(Bit#(out_width))) ifc_get_new_out;
		//for(Integer i=0; i<cols; i=i+1)begin
		//	ifc_get_new_out[i] = (
		//		interface Get;
		//			method ActionValue#(Bit#(out_width)) get;
		//				Bit#(out_width) val1 = rg_params.preload_output ? ff_old_outputs[i].first : 0;
		//				Bit#(out_width) val2 = ff_new_outputs[i].first;
		//				if(rg_params.preload_output)
		//					ff_old_outputs[i].deq();
		//				ff_new_outputs[i].deq();
		//				return val1+val2;
		//			endmethod
		//		endinterface
		//	);
		//end
	
		method Action put_wt_resp(Vector#(nCol, Bit#(in_width)) weights);
			for(Integer i=0; i<cols; i=i+1)begin
				Int#(8) coord = fromInteger(rows) - 1 - unpack(rg_params.active_rows) + unpack(rg_wgt_cntr);
				systolic.ifc_put_wgt[i].put(tuple2(weights[i], coord));
			end
			rg_wgt_cntr <= rg_wgt_cntr + 1;
		endmethod
		
		interface put_inp_resp = ifc_put_input;
		interface put_old_out_resp = ifc_put_old_out;
		//interface get_new_output_data = ifc_get_new_out;

		interface Get get_new_output_data;
			method ActionValue#(Tuple3#(Vector#(nCol, Bit#(out_width)), Bit#(of_index), Dim1)) get;
				Vector#(nCol, Bit#(out_width)) values = replicate(0);
				for(Integer i=0; i<cols; i=i+1)begin
					Bit#(out_width) val1 = rg_params.preload_output ? ff_old_outputs[i].first : 0;
					Bit#(out_width) val2 = ff_new_outputs[i].first;
					if(rg_params.preload_output)
						ff_old_outputs[i].deq();
					ff_new_outputs[i].deq();
					values[i] =  val1+val2;
				end
				return tuple3(values, rg_new_out_addr, rg_params.active_cols);
			endmethod
		endinterface

		interface Get get_wt_addr;
			method ActionValue#(Tuple2#(Bit#(wt_index), Dim1)) get if(rg_valid && rg_wgt_req);
				rg_wgt_addr <= rg_wgt_addr - 1;
				if(rg_wgt_addr == rg_params.weight_address)
					rg_wgt_req <= False;
				return tuple2(rg_wgt_addr, rg_params.active_cols);
			endmethod
		endinterface
		
		interface Get get_inp_addr;
			method ActionValue#(Tuple2#(Bit#(if_index), Dim1)) get if(rg_valid);

				Bool lv_pad_zero = (rg_h_cntr < zeroExtend(rg_params.pad_top)) || (rg_w_cntr < zeroExtend(rg_params.pad_left)) 
                    || (rg_params.ofmap_height - rg_h_cntr < zeroExtend(rg_params.pad_bottom))
                    || (rg_params.ofmap_width - rg_w_cntr < zeroExtend(rg_params.pad_right));

				rg_pad_zro <= lv_pad_zero;

				if(rg_h_cntr == rg_params.ofmap_height-1 && rg_w_cntr == rg_params.ofmap_width-1)
					rg_inp_req <= False;
				else if(rg_w_cntr == rg_params.ofmap_width-1)begin
					rg_w_cntr <= 0;
					rg_h_cntr <= rg_h_cntr + 1;
					SRAM_index#(if_index) lv_addr = rg_inp_row_addr + zeroExtend(rg_params.stride_h);
					rg_inp_row_addr <= lv_addr;
					rg_inp_col_addr <= lv_addr;
				end
				else begin
					rg_w_cntr <= rg_w_cntr + 1;
					rg_inp_col_addr <= rg_inp_col_addr + zeroExtend(rg_params.stride_w);
				end
				return tuple2(rg_inp_col_addr, lv_pad_zero ? 0 : rg_params.active_rows);
			endmethod
		endinterface
		
		interface Get get_old_out_addr;
			method ActionValue#(Tuple2#(Bit#(of_index), Dim1)) get if(rg_valid && rg_params.preload_output);
				rg_old_out_addr <= rg_old_out_addr + 1;
				return tuple2(rg_old_out_addr, rg_params.active_cols);
			endmethod
		endinterface

		interface Put subifc_put_compute_params;
			method Action put(Compute_params#(if_index, of_index, wt_index, cp_pad) params) if(!rg_valid);
				rg_params <= params;
				rg_valid <= True;

				rg_wgt_req <= True;
				rg_wgt_res <= True;

				rg_wgt_addr <= params.weight_address + zeroExtend(params.active_rows-1);
				rg_wgt_cntr <= 0;

				rg_h_cntr <= 0;
				rg_w_cntr <= 0;

				rg_inp_row_addr <= params.input_address;
				rg_inp_col_addr <= params.input_address;

				rg_old_out_addr <= params.output_address;
				rg_new_out_addr <= params.output_address;
			endmethod
		endinterface
	endmodule
endpackage
