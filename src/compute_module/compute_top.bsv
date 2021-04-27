/*
Author: Gokulan Ravi
Email ID: gokulan97@gmail.com
*/

package compute_top;

  import FIFOF::*;
  import Vector::*;
  import isa::*;
  import GetPut::*;
  import systolic::*;
	`include "systolic.defines"

  interface Ifc_compute_module#(numeric type dram_addr_width, numeric type sram_addr_width,
                               numeric type in_width, numeric type out_width,
                               numeric type nRow, numeric type nCol,
                               numeric type if_index,
                               numeric type wt_index,
                               numeric type of_index, numeric type cp_pad);
    interface Put#(Compute_params#(if_index, of_index, wt_index, cp_pad)) subifc_put_compute_params;
    interface Get#(Bool) subifc_get_compute_finish;
		interface Vector#(nRow, Get#(SRAMKRdReq#(if_index))) get_inp_addr;
    interface Vector#(nRow, Put#(Bit#(in_width))) put_inp_resp;
    method ActionValue#(Tuple2#(SRAM_index#(wt_index), Dim1)) get_wt_addr;
    method Action put_wt_resp(Vector#(nCol, Bit#(in_width)) weights);
    interface Vector#(nCol, Get#(SRAMKRdReq#(of_index))) get_old_out_addr;
    interface Vector#(nCol, Put#(Bit#(out_width))) put_old_out_resp;
    interface Vector#(nCol, Get#(SRAMKWrReq#(of_index, out_width))) get_new_output_data;
  endinterface

//  (*synthesize*)
//  module mkgemm_Tb(Ifc_compute_module#(32,26,8,16,4,4,5,6,7,18));
//    let ifc();
//    mkgemm inst1(ifc);
//    return (ifc);
//  endmodule
  
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
    
		Reg#(Bool) rg_which_buffer <- mkReg(False);

    Reg#(Maybe#(Compute_params#(if_index, of_index, wt_index, cp_pad))) rg_params <- mkReg(tagged Invalid);
    Reg#(Bool) rg_weightload <- mkReg(False);
		Reg#(Bool) rg_weightload_req <- mkReg(False);

    Reg#(SRAM_index#(wt_index)) rg_wt_addr <- mkReg(?);
    
    Reg#(SRAM_index#(if_index)) rg_inp_row_addr <- mkReg(?);
    Reg#(SRAM_index#(if_index)) rg_inp_col_addr <- mkReg(?);

    Reg#(Dim1) rg_inp_traingle_cntr <- mkReg(0);
    Reg#(Dim1) rg_h_cntr <- mkReg(0);
    Reg#(Dim1) rg_w_cntr <- mkReg(0);
    Reg#(Dim1) rg_zero_cntr <- mkReg(0);

    Reg#(Dim1) rg_wt_cntr <- mkReg(0);

    Wire#(SRAM_index#(wt_index)) wr_wt_req <- mkWire();

    Vector#(nRow, Reg#(SRAMKRdReq#(if_index))) rg_inp_addr <- replicateM(mkReg(?));
    Vector#(nRow, Wire#(SRAMKRdReq#(if_index))) wr_inp_addr <- replicateM(mkWire());
		Vector#(nRow, Reg#(Bool)) rg_valid_row <- replicateM(mkReg(False));

		Reg#(Bit#(of_index)) rg_old_out_addr <- mkReg(?);
    Vector#(nCol, Reg#(SRAMKRdReq#(of_index))) rg_old_out_req <- replicateM(mkReg(?));
		Vector#(nCol, Wire#(SRAMKRdReq#(of_index))) wr_old_out_req <- replicateM(mkWire());

		Reg#(Dim1) rg_new_out_cntr <- mkReg(0);

    Vector#(nCol, Reg#(Bit#(of_index))) rg_new_out_addr <- replicateM(mkReg(?));

    Vector#(nCol, Reg#(Bool)) rg_valid_col <- replicateM(mkReg(False));

    FIFOF#(Dim1) ff_wt_coord <- mkFIFOF();
    FIFOF#(Dim1) ff_inp_count <- mkFIFOF();

    Ifc_systolic#(nRow, nCol, in_width, out_width) systolic <- mksystolic; 

    rule rl_send_wt_req(rg_params matches tagged Valid .params &&&
                        rg_weightload_req);

			wr_wt_req <= rg_wt_addr;
    endrule

    rule rl_send_init_acc_zero(rg_params matches tagged Valid .params &&&
                                 !rg_weightload &&&
                                 rg_zero_cntr > 0 &&&
                                 !params.preload_output);
      for(Integer i=0; i<cols; i=i+1)begin
        if(rg_valid_col[i])begin
          systolic.subifc_cols[i].subifc_put_acc.put(0);
        end
      end
			rg_zero_cntr <= rg_zero_cntr - 1;
    endrule

		rule rl_generate_inp_addr(rg_params matches tagged Valid .params &&& 
															!rg_weightload &&& // compute phase
															((rg_h_cntr == params.ofmap_height-1 && 
															rg_w_cntr == params.ofmap_width-1) || // Input feeding phase
															rg_inp_traingle_cntr > 1)); //Final triangle while feeding inputs

      Bool lv_pad_zero = (rg_h_cntr < zeroExtend(params.pad_top)) || (rg_w_cntr < zeroExtend(params.pad_left)) 
                          || (params.ofmap_height - rg_h_cntr < zeroExtend(params.pad_bottom))
                          || (params.ofmap_width - rg_w_cntr < zeroExtend(params.pad_right));

      Bool is_triangle = (pack(rg_inp_traingle_cntr) == fromInteger(params.active_rows));
      
      if(rg_h_cntr == params.ofmap_height-1 && rg_w_cntr == params.ofmap_width-1)begin
        rg_inp_traingle_cntr <= rg_inp_traingle_cntr - 1;
      end
      else if(rg_w_cntr == params.ofmap_width-1)begin
        rg_w_cntr <= 0;
        rg_h_cntr <= rg_h_cntr + 1;
        SRAM_index#(if_index) lv_addr = rg_inp_row_addr + zeroExtend(params.stride_h);
        rg_inp_row_addr <= lv_addr;
        rg_inp_col_addr <= lv_addr;
      end
      else begin
        rg_w_cntr <= rg_w_cntr + 1;
        rg_inp_col_addr <= rg_inp_col_addr + zeroExtend(params.stride_w);
      end

      let req0 = SRAMKRdReq{index: rg_inp_col_addr, valid: is_triangle && !lv_pad_zero, pad_zero: lv_pad_zero};
			rg_inp_addr[0] <= req0;
			wr_inp_addr[0] <= req0;
		
			rg_old_out_addr <= rg_old_out_addr + 1;
			let oreq0 = SRAMKRdReq{index: rg_old_out_addr, valid: is_triangle, pad_zero: ?};
			rg_old_out_req[i] <= oreq0;
			wr_old_out_req[i] <= oreq0;

      for(Integer i=1; i<rows; i=i+1)begin
        let temp = rg_inp_addr[i-1];
        let index = temp.index;
        let req = SRAMKRdReq{index: index, valid: temp.valid && (fromInteger(i) < params.active_rows), 
										pad_zero: temp.pad_zero || (temp.valid && fromInteger(i) >= params.active_rows)};
				rg_inp_addr[i] <= req;
				wr_inp_addr[i] <= req;
      end

			for(Integer i=1; i<cols; i=i+1)begin
				let oreq = rg_old_out_req[i-1];
				rg_old_out_req[i] <= rg_old_out_req[i];
				wr_old_out_req[i] <= rg_old_out_req[i];
			end

    endrule
 
    Vector#(nRow, Put#(Bit#(in_width))) ifc_put_input;
    for(Integer i=0; i<rows; i=i+1)begin
      ifc_put_input[i] = (
        interface Put;
          method Action put(Bit#(in_width) value);
            systolic.subifc_rows[i].subifc_put_inp.put(value);
          endmethod
        endinterface
      );
    end

    Vector#(nCol, Put#(Bit#(out_width))) ifc_put_old_output;
    for(Integer i=0; i<cols; i=i+1)begin
      ifc_put_old_output[i] = (
        interface Put;
          method Action put(Bit#(out_width) value) 
								if(rg_params matches tagged Valid .params &&& params.preload_output);
            systolic.subifc_cols[i].subifc_put_acc.put(value);
          endmethod
        endinterface
      );
    end

    Vector#(nCol, Get#(SRAMKWrReq#(of_index, out_width))) ifc_get_new_output;
    for(Integer i=0; i<cols; i=i+1)begin
      ifc_get_new_output[i] = (
        interface Get;
          method ActionValue#(SRAMKWrReq#(of_index, out_width)) get if(rg_valid_col[i] && rg_new_out_cntr > 0);
            let value <- systolic.subifc_cols[i].subifc_get_acc.get();
						rg_new_out_addr[i] <= rg_new_out_addr[i] + 1;
						if(i==validValue(rg_params).active_cols-1)begin
							rg_new_out_cntr <= rg_new_out_cntr - 1;
						end
            return SRAMKWrReq{index: rg_new_out_addr[i], data: value, valid: rg_which_buffer};
          endmethod
        endinterface
      );
    end

    Vector#(nCol, Get#(SRAMKRdReq#(of_index))) ifc_get_old_out_addr;
    for(Integer i=0; i<cols; i=i+1)begin
      ifc_get_old_out_addr[i] = (
        interface Get;
          method ActionValue#(SRAMKRdReq#(of_index)) get if(rg_params matches tagged Valid .params &&& params.preload_output &&& rg_valid_col[i]);
						return wr_old_out_req[i];
          endmethod
        endinterface
      );
    end
		
		//TODO - This interface is not properly instantiated in the top. Fix!
		Vector#(nRow, Get#(Tuple2#(SRAMKRdReq#(if_index), Dim1))) ifc_get_inp_addr;
		for(Integer i=0; i<rows; i=i+1)begin
			ifc_get_inp_addr[i] = (
				interface Get;
					method ActionValue#(SRAMKRdReq#(if_index)) get;
						//if(wr_inp_addr[i].pad_zero)begin
						//	systolic.subifc_rows[i].subifc_put_inp.put(0);
						//end
						return wr_inp_addr[i];
					endmethod
				endinterface
			);
		end

		interface get_inp_addr = ifc_get_inp_addr;
    interface get_new_output_data = ifc_get_new_output;
    interface put_old_out_resp = ifc_put_old_output;
    interface put_inp_resp = ifc_put_input;
    interface get_old_out_addr = ifc_get_old_out_addr;
		interface Get subifc_get_compute_finish;
			method ActionValue#(Bool) get if(isValid(rg_params) && rg_new_out_cntr == 0);
				rg_params <= tagged Invalid;
				return True;
			endmethod
		endinterface
		
  
    method ActionValue#(Tuple2#(SRAM_index#(wt_index), Dim1)) get_wt_addr
      if(rg_params matches tagged Valid .params &&&
         rg_wt_cntr < params.active_rows);
			rg_wt_addr <= rg_wt_addr - 1;
      rg_wt_cntr <= rg_wt_cntr + 1;
      ff_wt_coord.enq(rg_wt_cntr);
			if(rg_wt_cntr == params.active_rows-1)begin
				rg_weightload_req <= False;
			end
      return tuple2(wr_wt_req, params.active_cols);
    endmethod

    method Action put_wt_resp(Vector#(nCol, Bit#(in_width)) weights)
      if(rg_params matches tagged Valid .params &&& rg_weightload);

      Dim1 coord = ff_wt_coord.first;
      for(Integer i=0; i<cols; i=i+1)begin
        if(fromInteger(i) < params.active_cols)begin
          //send weight to systolic
					//systolic.subifc_cols[i].subifc_clear_wgt(); 
					systolic.subifc_cols[i].subifc_put_wgt.put(tuple2(weights[i], coord+1));
        end
      end
      ff_wt_coord.deq();
      if(coord == params.active_rows-1)begin
        rg_weightload <= False;
      end
    endmethod

    interface Put subifc_put_compute_params;
      method Action put(Compute_params#(if_index, of_index, wt_index, cp_pad) params) if(rg_params matches tagged Invalid);
        
        rg_params <= tagged Valid params;
				rg_which_buffer <= unpack(params.output_address[valueOf(of_index)-1]);//MSB of of_index

        rg_weightload <= True;
        rg_weightload_req <= True;
				rg_wt_addr <= params.weight_address + zeroExtend(params.active_rows) - 1;
        rg_wt_cntr <= nRow - params.active_rows;

        rg_h_cntr <= 0;
        rg_w_cntr <= 0;

        rg_inp_row_addr <= params.input_address;
        rg_inp_col_addr <= params.input_address;

        rg_inp_traingle_cntr <= fromInteger(params.active_rows);
        
        for(Integer i=0; i<rows; i=i+1)begin
          rg_valid_row[i] <= fromInteger(i) < params.active_rows;
        end
        
        for(Integer i=0; i<rows; i=i+1)begin
          rg_inp_addr[i] <= SRAMKRdReq{index: ?, valid: False, pad_zero: False};
        end
        
        rg_old_out_addr <= params.output_address;
        
        for(Integer i=0; i<cols; i=i+1)begin
          rg_valid_col[i] <= fromInteger(i) < params.active_cols;
        end

        let time_values = params.ofmap_width * params.ofmap_height;

				$display($time, "Received GEMM params", params.ofmap_width, params.ofmap_height, time_values);
        if(params.preload_output)begin
          for(Integer i=0; i<cols; i=i+1)begin
            rg_old_out_cntr[i] <= time_values;
          end
        end
        else begin
          rg_zero_cntr <= time_values;
        end
				
				rg_new_out_cntr <= time_values;

        for(Integer i=0; i<cols; i=i+1)begin
          rg_new_out_addr[i] <= params.output_address;
        end
      endmethod
    endinterface

  endmodule
endpackage
