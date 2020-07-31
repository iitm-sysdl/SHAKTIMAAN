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

  `define DRAM_ADDR_WIDTH 32
  `define SRAM_ADDR_WIDTH 26

  typedef struct {
    Bit#(a) index;
    Bool valid;
  } SRAMKRdReq#(numeric type a) deriving(Bits, Eq, FShow);

  typedef struct {
    Bit#(a) index;
    Bit#(b) data;
    Bool valid;
  } SRAMKWrReq#(numeric type a, numeric type b) deriving(Bits, Eq, FShow);

  interface Ifc_compute_module#(numeric type dram_addr_width, numeric type sram_addr_width,
                               numeric type in_width, numeric type out_width,
                               numeric type nRow, numeric type nCol,
                               numeric type if_index, numeric type if_bank,
                               numeric type wt_index, numeric type wt_bank,
                               numeric type of_index, numeric type of_bank);
    interface Put#(Compute_params) subifc_put_compute_params;
    //interface Get#(Bool) subifc_get_compute_finish;
    method ActionValue#(Vector#(nRow, SRAMKRdReq#(if_index))) get_inp_addr;
    interface Vector#(nRow, Put#(Bit#(in_width))) put_inp_resp;
    method ActionValue#(Vector#(nCol, SRAMKRdReq#(wt_index))) get_wt_addr;
    method Action put_wt_resp(Vector#(nCol, Bit#(in_width)) weights);
    Vector#(nCol, Get#(SRAMKRdReq#(of_index))) get_old_out_addr;
    interface Vector#(nCol, Put#(Bit#(out_width))) put_old_out_resp;
    interface Vector#(nCol, Get#(SRAMKWrReq#(of_index, out_width))) get_new_output_data;
  endinterface

  (*synthesize*)
  module mkgemm_Tb(Ifc_compute_module#(32,26,8,16,4,4,5,2,6,2,7,2));
    let ifc();
    mkgemm inst1(ifc);
    return (ifc);
  endmodule
  
  module mkgemm(Ifc_compute_module#(dram_addr_width, sram_addr_width,
                                   in_width, out_width,
                                   nRow, nCol,
                                   if_index, if_bank,
                                   wt_index, wt_bank,
                                   of_index, of_bank))
    provisos(Add#(dram_addr_width, 0, `DRAM_ADDR_WIDTH),
             Add#(sram_addr_width, 0, `SRAM_ADDR_WIDTH),
             Mul#(ibytes, 8, in_width),
             Mul#(wbytes, 8, in_width),
             Mul#(obytes, 8, out_width),
             Add#(a__, in_width, TMul#(in_width, 2))
             );

    let iBits = valueOf(if_bank);
    let oBits = valueOf(of_bank);

    let ibuf_index = valueOf(if_index);
    let ibuf_bankbits = valueOf(if_bank);
    
    let wbuf_index = valueOf(wt_index);
    let wbuf_bankbits = valueOf(wt_bank);

    let obuf_index = valueOf(of_index);
    let obuf_bankbits = valueOf(of_bank);

    let rows = valueOf(nRow);
    let cols = valueOf(nCol);

    let iBytes = valueOf(ibytes);
    let wBytes = valueOf(wbytes);
    let oBytes = valueOf(obytes);
      
    function Tuple2#(Bit#(if_index),Bit#(if_bank)) split_address_IBUF(Bit#(sram_width) addr);
	  	Bit#(if_bank) gbank = addr[ibuf_bankbits+iBytes-1:iBytes];
	  	Bit#(if_index) gindex = addr[ibuf_index+ibuf_bankbits+iBytes-1:iBytes+ibuf_bankbits];
	  	return tuple2(gindex,gbank);
	  endfunction

	  function Tuple2#(Bit#(wt_index),Bit#(wt_bank)) split_address_WBUF(Bit#(sram_width) addr);
	  	Bit#(wt_bank) gbank = addr[wbuf_bankbits+wBytes-1:wBytes];
	  	Bit#(wt_index) gindex = addr[wbuf_index+wbuf_bankbits+wBytes-1:wbuf_bankbits+wBytes];
	  	return tuple2(gindex,gbank);
	  endfunction

	  function Tuple2#(Bit#(of_index),Bit#(of_bank)) split_address_OBUF(Bit#(sram_width) addr);
	  	Bit#(of_bank) gbank = addr[obuf_bankbits+oBytes-1:oBytes];
	  	Bit#(of_index) gindex = addr[obuf_index+obuf_bankbits+oBytes-1:obuf_bankbits+oBytes];
	  	return tuple2(gindex,gbank);
	  endfunction
	
    Reg#(Maybe#(Compute_params)) rg_params <- mkReg(tagged Invalid);
    Reg#(Bool) rg_weightload <- mkReg(False);
    
    Reg#(SRAM_index#(wt_index)) rg_wt_addr <- mkReg(?);
    
    Reg#(SRAM_index#(if_index)) rg_inp_row_addr <- mkReg(?);
    Reg#(SRAM_index#(if_index)) rg_inp_col_addr <- mkReg(?);

    Reg#(Dim1) rg_inp_traingle_cntr <- mkReg(0);
    Reg#(Dim1) rg_h_cntr <- mkReg(0);
    Reg#(Dim1) rg_w_cntr <- mkReg(0);
    Reg#(Dim1) rg_zero_cntr <- mkReg(0);

    Reg#(Dim1) rg_wt_cntr <- mkReg(0);

    Vector#(nCol, Wire#(SRAMKRdReq#(wt_index))) wr_wt_reqs <- replicateM(mkWire());

    Vector#(nRow, Reg#(SRAMKRdReq#(if_index))) rg_inp_addr <- replicateM(mkReg(?));
    Vector#(nRow, Reg#(Bool)) rg_valid_row <- replicateM(mkReg(False));

    Vector#(nCol, Reg#(Bit#(of_index))) rg_old_out_addr <- replicateM(mkReg(?));
    Vector#(nCol, Reg#(Dim1)) rg_old_out_cntr <- replicateM(mkReg(?));

    Vector#(nCol, Reg#(Bit#(of_index))) rg_new_out_addr <- replicateM(mkReg(?));

    Vector#(nCol, Reg#(Bool)) rg_valid_col <- replicateM(mkReg(False));

    FIFOF#(Dim1) ff_wt_coord <- mkFIFOF();
    FIFOF#(Dim1) ff_inp_count <- mkFIFOF();

    Ifc_systolic#(nRow, nCol, in_width, out_width) systolic <- mksystolic; 

    rule rl_send_wt_req(rg_params matches tagged Valid .params &&&
                        rg_weightload &&&
                        rg_wt_cntr >= 0);
      
      for(Integer i=0; i<cols; i=i+1)begin
        wr_wt_reqs[i] <= SRAMKRdReq {index: rg_wt_addr, valid: fromInteger(i) < params.active_cols};
      end
    endrule

    rule rl_send_init_acc_zero(rg_params matches tagged Valid .params &&&
                                 !rg_weightload &&&
                                 rg_zero_cntr > 0 &&&
                                 !params.preload_output);
      for(Integer i=0; i<cols; i=i+1)begin
        if(rg_valid_col[i])begin
          //send 0 to column i of systolic array
          //systolic.cfifo[i].send_colbuf_value(
        end
      end
    endrule

    Vector#(nRow, Put#(Bit#(in_width))) ifc_put_input;
    for(Integer i=0; i<rows; i=i+1)begin
      ifc_put_input[i] = (
        interface Put;
          method Action put(Bit#(in_width) value);
            //send value to row i of systolic array
            systolic.rfifo[i].send_rowbuf_value(tagged Valid value);
          endmethod
        endinterface
      );
    end

    Vector#(nCol, Put#(Bit#(out_width))) ifc_put_old_output;
    for(Integer i=0; i<cols; i=i+1)begin
      ifc_put_old_output[i] = (
        interface Put;
          method Action put(Bit#(out_width) value);
            //send value to column i of systolic array
            systolic.cfifo[i].send_acc_value(value);
          endmethod
        endinterface
      );
    end

    Vector#(nCol, Get#(SRAMKWrReq#(of_index, out_width))) ifc_get_new_output;
    for(Integer i=0; i<cols; i=i+1)begin
      ifc_get_new_output[i] = (
        interface Get;
          method ActionValue#(SRAMKWrReq#(of_index, out_width)) get if(rg_valid_col[i]);
            //let value = TODO: read output value from column i of systolic array
            let value <- systolic.cfifo[i].send_accumbuf_value();
            rg_new_out_addr[i] <= rg_new_out_addr[i] + 1;
            return SRAMKWrReq{index: rg_new_out_addr[i], data: ?/*value*/, valid: True};
          endmethod
        endinterface
      );
    end

    Vector#(nCol, Get#(SRAMKRdReq#(of_index))) ifc_get_old_out_addr;
    for(Integer i=0; i<cols; i=i+1)begin
      ifc_get_old_out_addr[i] = (
        interface Get;
          method ActionValue#(SRAMKRdReq#(of_index)) get if(rg_valid_col[i]);
            rg_old_out_cntr[i] <= rg_old_out_cntr[i] + 1;
            return SRAMKRdReq{index: rg_old_out_addr[i], valid: rg_valid_col[i]};
          endmethod
        endinterface
      );
    end

    interface get_new_output_data = ifc_get_new_output;
    interface put_old_out_resp = ifc_put_old_output;
    interface put_inp_resp = ifc_put_input;
    interface get_old_out_addr = ifc_get_old_out_addr;

    method ActionValue#(Vector#(nRow, SRAMKRdReq#(if_index))) get_inp_addr
        if(rg_params matches tagged Valid .params &&& 
          !rg_weightload &&& // compute phase
          ((rg_h_cntr == params.output_width-1 && rg_w_cntr == params.output_width-1) || // Input feeding phase
          rg_inp_traingle_cntr > 1)); //Final triangle while feeding inputs

      Bool lv_pad_zero = (rg_h_cntr < params.pad_top) || (rg_w_cntr < params.pad_left) 
                          || (params.output_height - rg_h_cntr < params.pad_bottom)
                          || (params.output_width - rg_w_cntr < params.pad_right); 

      Bool is_triangle = (pack(rg_inp_traingle_cntr) == fromInteger(rows));
      
      if(rg_h_cntr == params.output_height-1 && rg_w_cntr == params.output_width-1)begin
        //end of sending values, start sending invalids
        rg_inp_traingle_cntr <= rg_inp_traingle_cntr - 1;
      end
      else if(rg_w_cntr == params.output_width-1)begin
        rg_w_cntr <= 0;
        rg_h_cntr <= rg_h_cntr + 1;
        let lv_addr = rg_inp_row_addr + params.stride_h;
        rg_inp_row_addr <= lv_addr;
        rg_inp_col_addr <= lv_addr;
      end
      else begin
        rg_w_cntr <= rg_w_cntr + 1;
        rg_inp_col_addr <= rg_inp_col_addr + params.stride_w;
      end

      rg_inp_addr[0] <= SRAMKRdReq{index: rg_inp_col_addr, valid: is_triangle};

      for(Integer i=1; i<rows; i=i+1)begin
        let temp = rg_inp_addr[i-1];
        let index = temp.index;
        rg_inp_addr[i] <= SRAMKRdReq{index: index, valid: temp.valid && (fromInteger(i) < params.active_rows)};
      end

      Vector#(nRow, SRAMKRdReq#(if_index)) addresses;
      addresses[0] = SRAMKRdReq{index: rg_inp_col_addr, valid: is_triangle};
      for(Integer i=1; i<rows; i=i+1)begin
        addresses[i] = rg_inp_addr[i];
      end
      return addresses;
    endmethod
   
    method ActionValue#(Vector#(nCol, SRAMKRdReq#(wt_index))) get_wt_addr
      if(rg_params matches tagged Valid .params &&&
         rg_wt_cntr >= 0);
      
      Vector#(nCol, SRAMKRdReq#(wt_index)) requests;
      for(Integer i=0; i<cols; i=i+1)begin
        requests[i] = wr_wt_reqs[i];
      end
      rg_wt_addr <= rg_wt_addr + 1;
      rg_wt_cntr <= rg_wt_cntr - 1;
      ff_wt_coord.enq(rg_wt_cntr);
      return requests;
    endmethod

    method Action put_wt_resp(Vector#(nCol, Bit#(in_width)) weights)
      if(rg_params matches tagged Valid .params &&& rg_weightload);

      Dim1 coord = ff_wt_coord.first;
      for(Integer i=0; i<cols; i=i+1)begin
        if(fromInteger(i) < params.active_cols)begin
          //send weight to systolic
        end
      end
      ff_wt_coord.deq();
      if(coord == params.active_rows - 1)begin
        rg_weightload <= False;
      end
    endmethod

    interface Put subifc_put_compute_params;
      method Action put(Compute_params params) if(rg_params matches tagged Invalid);
        
        rg_params <= tagged Valid params;
        
        rg_weightload <= True;
        rg_wt_addr <= params.weight_address;
        rg_wt_cntr <= params.active_rows - 1;

        rg_h_cntr <= 0;
        rg_w_cntr <= 0;

        rg_inp_row_addr <= params.input_address;
        rg_inp_col_addr <= params.input_address;

        rg_inp_traingle_cntr <= fromInteger(rows);
        
        for(Integer i=0; i<rows; i=i+1)begin
          rg_valid_row[i] <= fromInteger(i) < params.active_rows;
        end
        
        for(Integer i=0; i<rows; i=i+1)begin
          rg_inp_addr[i] <= SRAMKRdReq{index: ?, valid: False};
        end
        
        for(Integer i=0; i<cols; i=i+1)begin
          rg_old_out_addr[i] <= params.output_address;
        end
        
        for(Integer i=0; i<cols; i=i+1)begin
          rg_valid_col[i] <= fromInteger(i) < params.active_cols;
        end

        let time_values = params.output_width * params.output_height;
        
        if(params.preload_output)begin
          for(Integer i=0; i<cols; i=i+1)begin
            rg_old_out_cntr[i] <= time_values;
          end
        end
        else begin
          rg_zero_cntr <= time_values;
        end
        
        for(Integer i=0; i<cols; i=i+1)begin
          rg_new_out_addr[i] <= params.output_address;
        end
      endmethod
    endinterface

  endmodule
endpackage
