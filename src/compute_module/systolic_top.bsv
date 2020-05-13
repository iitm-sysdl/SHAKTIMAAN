/* 
Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package systolic_top;

//Imports
import Vector::*;
import systolic::*;

//enums
typedef enum
  {
    Idle,
    WeightLoad,
    ComputeIdle,
    ComputeBusy
  }State;

interface Ifc_systolic_top#(numeric type macWidth, numeric type nRow, numeric type nCol);
  method SRAM_address mv_send_weight_addr;
  method Action ma_get_weight_resp(Vector#(nCol, Bit#(macWidth)) inp_weight
    /*, Vector#(nCol, Bit#(TMul#(2,macWidth))) inp_acc*/);
  method Tuple2#(SRAM_address, Vector#(nRow, Reg#(Bit#(16)))) mv_send_inp_addr;
  method Action ma_get_inp_rsp(Vector#(nRow, Maybe#(Bit#(macWidth))) inp);
  method Action init_params#(Compute_params packet);
endinterface

module mksystolic_top(Ifc_systolic_top#(macWidth, nRow, nCol))
  provisos(
          );

  let valnCol = valueOf(nCol);
  let valmacWidth = valueOf(macWidth);
  let valnRow = valueOf(nRow);

  /*------------------------INTERNAL REGISTERS-------------------------------------------*/
  Reg#(State)                         rg_state              <-        mkReg(Idle); 
  Reg#(Maybe#(Compute_params))        rg_compute_packet     <-        mkReg(tagged Invalid);
  Reg#(Dim1)                          rg_coord              <-        mkConfigReg(0); //Super high Fan-out?
  
  /*------------------------INTERNAL WIRES-----------------------------------------------*/
  Wire#(SRAM_address)                 wr_weight_addr        <-        mkWire();


  Vector#(nRow, Maybe#(Reg#(Bit#(16))))       rowCounter            <-        replicateM(mkReg(-1));
  Wire#(Tuple2#(SRAM_address, Vector#(nRow, Reg#(Bit#(16))))) wr_inp_addr <- mkReg(0);

  //Maintenance registers 
  
  Reg#(Bit#(8)) rg_cloop <- mkReg(0);
  Reg#(SRAM_address) rg_inp_addr <- mkReg(0);

  Ifc_systolic#(nRow,nCol,macWidth) systolic_array <- mksystolic;  

  rule rl_computeSystolic(rg_state == ComputeBusy);
    Vector#(nRow, Bit#(16)) lv_addr = 0;
    for(Integer i = 0; i < valnRow; i=i+1) begin
      if(fromInteger(i) <= rg_cloop) begin
          lv_addr[i] = validValue(rowCounter[i]) + rg_compute_packet.stride_w; 
          rowCounter[i] <= tagged Valid lv_addr[i];
      end
    end
    wr_inp_addr <= tuple2(rg_inp_addr, lv_addr);
    rg_state <= ComputeIdle;
  endrule

  method Action init_params#(Compute_params packet) if(rg_state == Idle);
      rg_compute_packet <= tagged Valid packet;
      wr_weight_addr <= packet.weight_address;
      rg_state <= WeightLoad;
  endmethod

  /*-----------Interfaces for weight load logic-----------------------------*/
  method SRAM_address mv_send_weight_addr;
    return wr_weight_addr;
  endmethod

  //Have a method to send Partial Sums

  method Action ma_get_weight_resp(Vector#(nCol, Bit#(macWidth)) inp_weight) if(rg_state == WeightLoad);
    for(Integer i = 0; i < nCol; i=i+1) begin
        let tup = tuple4(inp_weight[i], 0, rg_coord, 2'b00);
        systolic_array.cfifo[i].send_colbuf_value(tup);
    end

    if(rg_coord < validValue(compute_packet).num_active_rows) begin
      rg_coord <= rg_coord + 1;
      wr_weight_addr <= wr_weight_addr + rg_coord;
    end
    else begin
      rg_coord <= 0;
      rg_state <= ComputeBusy;
      rg_inp_addr <= validValue(rg_compute_packet).input_address;
    end
  endmethod
  /*--------End of Weight load interfaces----------------------------------*/

  method Action ma_get_inp_rsp(Vector#(nRow, Maybe#(Bit#(macWidth))) inp);
    for(Integer i = 0; i < nRow; i=i+1) begin
      systolic_array.rfifo[i].send_rowbuf_value(inp[i]);
    end

    if(rg_cloop < valnRow) begin
      rg_cloop <= rg_cloop + 1;
    end

    //Updating RowCounter Values and Other pointer registers 
    //This is not handling Zero padding yet - TODO add
    for(Integer i = 0; i < valnRow; i=i+1) begin
      if(rowCounter[i] matches tagged Valid .valrow) begin
        if(valrow == rg_compute_packet.in_fmap_width) begin
          if(rg_inp_addr == rg_compute_packet.in_fmap_height) begin
            rowCounter[i] <= tagged Invalid;
          end
          else begin
            rowCounter[i] <= tagged Valid 0;
            rg_inp_addr <= rg_inp_addr + rg_compute_packet.stride_h * rg_compute_packet.in_fmap_width;
            rg_state <= ComputeBusy;
          end
        end
      end
    end

  endmethod

  method Tuple2#(SRAM_address, Vector#(nRow, Reg#(Bit#(16)))) mv_send_inp_addr;
    return wr_inp_addr;
  endmethod

endmodule 

endpackage
