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
    ComputeBusy
  }State;

interface Ifc_systolic_top#(numeric type macWidth, numeric type nRow, numeric type nCol);
  method SRAM_address sendWeightReq;
  method Action getWeightRsp(Vector#(nCol, Bit#(macWidth)) inp_weight, Vector#(nCol, Bit#(TMul#(2,macWidth))) inp_acc);
  interface Put#(Compute_params) sysPacket; 
endinterface 


module mksystolic_top(Ifc_systolic_top#(macWidth, nRow, nCol))
  provisos(
          );

  let valnCol = valueOf(nCol);
  let valmacWidth = valueOf(macWidth);
  let valnRow = valueOf(nRow);
  
  Reg#(State) rg_state <- mkReg(Idle); 
  Reg#(Compute_params) rg_compute_packet <- mkRegU();
  Reg#(Bit#(8)) rg_coord <- mkConfigReg(0); //Super high Fan-out?
  Wire#(SRAM_address) wr_weight_addr <- mkWire();
  Vector#(nRow, Reg#(Bit#(16))) rowCounter <- replicateM(mkReg(-1));
  Wire#(Tuple2#(SRAM_address, Vector#(nRow, Reg#(Bit#(16))))) wr_inp_addr <- mkReg(0);

  //Maintenance registers 
  Reg#(Bit#(8)) rg_cloop <- mkReg(0);
  Reg#(SRAM_address) rg_inp_addr <- mkReg(0);


  Ifc_systolic#(nRow,nCol,macWidth) systolic_array <- mksystolic;  

  rule rl_computeSystolic(rg_state == ComputeBusy);
    //Send request and receive response
    //We need parameters for denoting Dilation and DepthWise in Conv2D
    Vector#(nRow, Bit#(16)) lv_addr = 0;
    for(Integer i = 0; i < valnRow; i=i+1) begin
      if(fromInteger(i) <= rg_cloop) begin
          lv_addr[i] = rowCounter[i]+1; //This +1 should ideally be +dilation
          rowCounter[i] <= lv_addr[i];
      end
    end
    wr_inp_addr <= tuple2(rg_inp_addr, lv_addr);
    rg_state <= Idle;
    //TODO - Take care of final zero triangle
  endrule


  interface Put sysPacket;
    method Action put#(Compute_params packet);
      rg_compute_packet <= packet;
      wr_weight_addr <= packet.weight_address;
    endmethod
  endinterface

  method SRAM_address sendWeightReq;
    return wr_weight_addr;
  endmethod

  method SRAM_address sendInpReq;
    return wr_inp_addr;
  endmethod

  method Action getinpRsp(Vector#(nRow, Maybe#(Bit#(macWidth))) inp);
    for(Integer i = 0; i < nRow; i=i+1) begin
      systolic_array.rfifo[i].send_rowbuf_value(inp[i]);
    end

    if(rg_cloop < valnRow) begin
      rg_cloop <= rg_cloop + 1;
    end

    //Updating RowCounter Values and Other pointer registers 
    for(Integer i = 0; i < valnRow; i=i+1) begin
      if(rowCounter[i] == rg_compute_packet.window_width) begin
        rowCounter[i] <= someVar + rg_compute_packet.in_fmap_width;
        //someVar update
      end
      else begin
      end
    end

  endmethod

  method Action getWeightRsp(Vector#(nCol, Bit#(macWidth)) inp_weight, Vector#(nCol, Bit#(TMul#(2,macWidth))) inp_acc);
    for(Integer i = 0; i < nCol; i=i+1) begin
        let tup = tuple4(inp_weight[i], inp_acc[i], rg_coord, 2'b00); //Last Field null for now
        systolic_array.cfifo[i].send_colbuf_value(tup);
    end

    if(rg_coord < compute_packet.co_ordinates) begin
      rg_coord <= rg_coord + 1;
      wr_weight_addr <= wr_weight_addr + rg_coord;
    end
    else begin
      rg_coord <= 0;
      rg_state <= ComputeBusy;
      rg_inp_addr <= rg_compute_packet.input_address;
    end
    //Maybe due to how the code is written above, the rg_coord may not be required in systolic.bsv anymore
  endmethod

endmodule 

endpackage
