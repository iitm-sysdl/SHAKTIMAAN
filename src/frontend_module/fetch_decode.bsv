/* 
Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package fetch_decode;

  import  GetPut::*;
  import  AXI4_Types  ::*;
  import  AXI4_Fabric ::*;
  import  FIFOF::*;
  import isa::*;
  `include "systolic.defines"
  
  interface Ifc_fetch_decode#(addr_width, data_width);
    interface AXI4_Slave_IFC#(addr_width, data_width, 0) slave;
    interface AXI4_Master_IFC#(addr_width, data_width, 0) master;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_load_params;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_store_params;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_compute_params;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_alu_params;
    method Bool is_complete;
  endinterface
  
  module mkfetch_decode(Ifc_fetch_decode#(addr_width, data_width))
    provisos(Mul#(128, N, data_width));
  
    Reg#(Bit#(addr_width)) rg_pc <- mkReg(?);
    Reg#(Bit#(16)) rg_num_ins <- mkReg(?);
  
    Reg#(Bool) rg_complete <- mkReg(True);
  
    FIFOF#(data_width) ff_fetch_data <- mkSizedFIFOF(valueOf(`FETCH_QUEUE_SIZE));
  
    Wire#(Dep_flags) wr_flags <- mkWire();
  
    Wire#(Params) wr_load  <- mkWire();
    Wire#(Params) wr_store <- mkWire();
    Wire#(Params) wr_compute <- mkWire();
    Wire#(Params) wr_alu <- mkWire();
  
    AXI4_Master_Xactor_IFC#(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;  
    AXI4_Slave_Xactor_IFC#(addr_width, data_width,0) s_xactor  <- mkAXI4_Slave_Xactor;
   
    //Configuration Space to write PC
    rule rl_axi_set_config;
      let aw <- pop_o(s_xactor.o_wr_addr);
      let w  <- pop_o(s_xactor.o_wr_data);
      let lv_addr = aw.awaddr;
      let lv_data = w.wdata;
       
      Bool valid = lv_addr == `CONFIG_ADDR;
  
      if(valid)begin
        rg_start_pc <= lv_data[valueOf(addr_width)-1:0];
        rg_ins_len <= lv_data[16+valueOf(addr_width)-1:valueOf(addr_width)];
        rg_complete <= False;
      end
       
      let resp = AXI4_Wr_Resp {bresp: (valid ? AXI4_OKAY : AXI4_ERROR), buser: aw.awuser, bid:aw.awid};
      s_xactor.i_wr_resp.enq (resp);
    endrule
  
    rule rl_send_request(rg_num_ins > 0);
      Bit#(8) burst_len = min(255, rg_num_ins-1);
      
      Bit#(addr_width) next_pc = rg_start_pc + zeroExtend(burst_len << 7);
      rg_start_pc <= nextPC;
      rg_num_ins <= rg_num_ins - burst_len - 1;
        
      let read_request = AXI4_Rd_Addr{ araddr: rg_start_pc, arid: `AXI_FETCH_MASTER, arlen: burst_len,
                                       arsize: 3'b100, arburst: 2'b01, aruser: 0};
      m_xactor.i_rd_addr.enq(read_request);
    endrule
  
    rule rl_recv_data;
      let resp <- pop_o(m_xactor.o_rd_data);
      let inst = resp.rdata;
  
      if(resp.rlast && rg_num_ins==0)begin
        rg_complete <= True;
      end
  
      ff_fetch_data.enq(inst);
    endrule
  
    rule rl_decode;
      Bit#(data_width) inst = ff_fetch_data.first;
      ff_fetch_data.deq();
  
      Opcode opcode = unpack(inst[3:0]);
      Params params = unpack(inst[:8]);
  
      wr_flags <= unpack(inst[7:4]);
      if(opcode == LOAD) begin
        wr_load <= params;
      end
      else if(opcode == STORE) begin
        wr_store <= params;
      end
      else if(opcode == ALU) begin
        wr_alu <= params;
      end
      else begin
        wr_compute <= params;
      end
    endrule
    
    interface fetch_slave = s_xactor.axi_side; 
    interface master = m_xactor.axi_side;
  
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_load_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
        return tuple2(wr_flags, wr_load);
      endmethod
    endinterface
  
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_store_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
        return tuple2(wr_flags, wr_store);
      endmethod
    endinterface
  
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_compute_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
        return tuple2(wr_flags, wr_compute);
      endmethod
    endinterface
  
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_alu_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
        return tuple2(wr_flags, wr_alu);
      endmethod
    endinterface
  
    method Bool is_complete if(rg_complete);
       return True;
    endmethod
  
  endmodule

endpackage
