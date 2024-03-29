/* 
Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package fetch_decode;

  import GetPut::*;
  import AXI4_Types  ::*;
  import AXI4_Fabric ::*;
	import Semi_FIFOF::*;
  import FIFOF::*;
  import isa::*;
  `include "systolic.defines"

	/* Objective of the module: 
	   1. Send instruction request to DRAM based on the given PC
		 2. Receive the instruction response and decode the instruction 
		 3. Send the instruction parameters to the appropriate queue and send it to dependency resolver
		 4. Increment the PC
	*/

	/* Interface definitions:
	   1. The slave interface is used for receiving initial PC and reset signals from the control processor 
		 2. The master interface is used to send requests and receive instruction response 
		 3. The get_<module>_params interface is used to send the decoded params to the appropriate queue 
		    maintained in the dependency resolution module
		 4. The is_bool signal can be used to notify the control processor of completion and can operate as 
		    the interrupt signal
	*/
  interface Ifc_fetch_decode#(numeric type addr_width, numeric type data_width);
    interface AXI4_Slave_IFC#(addr_width, data_width, 0) slave;
    interface AXI4_Master_IFC#(addr_width, data_width, 0) master;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_load_params;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_store_params;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_compute_params;
    interface Get#(Tuple2#(Dep_flags, Params)) ifc_get_alu_params;
    method Bool is_complete;
		method Bool send_interrupt;
  endinterface

  //(*synthesize*)
  //module mkfetch_decode_Tb(Ifc_fetch_decode#(`DRAM_ADDR_WIDTH,`AXI_DATAWIDTH));
  //  let ifc();
  //  mkfetch_decode _temp(ifc);
  //  return ifc;
  //endmodule
 
	module mkfetch_decode(Ifc_fetch_decode#(addr_width, data_width))
    provisos(
						 Add#(a__, addr_width, data_width),
						 Add#(b__,12,addr_width));
  
		/* Register and FIFO declarations */
    Reg#(Bit#(addr_width)) rg_pc <- mkReg(?);
    Reg#(Bit#(16)) rg_num_ins <- mkReg(0);
  
    Reg#(Bool) rg_complete <- mkReg(True);
  
    FIFOF#(Bit#(data_width)) ff_fetch_data <- mkSizedFIFOF(valueOf(`FETCH_QUEUE_SIZE));
  
    Wire#(Dep_flags) wr_flags <- mkWire();
  
    Wire#(Params) wr_load  <- mkWire();
    Wire#(Params) wr_store <- mkWire();
    Wire#(Params) wr_compute <- mkWire();
    Wire#(Params) wr_alu <- mkWire();
		Wire#(Bool) wr_interrupt <- mkWire();

		//Instantiating the master and slave interfaces  
    AXI4_Master_Xactor_IFC#(addr_width, data_width, 0) m_xactor <- mkAXI4_Master_Xactor;  
    AXI4_Slave_Xactor_IFC#(addr_width, data_width,0) s_xactor  <- mkAXI4_Slave_Xactor;
   
    //Configuration Space to write start PC from the host 
		//It also provides the number of instructions in the current program to be executed which is 
		//used to generate appropriate burst requests to fetch the data 
		(*mutually_exclusive="rl_axi_set_config, rl_send_request"*)
		(*mutually_exclusive="rl_axi_set_config, rl_recv_data"*)
		rule rl_axi_set_config;
      let aw <- pop_o(s_xactor.o_wr_addr);
      let w  <- pop_o(s_xactor.o_wr_data);
      let lv_addr = aw.awaddr;
      let lv_data = w.wdata;
       
      Bool valid = lv_addr == `CONFIG_ADDR;
  
      if(valid)begin
				Bit#(addr_width) addr = truncate(lv_data);
				rg_pc <= addr;
        Bit#(16) ins_count = lv_data[16+valueOf(addr_width)-1:valueOf(addr_width)];
				rg_num_ins <= ins_count;
        rg_complete <= False;
				$display($time, "Setting systolic config: %x %x, data: %x", addr, ins_count, lv_data); 
      end
      let resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
      s_xactor.i_wr_resp.enq (resp);
    endrule


		//This rule is used to send burst fetch request to the memory 
		//Each burst length can maximum do 256 contiguous fetches
		//num_ins/256 is the total number of fetch requests sent to the memory
    rule rl_send_request(rg_num_ins > 0);
      Bit#(8) burst_len = min(8'd255, truncate(rg_num_ins-1)); 
      Bit#(12) lv_blen_shifted = zeroExtend(burst_len) << 4; // burstleng(8)+4//TODO: Is this change correct?
      Bit#(addr_width) next_pc = rg_pc + zeroExtend(lv_blen_shifted);
      rg_pc <= next_pc;
      rg_num_ins <= rg_num_ins - zeroExtend(burst_len + 1);
        
      let read_request = AXI4_Rd_Addr{ araddr: rg_pc, arid: `Fetch_master, arlen: burst_len, arprot: ?, //TODO: fix arprot
                                       arsize: 3'b100, arburst: 2'b01, aruser: 0};
      m_xactor.i_rd_addr.enq(read_request);
			$display($time, "Sending request for addr: %x, pending: %x", rg_pc, rg_num_ins-zeroExtend(burst_len-1));
    endrule


		//This rule receives the burst fashion and keeps enqueueing it into the fetch queue, until
		//all instructions in the program are fetched!
    rule rl_recv_data(m_xactor.o_rd_data.first.rid == `Fetch_master &&
											m_xactor.o_rd_data.first.rresp==AXI4_OKAY);
      let resp <- pop_o(m_xactor.o_rd_data);
      let inst = resp.rdata;
  
      if(resp.rlast && rg_num_ins==0)begin
        rg_complete <= True;
      end

			$display($time, "Received data: %x", inst);
  
      ff_fetch_data.enq(inst);
    endrule

		rule rl_raise_interrupt(m_xactor.o_rd_data.first.rid == `Fetch_master &&
														m_xactor.o_rd_data.first.rresp == AXI4_SLVERR); 
			let resp <- pop_o(m_xactor.o_rd_data);
			wr_interrupt <= True;
		endrule

		//The fetched data is enqueued into the fetch pipeline FIFO, which is dequeued by 
		//decode stage, which will decode the instructions and decide which command queue 
		//the instructions' decoded params are enqueued
    rule rl_decode;
      Bit#(data_width) inst = ff_fetch_data.first;
      ff_fetch_data.deq();
  
      Opcode opcode = unpack(inst[127:124]);
      Params params = unpack(inst[119:0]); //Padded instruction parameters
			$display($time, "Decoding %x instruction: %x, %x, %x", opcode, params, inst[31:0], inst[123:120]);

      wr_flags <= unpack(inst[123:120]); //Dependency flags 
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
    
		//Exposing the master and slave fetch interface to the outer world 
    interface slave = s_xactor.axi_side; 
    interface master = m_xactor.axi_side;

		//These interfaces get the respective interface parameters and based on decode's signalling, 
		//Will send it to the respective interface within the dependency resolver module

    interface Get ifc_get_load_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
				$display($time, "Sending params to load module %x, %x", wr_flags, wr_load);
        return tuple2(wr_flags, wr_load);
      endmethod
    endinterface
  
    interface Get ifc_get_store_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
        return tuple2(wr_flags, wr_store);
      endmethod
    endinterface
  
    interface Get ifc_get_compute_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
 				$display($time, "Sending params to compute module %x, %x", wr_flags, wr_compute);
				return tuple2(wr_flags, wr_compute);
      endmethod
    endinterface
  
    interface Get ifc_get_alu_params;
      method ActionValue#(Tuple2#(Dep_flags, Params)) get;
        return tuple2(wr_flags, wr_alu);
      endmethod
    endinterface

		method Bool send_interrupt = wr_interrupt;

		//Exposing a complete signal to the top -- this can be sent as an interrupt to the host
		//processor signalling completion 
    method Bool is_complete if(rg_complete);
       return True;
    endmethod
  
  endmodule

endpackage
