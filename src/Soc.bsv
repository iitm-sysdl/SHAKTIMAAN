package Soc;
	`define VERBOSITY 0
	import accelerator :: *;
	import bram :: *;
	import Semi_FIFOF :: *;
	import AXI4_Types :: *;
	import AXI4_Fabric::*;
	import Connectable::*;
	`include "systolic.defines"

	`define ADDR_WIDTH 32
	`define BUS_WIDTH 128
	`define ADDR_SPACE 25

	function Bit#(TLog#(`Num_slaves)) fn_slave_map (Bit#(`ADDR_WIDTH) addr);
    Bit#(TLog#(`Num_slaves)) slave_num = 0;
    if(addr >= `MemoryBase && addr<= `MemoryEnd)begin
      slave_num = `Memory_slave;
		end
		else if(addr >= `SysBase && addr <= `SysEnd)begin
			slave_num = `Systolic_slave;
		end
		return slave_num;
	endfunction

	module mkTbSoc(Empty);
	
		Reg#(Bool) rg_start <- mkReg(True);

		Ifc_accelerator#(`ADDR_WIDTH,26,`BUS_WIDTH,32768,16,32768,16,32768,16,16,32,16,16) systolic <- mk_accelerator;
		
		AXI4_Fabric_IFC#(`Num_masters, `Num_slaves, `ADDR_WIDTH, `BUS_WIDTH, 0) fabric <- mkAXI4_Fabric(fn_slave_map);

    AXI4_Master_Xactor_IFC#(`ADDR_WIDTH, `BUS_WIDTH, 0) m_xactor <- mkAXI4_Master_Xactor;  
    Ifc_bram_axi4#(`ADDR_WIDTH, `BUS_WIDTH, 0, `ADDR_SPACE) main_memory <- mkbram_axi4(`MemoryBase, "code.mem", "MainMEM");
	
		mkConnection(m_xactor.axi_side, fabric.v_from_masters[`CPU_master]);
		mkConnection(systolic.ifc_fetch_master, fabric.v_from_masters[`Fetch_master]);
		mkConnection(systolic.ifc_load_master, fabric.v_from_masters[`Load_master]);
		mkConnection(systolic.ifc_store_master, fabric.v_from_masters[`Store_master]);

		mkConnection(fabric.v_to_slaves[`Memory_slave], main_memory.slave);
		mkConnection(fabric.v_to_slaves[`Systolic_slave], systolic.ifc_fetch_slave);

		rule rl_start(rg_start);
			rg_start <= False;
     AXI4_Wr_Addr#(`ADDR_WIDTH, 0) write_addr = AXI4_Wr_Addr {awaddr: `CONFIG_ADDR, awuser: 0, awlen: 16,
                       awsize: 0, awburst: 'b01, awid: `CPU_master, awprot: ?};
      m_xactor.i_wr_addr.enq(write_addr);
     
			Bit#(128) lv_value = (5 << 32) | ('h00800000) ;//'h0123456789ABCDEFFEDCBA9876543210;
//			lv_value[`ADDR_WIDTH-1:0] = fromInteger(`MemoryBase);
//			lv_value[16+`ADDR_WIDTH-1:`ADDR_WIDTH] = fromInteger(3);

			AXI4_Wr_Data#(`BUS_WIDTH) write_data = AXI4_Wr_Data {wdata: lv_value, wstrb: 'b1, wlast: True, wid: `CPU_master};
			m_xactor.i_wr_data.enq(write_data);
		endrule
	endmodule

endpackage
