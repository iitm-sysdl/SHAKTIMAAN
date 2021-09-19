package top;


// Importing Frontend modules 
import fetch_decode::*; 
import dependency_resolver::*; 

// Importing compute modules 
import ws_array::*;
import ws_pe::*; 
import compute_top::*; 

// Importing TensorALU modules 
import tensor_alu::*; 

// Importing Load / Store module
import load_module::*; 
import store_module::*;

// Importing buffers module 
import onchip_buffers::*;

import isa::*;

`include "top.defines"

(*synthesize*) 
module mkfetch_decode_test(Ifc_fetch_decode#(`ADDR_WIDTH, `DATA_WIDTH));
	let ifc();
	mkfetch_decode inst1(ifc);
	return(ifc);
endmodule

(*synthesize*)
module mkdependency_resolver_test(Ifc_dependency_resolver#(`IF_INDEX, `OF_INDEX, `WT_INDEX,
      `LD_PAD, `ST_PAD, `CP_PAD, `ALU_PAD));
	let ifc();
	mkdependency_resolver inst1(ifc); 
	return(ifc); 
endmodule

(*synthesize*)
module mkws_array_test(Ifc_ws_array#(`NROW,`NCOL,`IN_WIDTH,`OUT_WIDTH));
  let ifc();
  mk_ws_array inst(ifc);
  return (ifc);
endmodule

(*synthesize*)
module mkmac_test(Ifc_ws_pe#(`IN_WIDTH, `OUT_WIDTH));
  Ifc_ws_pe#(`IN_WIDTH, `OUT_WIDTH) inst1 <- mk_ws_pe(0, 0, 0);
  return (inst1);
endmodule

(*synthesize*)
module mkgemm_test(Ifc_compute_module#(`DRAM_ADDR_WIDTH,`SRAM_ADDR_WIDTH,`IN_WIDTH,`OUT_WIDTH,`NROW,`NCOL,`IF_INDEX,`WT_INDEX,`OF_INDEX,`CP_PAD));
  let ifc();
  mkgemm inst1(ifc);
  return (ifc);
endmodule
 
(*synthesize*)
module mktensor_alu_test(Ifc_tensor_alu#(`ALU_WIDTH,`NCOL,`OF_INDEX,`ALU_PAD));
  let ifc();
  mk_tensor_alu inst1(ifc);
  return (ifc);
endmodule
 
 
(*synthesize*)
module mkload_test(Ifc_load_Module#(`ADDR_WIDTH, `DATA_WIDTH, `SRAM_ADDR_WIDTH, `WT_INDEX, `WT_BANK, `WT_DATA, `IF_INDEX, `IF_BANK, `IF_DATA, `OF_INDEX, `OF_BANK, `OF_DATA, `MAX_INDEX, `MAX_BANK, `MAX_DATA, `MAX_WORDS));
  let ifc();
  mk_load_Module inst1(ifc);
  return (ifc);
endmodule
 
(*synthesize*)
module mkstore_test(Ifc_col2im#(`ADDR_WIDTH, `DATA_WIDTH, `SRAM_ADDR_WIDTH, `OF_INDEX, `OF_BANK, `OF_DATA, `OF_VALUES, `ST_PAD));
  let ifc();
  mkcol2im inst1(ifc);
  return (ifc);
endmodule

(*synthesize*) 
module mkonchip_test(Ifc_onchip_buffers#(`SRAM_ADDR_WIDTH, `IF_INDEX, `IF_BANK, `IF_ENTRIES, `WT_INDEX, `WT_BANK, `WT_ENTRIES, `OF_INDEX, `OF_BANK, `OUT_ENTRIES, `IN_WIDTH, `OUT_WIDTH));
	let ifc(); 
	mkbuffers inst1(ifc); 
	return (ifc); 
endmodule


//(*synthesize*)
//module mktop_tb(Ifc_accelerator#(32, 26, 128, 32768, 16, 32768, 16, 32768, 16, 16, 32, 16, 16));
//  let ifc();
//  mk_accelerator inst1(ifc);
//  return (ifc);
//endmodule

endpackage
