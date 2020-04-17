/*
Author: Gokulan Ravi
Email ID: gokulan97@gmail.com
Details: ISA typedefs for Systolic Array
*/

package isa;

`define INS_WIDTH 64
`define DRAM_ADDR_WIDTH 32
`define SRAM_ADDR_WIDTH 26
`define DIM_WIDTH1 8
`define DIM_WIDTH2 4

typedef Bit#(`DRAM_ADDR_WIDTH) DRAM_address;
typedef Bit#(`SRAM_ADDR_WIDTH) SRAM_address;
typedef Bit#(`DIM_WIDTH1) Dim1;
typedef Bit#(`DIM_WIDTH2) Dim2;

typedef enum{
    LOAD,
    STORE,
    SETUP,
    COMPUTE,
    ALU
} Opcode;

typedef struct {
    Bit#(1) push_prev_dep;
    Bit#(1) pop_prev_dep;
    Bit#(1) push_next_dep;
    Bit#(1) pop_next_dep;
} Dep_flags;

typedef struct {
    Opcode opcode;
    Dep_flags flags;
    DRAM_address address;
} Instruction;

typedef enum {
    Max,
    Min,
    Add,
    Shift
} ALU_Opcode;

typedef struct {
  DRAM_address dram_address;
  SRAM_address sram_address;
  Dim1 x_size, y_size, z_size;
  Dim1 z_stride, y_stride;
  Bool is_reset;
} Mem_params;

typedef Mem_params Load_params;
typedef Mem_params Store_params;

typedef struct {
  SRAM_address input_address;
  SRAM_address output_address;
  SRAM_address weight_address;
  Dim1 in_fmap_height, in_fmap_width;
  Dim2 stride_h, stride_w;
  Dim2 pad_left, pad_right, pad_top, pad_bottom;
  Bool preload_output;
} Compute_params;

typedef struct {
    ALU_Opcode alu_opcode;
    SRAM_address input_address;
    SRAM_address output_address;
    Dim1 output_height, output_width; // OH', OW'
    Dim2 window_height, window_width; // R, S
    Dim1 mem_stride_OW, mem_stride_R, mem_stride_S; // S_OW, S_R, S_S
    Dim2 stride_h, stride_w; //Sx, Sy
    Bool use_immediate;
} ALU_params;

endpackage
