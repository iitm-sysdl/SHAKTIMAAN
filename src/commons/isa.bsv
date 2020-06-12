/*
Author: Gokulan Ravi, Mohan Prasath G R
Email ID: gokulan97@gmail.com, mohanprasathr@gmail.com
Details: ISA typedefs for Systolic Array
*/

package isa;

`define INS_WIDTH 64
`define DRAM_ADDR_WIDTH 32
`define SRAM_ADDR_WIDTH 26
`define DIM_WIDTH1 12
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
} Opcode deriving(Bits, Eq, FShow);

typedef struct {
    Bit#(1) push_prev_dep;
    Bit#(1) pop_prev_dep;
    Bit#(1) push_next_dep;
    Bit#(1) pop_next_dep;
} Dep_flags deriving(Bits, Eq, FShow);

typedef struct {
    Opcode opcode;
    Dep_flags flags;
    DRAM_address address;
} Instruction deriving(Bits, Eq, FShow);

typedef enum {
    Max,
    Min,
    Add,
    Shift
} ALU_Opcode deriving(Eq, Bits, FShow);

typedef struct {
  DRAM_address dram_address;
  SRAM_address sram_address;
  Dim1 x_size; Dim1 y_size; Dim1 z_size;
  Dim1 z_stride; Dim1 y_stride;
  Bool is_reset;
  Bool bitwidth;
} Mem_params deriving(Bits, Eq, FShow);

typedef Mem_params Load_params;
typedef Mem_params Store_params;

typedef struct {
  SRAM_address input_address;
  SRAM_address output_address;
  SRAM_address weight_address;
  Dim1 in_fmap_height; Dim1 in_fmap_width;
  Dim2 stride_h; Dim2 stride_w;
  Dim2 pad_left; Dim2 pad_right; Dim2 pad_top; Dim2 pad_bottom;
  Bool preload_output;
} Compute_params deriving(Bits, Eq, FShow);

typedef struct {
    ALU_Opcode alu_opcode;
    SRAM_address input_address;
    SRAM_address output_address;
    Dim1 output_height; // OH'
    Dim1 output_width; // OW'
    Dim2 window_height; // R
    Dim2 window_width; // S
    Dim1 mem_stride_OW; // S_OW
    Dim1 mem_stride_R; // S_R
    Dim1 mem_stride_S; // S_S
    Dim2 stride_h; // Sx
    Dim2 stride_w; // Sy
    Dim1 num_of_filters; //Number of filters(M)
    Bool use_immediate;
    Dim1 immediate_value;// Modify the length of immediate value if required
} ALU_params deriving(Bits, Eq, FShow);

endpackage
