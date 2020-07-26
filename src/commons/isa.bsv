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
    COMPUTE,
    ALU
} Opcode deriving(Bits, Eq, FShow);

typedef struct {
    Bit#(1) push_prev_dep;
    Bit#(1) pop_prev_dep;
    Bit#(1) push_next_dep;
    Bit#(1) pop_next_dep;
} Dep_flags deriving(Bits, Eq, FShow);

typedef enum {
    Max,
    Min,
    Add,
    Shift
} ALU_Opcode deriving(Eq, Bits, FShow);

typedef Bit#(a) SRAM_index#(numeric type a);
typedef Bit#(a) pad_bits(numeric type a);

typedef struct {
  DRAM_address dram_address;
  SRAM_address sram_address;
  Dim1 x_size; Dim1 y_size; Dim1 z_size;
  Dim1 z_stride; Dim1 y_stride;
  Bool is_reset;
  Bool bitwidth;
  pad_bits#(a) padding;
} Mem_params deriving(Bits, Eq, FShow);
                  
typedef Mem_para  ms Load_params;
typedef Mem_para  ms Store_params;
                  
typedef struct {  
  SRAM_index#(a)   input_address;
  SRAM_index#(b)   output_address;
  SRAM_index#(c)   weight_address;
  Dim1 ifmap_hei  ght; Dim1 ifmap_width;
  Dim1 ofmap_hei  ght; Dim1 ofmap_width;
  Dim1 active_ro  ws; Dim1 active_cols;
  Dim2 stride_h;   Dim2 stride_w;
  Dim2 pad_left;   Dim2 pad_right; Dim2 pad_top; Dim2 pad_bottom;
  Bool preload_output;
  pad_bits#(d) padding;
} Compute_params#(numeric type a, numeric type b, numeric type c, numeric type d) deriving(Bits, Eq, FShow);

typedef struct {
  ALU_Opcode alu_opcode;
  SRAM_index#(a) input_address;
  SRAM_index#(b) output_address;
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
  pad_bits#(c) padding;
} ALU_params#(numeric type a, numeric type b, numeric type c) deriving(Bits, Eq, FShow);

endpackage
