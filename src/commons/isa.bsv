/*
Author: Gokulan Ravi, Mohan Prasath G R
Email ID: gokulan97@gmail.com, mohanprasathr@gmail.com
Details: ISA typedefs for Systolic Array
*/

package isa;
  `include "systolic.defines"
typedef Bit#(`DRAM_ADDR_WIDTH) DRAM_address;
typedef Bit#(`SRAM_ADDR_WIDTH) SRAM_address;
typedef Bit#(`DIM_WIDTH1) Dim1;
typedef Bit#(`DIM_WIDTH2) Dim2;

typedef enum
{
  Invalid,
  InputBuffer,
  OutputBuffer,
  WeightBuffer
}Buffer deriving(Bits, Eq, FShow);

typedef struct
{
	Buffer buffer;
	Bit#(a) index;
	Bit#(b) bank;
	Bit#(c) data;
} SRAMReq#(numeric type a, numeric type b, numeric type c) deriving(Bits, Eq, FShow);

typedef struct
{
  Buffer buffer;
  Bit#(a) index;
  Bit#(b) bank;
}SRAMRdReq#(numeric type a, numeric type b) deriving(Bits, Eq, FShow);

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
typedef Bit#(a) Pad_bits#(numeric type a);

typedef struct {                            //120 Total
  DRAM_address dram_address;                // 32
  SRAM_address sram_address;                // 26
  Dim1 x_size; Dim1 y_size; Dim1 z_size;    // 24
  Dim1 z_stride; Dim1 y_stride;             // 16
  Bool is_reset; Bool bitwidth;             //  2
  Pad_bits#(a) padding;                     // 20
} Mem_params#(numeric type a) deriving(Bits, Eq, FShow);

typedef Mem_params#(a) Load_params#(numeric type a);
typedef Mem_params#(a) Store_params#(numeric type a);
                
typedef struct {                                                  //120 Total
  SRAM_index#(a) input_address;                                   // 15
  SRAM_index#(b) output_address;                                  // 15
  SRAM_index#(c) weight_address;                                  // 15
  Dim1 ifmap_height; Dim1 ifmap_width;                            // 16
  Dim1 ofmap_height; Dim1 ofmap_width;                            // 16
  Dim1 active_rows; Dim1 active_cols;                             // 16
  Dim2 stride_h; Dim2 stride_w;                                   //  8
  Dim2 pad_left; Dim2 pad_right; Dim2 pad_top; Dim2 pad_bottom;   // 16
  Bool preload_output;                                            //  1
  Pad_bits#(d) padding;                                           //  2
} Compute_params#(numeric type a, numeric type b, numeric type c, numeric type d) deriving(Bits, Eq, FShow);

typedef struct {                                      // 120 Total
  ALU_Opcode alu_opcode;                              //   2
  SRAM_index#(a) input_address;                       //  15
  SRAM_index#(b) output_address;                      //  15
  Dim1 output_height; // OH'                          //   8
  Dim1 output_width; // OW'                           //   8
  Dim2 window_height; // R                            //   4
  Dim2 window_width; // S                             //   4
  Dim1 mem_stride_OW; // S_OW                         //   8
  Dim1 mem_stride_R; // S_R                           //   8
  Dim1 mem_stride_S; // S_S                           //   8
  Dim2 stride_h; // Sx                                //   4
  Dim2 stride_w; // Sy                                //   4
  Dim1 num_of_filters; //Number of filters(M)         //   8
  Bool use_immediate;                                 //   1
  Dim1 immediate_value;                               //   8
  Pad_bits#(c) padding;                               //  15
} ALU_params#(numeric type a, numeric type b, numeric type c) deriving(Bits, Eq, FShow);

endpackage
