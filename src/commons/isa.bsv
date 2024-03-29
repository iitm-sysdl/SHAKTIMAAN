/*
Author: Gokulan Ravi, Mohan Prasath G R
Email ID: gokulan97@gmail.com, mohanprasathr@gmail.com
Details: ISA typedefs for Systolic Array
*/

package isa;
  
	import Vector::*;

	`include "systolic.defines"
typedef Bit#(`DRAM_ADDR_WIDTH) DRAM_address;
typedef Bit#(`SRAM_ADDR_WIDTH) SRAM_address;
typedef Bit#(`DIM_WIDTH1) Dim1;
typedef Bit#(`DIM_WIDTH2) Dim2;
typedef Bit#(`DIM_WIDTH3) Dim3;

typedef Bit#(TLog#(TAdd#(TMax#(`OBUF_BANKS,TMax#(`IBUF_BANKS,`WBUF_BANKS)),1))) Sram_valid;
typedef Bit#(`GEMM_PAD) Compute_pad;
typedef Bit#(`MEM_PAD) Mem_pad;
typedef Bit#(`ALU_PAD) ALU_pad;

typedef Bit#(120) Params;
typedef enum
{
  InputBuffer,
  WeightBuffer,
	OutputBuffer1,
	OutputBuffer2
}Buffer deriving(Bits, Eq, FShow);

typedef struct
{
	Bit#(a) index;
	Dim2 num_valid;
}WgtReq#(numeric type a) deriving(Bits, Eq, FShow);

typedef struct
{
	Buffer buffer;
	Bit#(a) index;
	Bit#(b) bank;
	Bit#(c) data;
	Sram_valid num_valid;
} SRAMReq#(numeric type a, numeric type b, numeric type c) deriving(Bits, Eq, FShow);

typedef struct
{
  Buffer buffer;
  Bit#(a) index;
  Bit#(b) bank;
	Sram_valid num_valid;
}SRAMRdReq#(numeric type a, numeric type b) deriving(Bits, Eq, FShow);

typedef struct
{
	Bool buffer;
	Bit#(a) index;
	Dim1 num_valid;
} TALUOpReq#(numeric type a) deriving(Bits, Eq, FShow);

typedef struct
{
	Bool buffer;
	Bit#(a) index;
	Vector#(nCol, Bit#(out_width)) values;
	Dim1 num_valid;
} TALUOutReq#(numeric type a, numeric type out_width, numeric type nCol) deriving(Bits, Eq, FShow);

typedef struct 
{
  Bit#(a) index;
  Bool valid;
	Bool pad_zero;
} SRAMKRdReq#(numeric type a) deriving(Bits, Eq, FShow);

typedef struct
{
  Bit#(a) index;
  Bit#(b) data;
  Bool valid;
} SRAMKWrReq#(numeric type a, numeric type b) deriving(Bits, Eq, FShow);

typedef enum{
    LOAD = 8,
    STORE,
    COMPUTE,
    ALU
} Opcode deriving(Bits, Eq, FShow);

typedef struct {
    Bool pop_prev_dep;
    Bool push_prev_dep;
    Bool pop_next_dep;
    Bool push_next_dep;
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
  Mem_pad padding;                     // 20
} Mem_params deriving(Bits, Eq, FShow);

typedef Mem_params Load_params;
typedef Mem_params Store_params;
                
typedef struct {                                                  //120 Total
  Dim3 input_address;                                             // 15
  Dim3 output_address;                                            // 15
  Dim3 weight_address;                                            // 15
  Dim1 ofmap_height; Dim1 ofmap_width;                            // 16
  Dim1 active_rows; Dim1 active_cols;                             // 16
  Dim2 stride_h; Dim2 stride_w;                                   //  8
  Dim2 pad_left; Dim2 pad_right; Dim2 pad_top; Dim2 pad_bottom;   // 16
  Bool preload_output;                                            //  1
  Compute_pad padding;                                            // 18
} Compute_params deriving(Bits, Eq, FShow);

typedef struct {                                      // 120 Total
  ALU_Opcode alu_opcode;                              //   2
  Dim3 input_address;                                 //  15
  Dim3 output_address;                                //  15
  Dim1 output_height; // OH'                          //   8
  Dim1 output_width; // OW'                           //   8
  Dim2 window_height; // R                            //   4
  Dim2 window_width; // S                             //   4
  Dim1 mem_stride_OW; // S_OW                         //   8
  Dim1 mem_stride_R; // S_R                           //   8
  Dim1 mem_stride_S; // S_S                           //   8
  Dim1 num_active;     //Number of filters(M)         //   8
  Bool use_immediate;                                 //   1
  Dim1 immediate_value;                               //   8
  ALU_pad padding;                                    //  23
} ALU_params deriving(Bits, Eq, FShow);

endpackage
