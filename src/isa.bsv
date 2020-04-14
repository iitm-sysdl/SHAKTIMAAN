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
    ALU_Opcode alu_opcode;
    SRAM_address address_operand;
    SRAM_address address_output;
    Dim1 window_loop_extent;
    Dim1 output_loop_extent;
    Dim1 window_size;
    Dim1 window_X_stride, window_Y_stride;
    Dim1 output_X_stride, output_Y_stride;
    //Dim1 input_element_stride, output_element_stride;
    //Dim1 input_stride, output_stride;
    Bit#(1) use_immediate;
} ALU_params;

endpackage
