# SHAKTIMAAN: SHAKTI Multiply-Accumulate Accelerator Network
SHAKTIMAAN is a synthesizable and parameterized open source DNN accelerator.
The accelerator contains a systolic array and a SIMD engine to perform the expensive DNN computations. The entire code is written in Bluespec and is
compatible with Shakti C-Class processor.

## Dependencies
1. [Installing Bluespec](https://github.com/B-Lang-org/bsc)
2. [Installing verilator](https://www.veripool.org/verilator/)

## Using SHAKTIMAAN

### Software compilation
In the first step, instructions for SHAKTIMAAN are generated using a separate compilation process. More details to follow.  Instructions are compiled and stored in RAM, which is loaded when the testbench is initiated directly from `code.mem`. More to follow.

### Configuration
A testbench is available in `src/Soc.bsv` which instantiates the accelerator and runs a single trace of instructions.
All configurations can be changed adhering to the provisos required for compilation of Bluespec files and can be changed in `src/Soc.bsv`.
Non configurable parameters, such as ISA definitions, are defined in `src/commons/systolic.defines`.

### Verilog generation
1. `cd src`
2. `make generate_verilog` - generates Verilog files from Bluespec for all files.

### Simulation
1. `cd src`
2. `make generate_verilog`
3. `make link_verilator` - generates a binary `bin/out` which simulates the array.

## Full system simulation
More details to follow
