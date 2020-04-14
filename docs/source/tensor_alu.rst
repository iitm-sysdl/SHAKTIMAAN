Tensor ALU
----------

- Author: Sadhana
- Co-Author: Mohan

Overview
^^^^^^^^
Tensor ALU is the combination of Vector Generation Logic and Vector of ALUs.
Vector generation logic sequentially generates vectors for the tensor operation. It loads 
input data from the output buffer using input base address and input memory 
stride and writes the output to the output buffer using output base address 
and output memory stride. The number of such vector operations and the number 
of elements in vector depends on the loop extent fields in ALU instruction

In Vector ALU,

 - Operands – Vector X, Vector Y
 - Output – Vector Z
 - Operations supported:

   1. Min
   2. Max
   3. Add
   4. Sub
   5. Shift

Size of Vectors(n) can be equal to the number of systolic columns (Just an arbitrary
choice can be modified based on performance)

Fields which are stored in buffer:

1. Input Matrix dimensions - N*C*H*W 
2. operations specific dimension - Strides Sx & Sy, dimension R & S
3. Base Address of Vector 1
4. Vector 1 Memory stride
5. Base Address of Vector 2
6. Vector 2 memory stride
7. Base address of output vector
8. Output memory stride

Tensor ALU instruction Fields:

1. Opcode
2. Dependency flags
3. ALU specific Opcode(3)
4. Is Matrix based(1)
5. Is immediate(1)
6. Immediate value(8)
7. Is continuous(1)
8. Continuous extent(8) - continuous number of operations (R*S)
9. Loop extent 0(8) - Total Vector operations in 2D (E*F), where E and F are decided based on
operation specific dimensions (Sx, Sy, R and S)
10. Loop extent 1(8) - Total number of 2D matrix operations (N*C)
 
Inputs to the Module
^^^^^^^^^^^^^^^^^^^^
* ALU instruction with all the necessary fields

Outputs from the Module
^^^^^^^^^^^^^^^^^^^^^^^
* ALU complete signal

Pseudo - Code
^^^^^^^^^^^^^
1. ALU module

  a. Receives the ALU specific opcode, one vector element from Vector X  and corresponding vector element from Y and gives an element of  Vector Z

2. Vector of ALU modules

  a. Instantiates ‘n’ ALU modules to operate on ‘n’ vector elements
  b. Gives appropriate inputs to the Vector of ALUs and receives ‘n’ outputs.

3. Vector generation logic

	Assumption : Matrix is stored in N*C*H*W format

  a. Get data from output buffer for vector operation
  b. Store the output vector to output buffer
  c. Calculate address based on the op-code for next vector operation and repeats for ‘loop count 1’ times

To Be Resolved
^^^^^^^^^^^^^^

Milestones
^^^^^^^^^^
1. Coding ALU module (8/4/2020 - 10/4/2020)
2. Coding Vector ALU module (11/4/2020 - 12/4/2020)
3. Little more prelim design work on Vector generation logic (9/4/2020 - 13/4/2020)
4. Coding the Vector generation logic(14/4/2020 - 20/4/2020)
