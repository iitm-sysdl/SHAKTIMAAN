Address Generation Unit
-----------------------

- Author: Mohan
- Co-Author: Gokulan

Overview
^^^^^^^^

**TBD**

Inputs to Module
^^^^^^^^^^^^^^^^

* Opcode
* Dep Flags
* DRAM base address
* SRAM base address
* Z_SIZE - Size of 1 channel of the 3D slice (number of filters in case of weights) 
* Z_STRIDE - Offset from start of channel to access next channel
* Y_SIZE - Number of columns to access to complete one 2D slice (number of channels in case of weights)
* Y_STRIDE - Offset from start of 2D slice to the next 2D slice
* X_SIZE - Number of rows to access to complete one 3D slice (number of pixels in unrolled RxS, in case of weights)
* Precision - Size of each element of the 3D slice

Outputs from Module
^^^^^^^^^^^^^^^^^^^

**TDB**

Assumptions
^^^^^^^^^^^

* We assume that the 3D slice in DRAM is in NHWC format, and will be stored in the same format in SRAM.

Pseudo Code for loading 3D slice from DRAM to SRAM
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: cpp

  for i = 0 to Z_SIZE: // Nested loop unrolled in time, across clock cycles 
    for j = 0 to Y_SIZE:
      for k = 0 to X_SIZE:
      dram_addr = dram_base + j * X_STRIDE + i * Y_STRIDE
      sram_addr = sram_base + j * X_SIZE + i * X_SIZE * Y_SIZE
      load(dram_addr, X_SIZE, sram_addr)


Doubts or Pitfalls that might arise
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Z_STRIDE and Y_STRIDE defined in the instruction are different from the ConvStride defined above for convolution.
* The pseudocode provided above does not factor for reducing input buffer bank conflicts. 

    - TODO: Find effective feeding mechanism to reduce input buffer bank conflicts.

* One AXI request per 2D slice or 3D slice?
* Unified buffer or seperate buffers?

