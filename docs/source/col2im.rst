Col2Im and Store address generation logic
-----------------------------------------

- Author: Sujay Pandit 
- Co-Author: Arjun Menon

Inputs to the Module 
^^^^^^^^^^^^^^^^^^^^

1. Start col2im signal
2. SRAM base addresses for all the Output buffer banks (Not if addresses are hard coded)
3. DRAM base addresses
4. The actual size of the output (Is needed to take care of strides) (O’)
5. Size of data to be stored NC’R’ (C’R’ represents a subset of the input convoluted by weights and C’ is equal to number of systolic array columns, R’ is number of o/p buffer rows)

Outputs from the Module
^^^^^^^^^^^^^^^^^^^^^^^

1. AXI store requests, completed signal

Assumptions
^^^^^^^^^^^
* The number of output buffer banks is equal to the number of systolic array columns.
* Every output buffer stores data corresponding to a filter. (A buffer may have twice the number of entries to run stores and systolic writes in parallel). 
* W’ must be a multiple of ``\`bus_width``

Pseudo Code
^^^^^^^^^^^

.. code:: cpp

  For i in num_of_output_buffer banks (C’ buffer banks)
        Read buffer_i (R’ rows))
                for j in 0,buffer_i_rows-1: 
                        Store( row_j,DRAM_address + j*(O’-size(row_j) )


  Store(a,b):
        Burst_length= W’/`bus_width
        Base_addr: b
        Data: a
        Size: `bus_width

To Be Resolved
^^^^^^^^^^^^^^

* Why are SRAMs address mapped? 2 options:

  1. SRAMs are not address mapped. In this case, the compiler will issue a store only after the compute is over. In this case, we know where the SRAM starts, and hence the H/W can automatically start reading from the SRAM.
  2. SRAM is address mapped. If the compiler wants to schedule intermediate stores (while load and compute is being performed), then these addresses would have to be a part of the instruction issued by the compiler.

* How does the compiler generate store requests? What does one SRAM address indicate? Does it mean a burst request or stride, or could the compiler generate either of these?


Milestones
^^^^^^^^^^
* Im2col- (13th-19th April)
* Store Address Generation logic- 19th-22nd April

