Im2Col
------

- Author: Gokulan
- Co-Author: Mohan

Overview
^^^^^^^^

Inputs to the module
^^^^^^^^^^^^^^^^^^^^

* SRAM base address
* Input feature map dimensions (N', C', H', W') -- received from COMPUTE instruction
* Weight dimensions (M', C', R, S) -- received from previous SETUP instruction

Outputs from the Module
^^^^^^^^^^^^^^^^^^^^^^^

Assumptions
^^^^^^^^^^^
* The 3D slice stored in DRAM is stored continuously in SRAM.

Pseudo Code
^^^^^^^^^^^

.. code:: cpp

  for i = 1 to N’ // This loop is mapped in time
	  for j = 1 to C’: // This loop is mapped in space, across all systolic rows
		  counter[m] = 0
  		for m = 1 to R:
	  		value = fetch_value_from_sram(inp_base + m * W’ + j * H’ * W’)
		  	start_idx, end_idx = compute_replication(m, counter[m])
			  counter[m]++
  			for n=start_idx to end_idx:
	  			systolic.row[n].send_value(value)

To Be Resolved
^^^^^^^^^^^^^^
* Unroll across C dimension instead of RxS dimension.
* Changes in sharing of work across different feeder logics
* Bank conflicts while reading inputs

