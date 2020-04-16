==============================
Module: mkintMulWS 
==============================

Overview
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This module defines a single PE which supports weight-stationary and output-stationary dataflows. Such an element is capable of performing a single MAC operation in a systolic architecture. To carry out a complete convolution operation, these PEs are connected in a grid through the interface defined in the following section. This grid can then be connected to a set of FIFOs defined in mksystolic module through which the processor can push data and pull results.

Interface: Ifc_intMul_WS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
We are using Ifc_intMul_WS for this module, which in itself has 4 subinterfaces in the following sections. The connections between these subinterfaces are made in mksystolic module. Similarly, the input and output FIFO connections are also made in the same module. So please refer documentation of mksystolic for more information.

Subinterfaces in Ifc_intMul_WS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* **subifc_from_west**: A Put interface which is used to receive the input value(bWidth bits wide) from its horizontal neighbor. The PEs in first column are connected to inputFIFOs, definition of which is present in mksystolic module.
* **subifc_to_east**: A Get interface which is used to forward the input data received in the previous cycle to its horizontal neighbor. Thees connections from PEs present in last column are handled by mksystolic module.
* **subifc_from_north**: A Put interface which is used to receive the weight/output(twbWidth bits wide) value from its vertical neighbor. This is accompanied by metadata which contains an 8-bit counter value(used during WS Setup Phase), a 2-bit bitwidth value and an enumerated Mode value(to determine what operation which needs to be performed on this data). PEs in first row are connected to WeightFIFOs, definition of which is present in mksystolic module.
* **subifc_to_south**: A Get interface which is used to send the weight/output value to its vertical neighbor. The datatype in this interface matches to the one in subifc_from_north. PEs in last row are connected to AccumFIFOs, definition of which is present in mksystolic module.

Parameters which need to be defined
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* **bWidth**: Input data width.
* **twbWidth**: Accumulator data width.
* Note: We don't need a parameter for weight width. Since accumulator and weight exchange their places while switching between the two dataflows, the corresponding hardware units must always be twbWidth wide to address to worst case scenarios.


Pseudo Code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
|    (rg_north, rg_counter, rg_bitwidth, rg_mode) = from_north;
|       **if(rg_mode==WS Setup && rg_coord <= counter)** {
|        if(rg_coord == rg_counter)rg_fixed <= rg_from_north;
|        if(rg_coord < rg_counter) wr_to_south <= rg_from_north; 
|       }
|
|       **if(rg_mode==OS Setup Phase)** {
|         rg_fixed <= rg_north;  
|         wr_to_south  <= rg_north
|       }
|
|       **if(rg_mode==WS MAC Phase)** {
|         rg_west      = from_west;
|         wr_to_south <= rg_north + rg_fixed * rg_west;  
|         wr_to_east  <= rg_west;
|       }
|
|       **if(rg_mode==OS MAC Phase)** {
|         rg_west = from_west;
|         rg_fixed <= rg_fixed + rg_north * rg_west;  
|         wr_to_south <= rg_north;
|         to_east <= rg_west;
|       }
|
|       **if(rg_mode==OS Drain Phase)** {
|         wr_to_south <= rg_fixed;
|         rg_fixed <= rg_north;
|       }
|    to_south = (wr_to_south, rg_counter, rg_bitwidth, rg_mode);

