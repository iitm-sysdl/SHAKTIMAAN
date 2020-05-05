==============================
Multi-Dataflow Support
==============================

Overview
------------------------------------
To extend the support of current dataflow from weight stationary to output stationary as well.

Inputs to the Module
------------------------------------
* **Weights**: The weights for convolution into Weight FIFOs. 
* **Counter**: The value that the y-coordinate of the accompanying weight and store it in the correct PE in weight stationary approach. This is passed as a tupled value from Weight FIFO.
* **Mode**: The value that determines which phase of a dataflow is the current PE operating in. A valid value should always be passed for every transactin along North-South interface. This is passed as a tupled value from Weight FIFO.
* **Conv. Inputs**: The inputs for convolution into Input FIFOs.

Outputs from the Module
-----------------------------------
* **Conv. Outputs**: The outputs will be sent out from Accumulator FIFOs to Accumulation buffers. The Accumulator FIFOs are filled during MAC phase itself in weight stationary approach, however we need a final drain phase to populate this FIFO in output stationary approach.

Pseudo Code
------------------------------------
| **for each PE:**
|    (rg_north,rg_counter,rg_bitW,rg_mode) = from_north;
|
|       **if(rg_mode==WS Setup Phase)** {
|         if(rg_counter == rg_coord) rg_fixed = rg_north;    
|         if(rg_counter < rg_coord) to_south = (rg_north,rg_counter,rg_bitW,rg_mode);
|
|       **if(rg_mode==OS Setup Phase)** { 
|         rg_fixed = 0;   
|         to_south = (rg_north,rg_counter,rg_bitW,rg_mode);
|
|       **if(rg_mode==WS MAC Phase)** { 
|         rg_west = from_west; 
|         new_accum = rg_north + rg_fixed * rg_west;   
|         to_south = (new_accum,rg_counter,rg_bitW,rg_mode);
|         to_east = rg_west; 
|
|       **if(rg_mode==OS MAC Phase)** { 
|         rg_west = from_west; 
|         rg_fixed = rg_fixed + rg_north * rg_west;   
|         to_south = (rg_north,rg_counter,rg_bitW,rg_mode);
|         to_east = rg_west; 
|
|       **if(rg_mode==OS Drain Phase)** { 
|         to_south = (rg_fixed,rg_counter,rg_bitW,rg_mode);
|         rg_fixed = rg_north;

Assumptions/Pitfalls
------------------------------------
* Can we combine accum_from_south and from_south interfaces into one? 
* Need to add another 3-bit value to North-South interface corresponding to mode of operation?
