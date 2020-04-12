##############################
Interface with core and memory
##############################

The accelerator has an AXI interface, using which it sends and receives data from main memory, as well as from the host processor. The following are the interactions which the interface module makes with other modules present in the accelerator.

* The Frontend module in the frontend for fetching instructions.
* Load/Store queues in Frontend also generate memory requests for data stored in the backend.


