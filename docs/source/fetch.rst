Fetch [FrontEnd]
----------------


- Author: Vinod Ganesan
- Co-Author: Arjun Menon

Overview
^^^^^^^^

Inputs to the Module
^^^^^^^^^^^^^^^^^^^^
* AXIResp packet for instruction fetch 
* resetPC value from the host processor

Outputs from Module
^^^^^^^^^^^^^^^^^^^
* AXIReq packet for instruction fetch
* toDepResolver - ILEN bits 

Assumptions
^^^^^^^^^^^
The main assumption here is that the bus-width $\geq$ ILEN-width and there is no need to perform multiple fetches for the same instruction. Also, another assumption is that the resetPC is set by the processor along with a start-bit, after which the accelerator will start fetching from resetPC and increments the address everytime by resetPC+ILEN-width. 


Pseudo-Code
^^^^^^^^^^^

.. code:: cpp

  while true do
    sendFetchReq(resetPC)
    resetPC <= resetPC+4
    while true do
      if recvFetchResp.valid() then
        inst <= recvFetchResp().inst
        break
      end if
    end while
    // Instruction Decode
    if inst.opcode() == LOAD then
      tmpLoadQueue.enq(inst)
    else if inst.opcode() == STORE then
      tmpStoreQueue.enq(inst)
    else
      tmpGemmQueue.enq(inst)
    end if
  end while == 0
