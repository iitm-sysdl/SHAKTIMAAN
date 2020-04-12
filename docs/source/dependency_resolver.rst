Dependency Resolver [FrontEnd]
------------------------------

- Author: Vinod Ganesan
- Co-Author: Arjun Menon

Overview
^^^^^^^^

Inputs to Module
^^^^^^^^^^^^^^^^

* InstructionFromFetch - ILEN bits
* Dependency Tokens

    - loadtoGemm (1-bit)
    - gemmtoLoad (1-bit)
    - gemmtoStore (1-bit)
    - storetoGemm (1-bit)

Outputs from Module
^^^^^^^^^^^^^^^^^^^
* InstrtoLoadModule - ILEN bits
* InstrtoStoreModule - ILEN bits
* InstrtoComputeModule - ILEN bits

Assumptions
^^^^^^^^^^^

The dependencies between Load, Store and Gemm are encoded in the instruction statically

Pseudo Code
^^^^^^^^^^^

.. code:: cpp

  while true do
    while tmpLoadQueue.notEmpty() do
      // Parallel While
      instL <= tmpLoadQueue.first
      if ( (InstL.dept.pop_next_dep && !gemmtoLoadQ.empty()) or 
            !instL.dept.pop_next_dep) then
        tmpLoadQueue.pop
        loadQueue.enq(instL)
      else
        Continue
      end if
    end while

    while tmpGemmQueue.notEmpty() do
      //Parallel While
      InstG <= tmpGemmQueue.first
      if ( (InstG.dept.popprevdep   and   !loadtoGemmQ.empty)   or
            !In-stG.dept.popprevdep) and  ((InstG.dept.popnextdep  and  !storeto-GemmQ.empty) or 
            !InstG.Dep.popnextdep)) then

          tmpGemmQueue.pop
          gemmQueue.enq(instG)
      else
        Continue
      endif
    end while

    whiletmpStoreQueue.notEmpty()do
      // Parallel While
      instS <= tmpStoreQueue.first
      if (InstS.dept.popprevdep   and   !gemmtoStoreQ.empty())   or   !in-stS.dept.popprevdep then
        tmpStoreQueue.pop
        StoreQueue.enq(instL)
      else
        Continue
      end if
    end while
  end while = 0


