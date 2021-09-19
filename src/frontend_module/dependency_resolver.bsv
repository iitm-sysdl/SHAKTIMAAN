/* 
Author: Vinod Ganesan, Gokulan Ravi
Email id: g.vinod1993@gmail.com, gokulan97@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package dependency_resolver;

  import GetPut::*;
  import FIFOF::*;
  import isa::*;
  `include "systolic.defines"

/* Objective of the Module:                                                                       
       The dependency resolution module is responsible for scheduling instructions                    
       to the appropriate modules, i.e. load, store, GEMM, and TensorALU                              
       1. If the instruction encoding has a dependency flag with another instruction, the dependency  
          resolution module will not schedule that instruction until the dependent instruction has    
          completed its execution                                                                     
       2. The dependent instruction, upon completion sends a finish signal to one of the dependency   
          queues which is monitored by the dependency resolution module which schedules the stalled 
					instruction now                                                                                   
*/             

	//The interface contains sub-interfaces to connect Dependency resolvers with the fetch stage 
	//and the load, store and compute modules
	//The interface contains numeric types: if_index - addressing bits for input buffer 
	//of_index - addressing bits for output buffer, wt_index - addressing bits for weight buffer 
	//st_pad, cp_pad, ld_pad and alu_pad - Extra padding bits for the params to make it 128 bits 
	//The pad bits are numeric types since buffer sizes are variables!

	/* Interface definitions:
		 1. The ifc_put_<module>_params receives the packets sent by the fetch module and enqueues into 
				the appropriate queue 
		 2. The ifc_get_<module>_instruction is the interface that dequeues the FIFO upon resolving the dependencies
		 3. The ifc_put_<module>_complete is used to receive the complete signals from the previously
		    scheduled instructions which is vital for resolving the dependencies of the subsequent instructions
	*/
  interface Ifc_dependency_resolver;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_load_params;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_store_params;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_compute_params;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_alu_params;
  
    interface Get#(Load_params) ifc_get_load_instruction;
    interface Get#(Store_params) ifc_get_store_instruction;
    interface Get#(Compute_params) ifc_get_gemm_instruction;
    interface Get#(ALU_params) ifc_get_alu_instruction;
  
    interface Put#(Bool) ifc_put_load_complete;
    interface Put#(Bool) ifc_put_store_complete;
    interface Put#(Bool) ifc_put_gemm_complete;
    interface Put#(Bool) ifc_put_alu_complete;
  endinterface 

  //(*synthesize*)
  //module mkdependency_resolver_Tb(Ifc_dependency_resolver);
  //  let ifc();
  //  mkdependency_resolver _temp(ifc);
  //  return ifc;
  //endmodule
 
	module mkdependency_resolver(Ifc_dependency_resolver);
  
		//Instantiating the load, store, gemm and ALU queue 
    FIFOF#(Dep_flags) ff_load_queue  <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
    FIFOF#(Dep_flags) ff_gemm_queue  <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
    FIFOF#(Dep_flags) ff_alu_queue   <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
    FIFOF#(Dep_flags) ff_store_queue <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
  
    FIFOF#(Load_params)    ff_load_params  <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
    FIFOF#(Store_params)   ff_store_params <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
    FIFOF#(Compute_params) ff_gemm_params  <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
    FIFOF#(ALU_params)     ff_alu_params   <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
  
    FIFOF#(Bool) ff_load_to_gemm  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_gemm_to_load  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_gemm_to_alu   <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_alu_to_gemm   <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_alu_to_store  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_store_to_alu  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));

    Wire#(Bool) load_dispatch <- mkWire();
    Wire#(Bool) gemm_load_dispatch <- mkWire();
    Wire#(Bool) gemm_alu_dispatch <- mkWire();
    Wire#(Bool) talu_gemm_dispatch <- mkWire();
    Wire#(Bool) talu_store_dispatch <- mkWire();
    Wire#(Bool) store_dispatch <- mkWire();
  
    function Bool fn_resolve_prev_pop(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) dep_queue);
      Dep_flags flags = flag_queue.first;
      return !flags.pop_prev_dep || (flags.pop_prev_dep && dep_queue.notEmpty());
    endfunction
  
    function Bool fn_resolve_next_pop(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) dep_queue);
      Dep_flags flags = flag_queue.first;
      return !flags.pop_next_dep || (flags.pop_next_dep && dep_queue.notEmpty());
    endfunction
  
    function Action fn_push_prev(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) prev_queue);
      action
				Dep_flags flags = flag_queue.first;
				if(flags.push_prev_dep)begin
					prev_queue.enq(True);
				end
			endaction
    endfunction
  
    function Action fn_push_next(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) next_queue);
      action
				Dep_flags flags = flag_queue.first;
				if(flags.push_next_dep)begin
					next_queue.enq(True);
				end
			endaction
    endfunction
  
    rule ff_gemm_to_load_deq(load_dispatch == True);
     ff_gemm_to_load.deq();
    endrule
  
    rule ff_load_to_gemm_deq(gemm_load_dispatch == True);
     ff_load_to_gemm.deq();
    endrule

    rule ff_alu_to_gemm_deq(gemm_alu_dispatch == True);
     ff_alu_to_gemm.deq();
    endrule

    rule ff_gemm_to_alu_deq(talu_gemm_dispatch == True);
     ff_gemm_to_alu.deq();
    endrule

    rule ff_store_to_alu_deq(talu_store_dispatch == True);
     ff_store_to_alu.deq();
    endrule

    rule ff_alu_to_store_deq(store_dispatch == True);
     ff_alu_to_store.deq();
    endrule

    interface Put ifc_put_load_params;
      method Action put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        Load_params ld_params = unpack(pack(params));
        ff_load_queue.enq(flags);
        ff_load_params.enq(ld_params);
      endmethod
    endinterface
  
    interface Put ifc_put_store_params;
      method Action put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        Store_params st_params = unpack(pack(params));
        ff_store_queue.enq(flags);
        ff_store_params.enq(st_params);
      endmethod
    endinterface
        
    interface Put ifc_put_compute_params;
      method Action put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        Compute_params cp_params = unpack(pack(params));
        ff_gemm_queue.enq(flags);
        ff_gemm_params.enq(cp_params);
      endmethod
    endinterface
  
    interface Put ifc_put_alu_params;
      method Action put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        ALU_params alu_params = unpack(pack(params));
        ff_alu_queue.enq(flags);
        ff_alu_params.enq(alu_params);
      endmethod
    endinterface
  
    interface Get ifc_get_load_instruction;
      method ActionValue#(Load_params) get if(fn_resolve_next_pop(ff_load_queue, ff_gemm_to_load));
        ff_load_params.deq();
	load_dispatch <= ff_load_queue.first.pop_next_dep;
        return ff_load_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_load_complete;
      method Action put(Bool complete);
        fn_push_next(ff_load_queue, ff_load_to_gemm);
        ff_load_queue.deq();
      endmethod
    endinterface
  
    interface Get ifc_get_store_instruction;
      method ActionValue#(Store_params) get if(fn_resolve_prev_pop(ff_store_queue, ff_alu_to_store));
        ff_store_params.deq();
	store_dispatch <= ff_store_queue.first.pop_prev_dep;
        return ff_store_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_store_complete;
      method Action put(Bool complete);
				fn_push_prev(ff_store_queue, ff_store_to_alu);
        ff_store_queue.deq();
      endmethod
    endinterface
  
    interface Get ifc_get_gemm_instruction;
      method ActionValue#(Compute_params) get
        if(fn_resolve_prev_pop(ff_gemm_queue, ff_load_to_gemm) &&
           fn_resolve_next_pop(ff_gemm_queue, ff_alu_to_gemm));
        ff_gemm_params.deq();
	gemm_load_dispatch <= ff_gemm_queue.first.pop_prev_dep;
	gemm_alu_dispatch <= ff_gemm_queue.first.pop_next_dep;
        return ff_gemm_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_gemm_complete;
      method Action put(Bool complete);
        fn_push_prev(ff_gemm_queue, ff_gemm_to_load);
        fn_push_next(ff_gemm_queue, ff_gemm_to_alu);
        ff_gemm_queue.deq();
      endmethod
    endinterface
  
    interface Get ifc_get_alu_instruction;
      method ActionValue#(ALU_params) get
        if(fn_resolve_prev_pop(ff_alu_queue, ff_gemm_to_alu) &&
           fn_resolve_next_pop(ff_alu_queue, ff_store_to_alu));
        ff_alu_params.deq();
	talu_gemm_dispatch <= ff_alu_queue.first.pop_prev_dep;
	talu_store_dispatch <= ff_alu_queue.first.pop_next_dep;
        return ff_alu_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_alu_complete;
      method Action put(Bool complete);
        fn_push_prev(ff_alu_queue, ff_alu_to_gemm);
        fn_push_next(ff_alu_queue, ff_alu_to_store);
        ff_alu_queue.deq();
      endmethod
    endinterface

  endmodule

endpackage
