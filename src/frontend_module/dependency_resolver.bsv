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

  interface Ifc_dependency_resolver#(numeric type if_index, numeric type of_index, numeric type wt_index,
      numeric type ld_pad, numeric type st_pad, numeric type cp_pad, numeric type alu_pad);
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_load_params;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_store_params;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_compute_params;
    interface Put#(Tuple2#(Dep_flags, Params)) ifc_put_alu_params;
  
    interface Get#(Load_params#(ld_pad)) ifc_get_load_instruction;
    interface Get#(Store_params#(st_pad)) ifc_get_store_instruction;
    interface Get#(Compute_params#(if_index, of_index, wt_index, cp_pad)) ifc_get_gemm_instruction;
    interface Get#(ALU_params#(of_index, alu_pad)) ifc_get_alu_instruction;
  
    interface Put#(Bool) ifc_put_load_complete;
    interface Put#(Bool) ifc_put_store_complete;
    interface Put#(Bool) ifc_put_gemm_complete;
    interface Put#(Bool) ifc_put_alu_complete;
  endinterface 
  
  module mkdepResolver(Ifc_dependency_resolver#(if_index, of_index, wt_index, ld_pad, st_pad, cp_pad, alu_pad));
  
    FIFOF#(Dep_flags) ff_load_queue  <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
    FIFOF#(Dep_flags) ff_gemm_queue  <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
    FIFOF#(Dep_flags) ff_alu_queue   <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
    FIFOF#(Dep_flags) ff_store_queue <- mkSizedFIFOF(valueOf(`INS_QUEUE_SIZE));
  
    FIFOF#(Load_params#(ld_pad))  ff_load_params  <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
    FIFOF#(Store_params#(st_pad)) ff_store_params <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
    FIFOF#(Compute_params#(if_index, of_index,wt_index, cp_pad)) 
                                  ff_gemm_params  <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
    FIFOF#(ALU_params#(of_index, alu_pad)) ff_alu_params   <- mkSizedFIFOF(valueOf(`PARAMS_QUEUE_SIZE));
  
    FIFOF#(Bool) ff_load_to_gemm  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_gemm_to_load  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_gemm_to_alu   <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_alu_to_gemm   <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_alu_to_store  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
    FIFOF#(Bool) ff_store_to_alu  <- mkSizedFIFOF(valueOf(`DEP_QUEUE_SIZE));
  
    function Bool fn_resolve_prev_pop(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) dep_queue);
      Dep_flags flags = flag_queue.first;
      return !flags.pop_prev_dep || (flags.pop_prev_dep && queue.notEmpty());
    endfunction
  
    function Bool fn_resolve_next_pop(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) queue);
      Dep_flags flags = flag_queue.first;
      return !flags.pop_next_dep || (flags.pop_next_dep && queue.notEmpty());
    endfunction
  
    function void fn_push_prev(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) prev_queue);
      Dep_flags flags = flag_queue.first;
      if(flags.push_prev_dep)begin
        prev_queue.enq(True);
      end
    endfunction
  
    function void fn_push_next(FIFOF#(Dep_flags) flag_queue, FIFOF#(Bool) next_queue);
      Dep_flags flags = flag_queue.first;
      if(flags.push_next_dep)begin
        next_queue.enq(True);
      end
    endfunction
  
    interface Put ifc_put_load_params;
      method ActionValue put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        Load_params#(ld_pad) ld_params = unpack(pack(params));
        ff_load_queue.enq(flags);
        ff_load_params.enq(ld_params);
      endmethod
    endinterface
  
    interface Put ifc_put_store_params;
      method ActionValue put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        Store_params#(st_pad) st_params = unpack(pack(params));
        ff_store_queue.enq(flags);
        ff_store_params.enq(st_params);
      endmethod
    endinterface
        
    interface Put ifc_put_compute_params;
      method ActionValue put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        Compute_params#(if_index, of_index, wt_index, cp_pad) cp_params = unpack(pack(params));
        ff_gemm_queue.enq(flags);
        ff_gemm_params.enq(cp_params);
      endmethod
    endinterface
  
    interface Put ifc_put_alu_params;
      method ActionValue put(Tuple2#(Dep_flags, Params) ins);
        Dep_flags flags = tpl_1(ins);
        Params params   = tpl_2(ins);
        ALU_params#(of_index, alu_pad) alu_params = unpack(pack(params));
        ff_alu_queue.enq(flags);
        ff_alu_params.enq(cp_params);
      endmethod
    endinterface
  
    interface Get ifc_get_load_instruction;
      method ActionValue#(Load_params#(ld_pad)) get if(fn_resolve_next_pop(ff_load_queue, ff_gemm_to_load));
        ff_load_params.deq();
        return ff_load_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_load_complete;
      method ActionValue put(Bool complete);
        fn_push_next(ff_load_queue, ff_load_to_gemm);
        ff_load_queue.deq();
      endmethod
    endinterface
  
    interface Get ifc_get_store_instruction;
      method ActionValue#(Store_params#(st_pad)) get if(fn_resolve_prev_pop(ff_store_queue, ff_alu_to_store));
        ff_store_params.deq();
        return ff_store_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_store_complete;
      method ActionValue put(Bool complete);
        fn_push_prev(ff_store_queue, ff_store_to_alu);
        ff_store_queue.deq();
      endmethod
    endinterface
  
    interface Get ifc_get_gemm_instruction;
      method ActionValue#(Compute_params#(if_index, of_index, wt_index, cp_pad)) get
        if(fn_resolve_prev_pop(ff_gemm_queue, ff_load_to_gemm) &&
           fn_resolve_next_pop(ff_gemm_queue, ff_alu_to_gemm));
        ff_gemm_params.deq();
        return ff_gemm_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_gemm_complete;
      method ActionValue put(Bool complete);
        fn_push_prev(ff_gemm_queue, ff_gemm_to_load);
        fn_push_next(ff_gemm_queue, ff_gemm_to_alu);
        ff_gemm_queue.deq();
      endmethod
    endinterface
  
    interface Get ifc_get_alu_instruction;
      method ActionValue#(ALU_params#(of_index, alu_pad)) get
        if(fn_resolve_prev_pop(ff_alu_queue, ff_gemm_to_alu) &&
           fn_resolve_next_pop(ff_alu_queue, ff_store_to_alu));
        ff_alu_params.deq();
        return ff_alu_params.first;
      endmethod
    endinterface
  
    interface Put ifc_put_alu_complete;
      method ActionValue put(Bool complete);
        fn_push_prev(ff_alu_queue, ff_alu_to_gemm);
        fn_push_next(ff_alu_queue, ff_alu_to_store);
        ff_alu_queue.deq();
      endmethod
    endinterface
  
  endmodule

endpackage
