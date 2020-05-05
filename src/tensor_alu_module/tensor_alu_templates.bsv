package tensor_alu_templates;

//A Dummy module to simply compile scalar_alu
(*synthesize*)
module mkscalardummy(Ifc_scalar_alu#(32));
    let ifc();
    mk_scalar_alu inst(ifc);
    return (ifc);
endmodule

//A Dummy module to simply compile vector_alu
(*synthesize*)
module mkvectordummy(Ifc_vector_alu#(32, 16));
    let ifc();
    mk_vector_alu inst(ifc);
    return (ifc);
endmodule

//A Dummy module to simply compile tensor_alu
(*synthesize*)
module mktensordummy(Ifc_tensor_alu#(32, 16));
    let ifc();
    mk_tensor_alu inst(ifc);
    return (ifc);
endmodule

endpackage