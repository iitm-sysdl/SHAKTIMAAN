# SHAKTI License header
If you are contributing to SHAKTI, then please use a BSD-3 clause header with ownership to IIT-Madras. 
If necessary, add author names along with email-ids.

# BSV Coding Guidelines

### General Variable Naming:

1. **NO** camel casing:
    * Integer thisVariableIsBAD;  **#bad**
2.  Use underscore `_` as delimeter within a variable name:
    * Integer this_variable_is_awesome ;
    * Integer thisvariablesucks; **#bad**

### Packages:
1. All package names to start with lower case only.
2. If possible only maintain a single module per package.

### Interfaces:
1. All interfaces to start with prefix: `Ifc_`
2. If an interface is defined for a single package should be named as: `Ifc_<package_name>`
3. Subinterfaces to have a prefix `subifc_`

### Modules:
1. All modules to start with prefix: `mk_`
2. If a module is defined for a single package should be named as: `mk_<package_name>`

### Registers:
1. All registers to have a prefix: `rg_`

### Wires:
1. All wires to have a prefix: `wr_`

### FIFOs:
1. All fifos to have a prefix: `ff_`

### RegFile:
1. All RegFile to have a prefix: `rg_`

### Vectors:
1. All vectors to have a prefix: `v_`

### Local Variables:
1. All local variables to have prefix: `lv_`

### Function Names:
1. All functions to have a prefix: `fn_`

### Method Names:
1. All `Action` methods to have a prefix: `ma_`
2. All `Value` methods to have a prefix: `mv_`
3. All `ActionValue` method to have a prefix: `mav_`

### Commenting (for use with bsvsphinx-plugin):
Use the `/*doc:prefix: comments */` in the following manner to allow the bsvsphinx plugin to extract information and comments from the code itself.

1.  Overview : This can be placed anywhere in the doc with the following syntax:
    ```
    /*doc:overview:
      Provide a complete overview in RST format
    */
    ```

1. **Modules**: Make sure the keyword `module` follows immediately after the `doc` attributes.
    ```
    /*doc:module: implements the fetch + decode + operand-fetch */
    module mkstage1(Ifc_stage1);
        ...
        ...
    endmodule
    ```

1. **Rules**: Make sure the keyword `rule` follows immediately after the `doc` attributes. All other rule attributes should be added before the `doc` attributes
    ``` 
    (*no_implicit_conditions*)
    /*doc:rule: this rule does awesome stuff */
    rule rl_myrule;
    endrule
    ```
2. **Registers, Wire, FIFOs**: Use the `doc` attribute just before the instance declaration:
    ```
    /*doc:reg: this register is awesome. 
     This is a multiline comment */
    Reg#(Bool) rg_myreg <- mkReg(False);

    /*doc:wire:this wire holds crazy stuff */
    Wire#(Bit#(2)) wr_yourwire <- mkWire();

    /*doc:fifo: this fifo is too big */
    FIFO#(MyStruct) ff_ourfifo <- mkFIFO();
    ```

3. **Methods**: Since `doc` is not supported by bsc over methods we need to prefix with a comment `//` as follows:
    ```
    (*always_ready*)
    /*doc:method: receives something */
    method Action ma_flush( Bit#(`vaddr) newpc);
    ``` 
    Make sure the attributes of a method are placed above the `doc` attribute.
    
4. **Subinterfaces**: Since `doc` is not supported by bsc over subinterfaces we need to prefix with a comment `//` as follows:

    ```
    /* doc:subifc: interface to receive response from fabric */
    interface Put#(InstResponse) subifc_inst_response;
    ```
    Make sure the attributes of a subinterface are placed above the `doc` attribute.

5. **Functions**: Similar to how methods are documented:
    ```
    /*doc:func: function to check triggers */
    function Bool fn_am_i_right (Int yes);
    ```
6. **Custom Notes**: For documenting custom notes not related to any rules, instances or methods:
    ```
    /*doc:note: This is a general note */
    ```
    


