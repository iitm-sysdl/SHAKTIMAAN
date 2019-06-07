package verify_with_buffer;
  import systolic::*;
  import Connectable::*;
  import RegFile::*;

  `define NUM_ROWS 8
  `define NUM_COLS 2
  `define MUL_WIDTH 16
  `define VERBOSE

  module mkverifywithbuffer(Empty);

    Ifc_systolic#(`NUM_ROWS, `NUM_COLS, `MUL_WIDTH) systolic_array <- mksystolic;
 
    RegFile#(Bit#(TLog#(`NUM_ROWS)), Bit#(TMul#(`NUM_COLS, `MUL_WIDTH))) weights_regfile <- mkRegFileFullLoad("./src/weights_regfile.txt");
    RegFile#(Bit#(TLog#(`NUM_ROWS)), Bit#(`MUL_WIDTH)) input_regfile <- mkRegFileFullLoad("./src/input_regfile.txt");
    RegFile#(Bit#(TLog#(`NUM_COLS)), Bit#(TMul#(2, `MUL_WIDTH))) output_regfile <- mkRegFile(0, fromInteger(`NUM_COLS-1));
    Reg#(Bit#(TLog#(`NUM_ROWS))) rg_weight_address <- mkReg(0);
    Reg#(Bool) rg_done <- mkReg(False);
    Reg#(Bit#(TLog#(`NUM_ROWS))) rg_input <- mkReg(0);
    Reg#(Bool) rg_done2 <- mkReg(False);
    Reg#(Bit#(TLog#(`NUM_COLS))) rg_accs <- mkReg(0);
    Reg#(Bool) rg_done3 <- mkReg(False);
    Reg#(Bit#(TLog#(`NUM_COLS))) rg_outs <- mkReg(0);
    Reg#(Bool) rg_done4 <- mkReg(False);

    rule rl_get_weight_response(!rg_done);
      let weights = weights_regfile.sub(rg_weight_address);
      rg_weight_address <= rg_weight_address + 1;

      `ifdef VERBOSE
        $display("Received Weight Response %h for address %d at time %d\n", weights, rg_weight_address-1, $time);
      `endif

      for(Integer i=0; i<`NUM_COLS; i=i+1)begin
        Bit#(`MUL_WIDTH) weight = weights[(i+1)*`MUL_WIDTH-1:i*`MUL_WIDTH];
        //send the value weight to (`NUM_COLS-i-1)th column.
        //$display("Weight %d to column %d %h\n", weight, `NUM_COLS-i-1, weights);

        Bit#(8) coord;
        if(rg_weight_address == fromInteger(`NUM_ROWS-1))
          coord = 'b1000;//fromInteger(`NUM_ROWS);
        else
          coord = zeroExtend(rg_weight_address) + 1;
          
        systolic_array.cfifo[`NUM_COLS-i-1].send_colbuf_value(tuple4(tagged Valid weight, 'b0, coord, 'b00));
        `ifdef VERBOSE
          $display("Sending weight %d to Column %d at time %d\n", weight, `NUM_COLS-i-1, $time);
        `endif
      end

      if(rg_weight_address == fromInteger(`NUM_ROWS-1))begin
        rg_done <= True;
        `ifdef VERBOSE
          $display("Populating Weights done at time %d\n", $time);
        `endif
      end
    endrule

    for(Integer i=0; i<`NUM_ROWS; i=i+1)begin
      rule rl_put_input(rg_done && !rg_done2 && rg_input==fromInteger(i));
        let input_value = input_regfile.sub(rg_input);
        systolic_array.rfifo[i].send_rowbuf_value(tagged Valid input_value);
        rg_input <= rg_input + 1;
        `ifdef VERBOSE
          $display("Sending input %d to row %d at time %d\n", input_value, i, $time);
        `endif
        if(rg_input == fromInteger(`NUM_ROWS-1))begin
          rg_done2 <= True;
          `ifdef VERBOSE
            $display("Sending all inputs completed at time %d\n", $time);
          `endif
        end
      endrule
    end

    for(Integer i=0; i<`NUM_COLS; i=i+1)begin
      rule rl_put_acc(rg_done && !rg_done3 && rg_accs == fromInteger(i));
        systolic_array.cfifo[i].send_acc_value(0);
        rg_accs <= rg_accs + 1;
        `ifdef VERBOSE
          $display("Sending Acc 0 to column %d at time %d\n", i, $time);
        `endif
        if(rg_accs == fromInteger(`NUM_COLS-1))begin
          rg_done3 <= True;
          `ifdef VERBOSE
            $display("Sending all accs completed at time %d\n", $time);
          `endif
        end
      endrule
    end

    for(Integer i=0; i<`NUM_COLS; i=i+1)begin
      rule rl_get_acc(!rg_done4 && rg_outs == fromInteger(i));
        let acc <- systolic_array.cfifo[i].send_accumbuf_value;
        output_regfile.upd(rg_outs, acc);
        rg_outs <= rg_outs + 1;
        `ifdef VERBOSE
          $display("Receiving acc %d from column %d at time %d\n", acc, i, $time);
        `endif
        if(rg_outs == fromInteger(`NUM_COLS-1))begin
          rg_done4 <= True;
          $finish(0);
          `ifdef VERBOSE
            $display("Received all outputs at time %d\n", $time);
          `endif
        end
      endrule
    end

 endmodule

endpackage
