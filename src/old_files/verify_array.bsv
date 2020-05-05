/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and/or other materials provided 
 with the distribution.  
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Author: Gokulan Ravi
Email id: gokulan97@gmail.com
Details: TestBench to test the MAC Array

--------------------------------------------------------------------------------------------------
*/

package verify_array;

  import intMul_WS::*;
  import Vector::*;
  import GetPut::*;
  import BRAM::*;
  import Connectable::*;
  import RegFile::*;

  `define NUM_ROWS 8
  `define NUM_COLS 2
  `define MUL_WIDTH 16
//  `define VERBOSE
/*
NUM_ROWS denotes the number of rows of PEs in the systolic array. Since the dataflow is weight
stationary, this also denotes the total number of elements present per filter.
NUM_COLS denotes the number of columns of PEs in the systolic array. In our case, it also denotes
the number of filters we use.
MUL_WIDTH denotes the number of bits, each of the input/weight is denoted by. The accumulated value
will occupy twice the number of bits.
*/

  module mkverifyarray(Empty);
    //Module, in which inputs and weights are stored in RegFile.
    //Weight is populated first, and then input and acc sent, finally outputs being printed
    //Works correctly, even with random stalls

    //The array, initialized
    Ifc_intMul_WS#(`MUL_WIDTH) array[`NUM_ROWS][`NUM_COLS];
    for(Integer i=0; i<`NUM_ROWS; i=i+1)begin
      for(Integer j=0; j<`NUM_COLS; j=j+1)begin
        array[i][j] <- mkintMulWS(fromInteger(i), fromInteger(j), `NUM_ROWS-i);
      end
    end

    //Vertical connections
    for(Integer i=0; i<`NUM_ROWS-1; i=i+1)begin
      for(Integer j=0; j<`NUM_COLS; j=j+1)begin
        mkConnection(array[i][j].send_acc_to_south, array[i+1][j].acc_from_north);
        mkConnection(array[i][j].to_south, array[i+1][j].from_north);
      end
    end

    //Horizontal Connections
    for(Integer i=0; i<`NUM_ROWS; i=i+1)begin
      for(Integer j=0; j<`NUM_COLS-1; j=j+1)begin
        mkConnection(array[i][j].to_east, array[i][j+1].from_west);
      end
    end

    //BRAM for storing weights
    BRAM_Configure cfg = defaultValue;
    cfg.memorySize = `NUM_ROWS;
  
    //BRAM2Port#( Bit#(TLog#(`NUM_ROWS)), Bit#(TMul#(`NUM_COLS, `MUL_WIDTH)) ) weights_bram <- mkBRAM2Server(cfg);
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

    function BRAMRequest#(Bit#(TLog#(`NUM_ROWS)), Bit#(TMul#(`NUM_COLS, `MUL_WIDTH))) makeWeightRequest 
      (Bit#(TLog#(`NUM_ROWS)) addr, Bit#(TMul#(`NUM_COLS, `MUL_WIDTH)) data);
      return BRAMRequest{
        write: False,
        responseOnWrite: True,
        address : addr,
        datain : data
      };
    endfunction

    //rule rl_put_weight_request(rg_weight_address != fromInteger(`NUM_ROWS-1));
    //  weights_bram.portA.request.put(makeWeightRequest(rg_weight_address, ?));
    //  rg_weight_address <= rg_weight_address + 1;
    //  `ifdef VERBOSE
    //    $display("Sending Weight Request for address %d at time %d\n", rg_weight_address, $time);
    //  `endif
    //endrule

    rule rl_get_weight_response(!rg_done);
      //let weights <- weights_bram.portA.response.get;
      let weights = weights_regfile.sub(rg_weight_address);
      rg_weight_address <= rg_weight_address + 1;

      `ifdef VERBOSE
        $display("Received Weight Response %h for address %d at time %d\n", weights, rg_weight_address-1, $time);
      `endif

      let tym <- $time;
      
      for(Integer i=0; i<`NUM_COLS; i=i+1)begin
        Bit#(`MUL_WIDTH) weight = weights[(i+1)*`MUL_WIDTH-1:i*`MUL_WIDTH];
        //send the value weight to (`NUM_COLS-i-1)th column.
        //$display("Weight %d to column %d %h\n", weight, `NUM_COLS-i-1, weights);

        Bit#(8) coord;
        if(rg_weight_address == fromInteger(`NUM_ROWS-1))
          coord = 'b1000;//fromInteger(`NUM_ROWS);
        else
          coord = zeroExtend(rg_weight_address) + 1;
          
        array[0][`NUM_COLS-i-1].from_north.put(tuple3(tagged Valid weight, coord, 'b00));
        `ifdef VERBOSE
          $display("Sending weight %d to Column %d at time %d\n", weight, `NUM_COLS-i-1, $time);
        `endif
        //weight = weight >> `MUL_WIDTH;
      end

      if(rg_weight_address == fromInteger(`NUM_ROWS-1))begin
        rg_done <= True;
        //`ifdef VERBOSE
          $display("Populating Weights done at time %d\n", $time);
        //`endif
        //$finish(0);
      end
    endrule

    Reg#(Bool) rg_ping <- mkReg(False);
    rule rl_ping;
      rg_ping <= !rg_ping;
    endrule

    for(Integer i=0; i<`NUM_ROWS; i=i+1)begin
      rule rl_put_input(rg_ping && rg_done && !rg_done2 && rg_input==fromInteger(i));
        let tym <- $time;
        let input_value = input_regfile.sub(rg_input);
        array[i][0].from_west.put(tagged Valid input_value);
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

    Reg#(Bool) rg_pong <- mkReg(False);
    rule rl_pong;
      rg_pong <= !rg_pong;
    endrule

    for(Integer i=0; i<`NUM_COLS; i=i+1)begin
      rule rl_put_acc(rg_pong && rg_done && !rg_done3 && rg_accs == fromInteger(i));
        array[0][i].acc_from_north.put(0);
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
      rule rl_get_acc(rg_done && !rg_done4 && rg_outs == fromInteger(i));
        let acc <- array[`NUM_ROWS-1][i].send_acc_to_south.get;
        output_regfile.upd(rg_outs, acc);
        rg_outs <= rg_outs + 1;
        //`ifdef VERBOSE
          $display("Receiving acc %d from column %d at time %d\n", acc, i, $time);
        //`endif
        if(rg_outs == fromInteger(`NUM_COLS-1))begin
          rg_done4 <= True;
          //`ifdef VERBOSE
            $display("Received all outputs at time %d\n", $time);
          //`endif
        end
      endrule
    end

    rule end_sim(rg_done4);
      $finish(0);
    endrule
  endmodule
endpackage
