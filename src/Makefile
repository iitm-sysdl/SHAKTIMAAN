TOP_MODULE:=mktop_tb
TOP_FILE:=accelerator.bsv
HOMEDIR:=./
TOP_DIR:=./
BSVOUTDIR:=./bin/
BSVBUILDDIR:=./build/
VERILOGDIR:=./verilog/
AXI4:=../../../fabrics/axi4:../../fabrics/axi4lite
AXI4_Lite:=../../../fabrics/axi4lite
BRIDGES:=../../../fabrics/bridges
COMMON_BSV:=../../../common_bsv:../../bram/
#TB:=./tb
FILES:=./commons/:./systolic/:./load_module:./store_module:./compute_module:./tensor_alu_module:./buffers_module:./frontend_module:$(AXI4):$(BRIDGES):$(COMMON_BSV)
BSVINCDIR:= .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:$(FILES)
FPGA=xc7a100tcsg324-1
export HOMEDIR=./
export TOP=$(TOP_MODULE)

default: full_clean compile link simulate
.PHONY: compile
compile:
	@echo Compiling $(TOP_MODULE)....
	@mkdir -p $(BSVBUILDDIR)
	@bsc -u -sim -simdir $(BSVBUILDDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR) -p $(BSVINCDIR) -g $(TOP_MODULE)  $(TOP_DIR)/$(TOP_FILE)
	@echo Compilation finished

.PHONY: link
link:
	@echo Linking $(TOP_MODULE)...
	@mkdir -p bin
	@bsc -e $(TOP_MODULE) -sim -o ./bin/out -simdir $(BSVBUILDDIR) -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC\
  -bdir $(BSVBUILDDIR) 
	@echo Linking finished

.PHONY: generate_verilog 
generate_verilog:
	@echo Compiling $(TOP_MODULE) in verilog ...
	@mkdir -p $(BSVBUILDDIR); 
	@mkdir -p $(VERILOGDIR); 
	@bsc -u -verilog -elab -vdir $(VERILOGDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR)\
  $(define_macros) -D verilog=True -D VERBOSITY=0 $(BSVCOMPILEOPTS) -no-warn-action-shadowing -p $(BSVINCDIR) -g $(TOP_MODULE) $(TOP_DIR)/$(TOP_FILE)\
  || (echo "BSC COMPILE ERROR"; exit 1) 
	@cp ${BLUESPECDIR}/Verilog.Vivado/RegFile.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2BELoad.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2BE.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/FIFO2.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/FIFO1.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/FIFO10.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/RevertReg.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/FIFO20.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/FIFOL1.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SyncFIFO.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/Counter.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SizedFIFO.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/RegFileLoad.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SyncReset0.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SyncRegister.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/MakeClock.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/UngatedClockMux.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/ClockInverter.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/MakeResetA.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/MakeReset0.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SyncResetA.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/ResetEither.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SyncHandshake.v ${VERILOGDIR}
	@cp ${BLUESPECDIR}/Verilog/SyncFIFO1.v ${VERILOGDIR}

.PHONY: link_verilator
link_verilator: ## Generate simulation executable using Verilator
	@echo "Linking $(TOP_MODULE) using verilator"
	@mkdir -p $(BSVOUTDIR) obj_dir
	@echo "#define TOPMODULE V$(TOP_MODULE)" > sim_main.h
	@echo '#include "V$(TOP_MODULE).h"' >> sim_main.h
	verilator $(VERILATOR_FLAGS) --cc $(TOP_MODULE).v -y $(VERILOGDIR) -y common_verilog --exe
	@ln -f -s ../sim_main.cpp obj_dir/sim_main.cpp
	@ln -f -s ../sim_main.h obj_dir/sim_main.h
	make $(verilate_fast) VM_PARALLEL_BUILDS=1 -j8 -C obj_dir -f V$(TOP_MODULE).mk
	@cp obj_dir/V$(TOP_MODULE) $(BSVOUTDIR)/out


.PHONY: simulate
simulate:
	@echo Simulation...
	./bin/out 
	@echo Simulation finished. 

.PHONY: vivado_build
vivado_build: 
	@vivado -mode tcl -source $(HOMEDIR)/tcl/create_project.tcl -tclargs $(TOP_MODULE) $(FPGA) || (echo "Could \
not create core project"; exit 1)
	@vivado -mode tcl -source $(HOMEDIR)/tcl/run.tcl || (echo "ERROR: While running synthesis")

.PHONY: clean
clean:
	rm -rf build bin *.jou *.log

.PHONY: full_clean
full_clean: clean
	rm -rf verilog fpga mk*

.PHONY: restore
restore: full_clean

.PHONY: release 
release:
	rm -rf $(REL_DIR)
	mkdir -p $(REL_DIR)
	mv mk* $(REL_DIR)
