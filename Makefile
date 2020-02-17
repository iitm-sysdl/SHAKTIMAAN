TOP_MODULE:=mksystolic3
TOP_FILE:=systolic_top.bsv
HOMEDIR:=./
TOP_DIR:=./src
BSVBUILDDIR:=./build/
VERILOGDIR:=./verilog/
AXI4:=./fabrics/axi4
BRIDGES:=./fabrics/bridges
COMMON_BSV:=./common_bsv
FILES:= ./src/:$(AXI4):$(BRIDGES):$(COMMON_BSV)
BSVINCDIR:= .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:$(FILES)
FPGA=xc7a100tcsg324-1
export HOMEDIR=./
export TOP=$(TOP_MODULE)

default: full_clean compile link simulate
.PHONY: compile
compile:
	@echo Compiling $(TOP_MODULE)....
	@mkdir -p $(BSVBUILDDIR)
	@bsc -u -sim -simdir $(BSVBUILDDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR) -keep-fires -p $(BSVINCDIR) -g $(TOP_MODULE)  $(TOP_DIR)/$(TOP_FILE)
	@echo Compilation finished

.PHONY: link
link:
	@echo Linking $(TOP_MODULE)...
	@mkdir -p bin
	@bsc -e $(TOP_MODULE) -sim -o ./bin/out -simdir $(BSVBUILDDIR) -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC\
  -bdir $(BSVBUILDDIR) -keep-fires 
	@echo Linking finished

.PHONY: generate_verilog 
generate_verilog:
	@echo Compiling $(TOP_MODULE) in verilog ...
	@mkdir -p $(BSVBUILDDIR); 
	@mkdir -p $(VERILOGDIR); 
	@bsc -u -verilog -elab -vdir $(VERILOGDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR)\
  $(define_macros) -D verilog=True -D VERBOSITY=0 $(BSVCOMPILEOPTS) -p $(BSVINCDIR) -g $(TOP_MODULE) $(TOP_DIR)/$(TOP_FILE)\
  || (echo "BSC COMPILE ERROR"; exit 1) 

.PHONY: simulate
simulate:
	@echo Simulation...
	./bin/out 
	@echo Simulation finished. 

.PHONY: vivado_build
vivado_build: 
	@vivado -mode tcl -source $(HOMEDIR)/src/tcl/create_project.tcl -tclargs $(TOP_MODULE) $(FPGA) || (echo "Could \
not create core project"; exit 1)
	@vivado -mode tcl -source $(HOMEDIR)/src/tcl/run.tcl || (echo "ERROR: While running synthesis")

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
