if { $argc != 2 } {
  puts "Please pass the top module name that needs to be synthesized along with the fpga part"
  puts " -tclargs <mkTbSoc> <xc7a100tcsg324-1>"
  exit 2
} else {
  puts "Synthesizing with Top Module: [lindex $argv 0] "
}


set curdir [ file dirname [ file normalize [ info script ] ] ]
source $curdir/env.tcl

# create folders
file mkdir $fpga_dir

# create project
create_project -force $core_project -dir $core_project_dir -part [lindex $argv 1]

# Set project properties
set obj [get_projects $core_project]
set_property "default_lib" "xil_defaultlib" $obj
set_property "simulator_language" "Mixed" $obj

# Create 'sources_1' fileset (if not found)
if {[string equal [get_filesets -quiet sources_1] ""]} {
  create_fileset -srcset sources_1
}

# Set 'sources_1' fileset object
add_files -norecurse -fileset [get_filesets sources_1] $home_dir/verilog/
add_files -norecurse -fileset [get_filesets sources_1] "/home/vin/hw_tools/bluespec/Bluespec-2019.05.beta2-legacy/lib/Verilog/"

# add include path
set_property include_dirs $home_dir/verilog/ [get_filesets sources_1]

# Set 'sources_1' fileset properties
set_property "TOP" "mktop_tb" [get_filesets sources_1]

# Create 'constrs_1' fileset (if not found)
if {[string equal [get_filesets -quiet constrs_1] ""]} {
  create_fileset -constrset constrs_1
}

# Set 'constrs_1' fileset object
set obj [get_filesets constrs_1]

# Add/Import constrs file and set constrs file properties
#set file "[file normalize "$core_project_dir/constraints/constraints.xdc"]"
#set file_added [add_files -norecurse -fileset $obj $file]

# generate all IP source code
#generate_target all [get_ips]

# force create the synth_1 path (need to make soft link in Makefile)
if {[string equal [get_runs -quiet core_synth_1] ""]} {
    create_run -flow {Vivado Synthesis 2016} -part [lindex $argv 1] -strategy\
"Vivado Synthesis Defaults" -constrset constrs_1 core_synth_1
} else {
    set_property strategy "Vivado Synthesis Defaults" [get_runs core_synth_1]
    set_property flow "Vivado Synthesis 2016" [get_runs core_synth_1]
}
# do not flatten design
set_property STEPS.SYNTH_DESIGN.ARGS.FLATTEN_HIERARCHY none [get_runs core_synth_1]

current_run -synthesis [get_runs core_synth_1]
#et_property strategy Flow_PerfOptimized_high [get_runs core_synth_1]

# Create 'impl_1' run (if not found)
if {[string equal [get_runs -quiet core_impl_1] ""]} {
  create_run -part [lindex $argv 1] -flow {Vivado Implementation 2016} -strategy\
 "Vivado Implementation Defaults" -constrset constrs_1 -parent_run core_synth_1 core_impl_1
} else {
  set_property strategy "Vivado Implementation Defaults" [get_runs core_impl_1]
  set_property flow "Vivado Implementation 2016" [get_runs core_impl_1]
}
set obj [get_runs core_impl_1]
set_property -name "part" -value [lindex $argv 1] -objects $obj
set_property -name "steps.write_bitstream.args.readback_file" -value "0" -objects $obj
set_property -name "steps.write_bitstream.args.verbose" -value "0" -objects $obj

# set the current impl run
current_run -implementation [get_runs core_impl_1]

puts "INFO: Project created:project_1"
exit
