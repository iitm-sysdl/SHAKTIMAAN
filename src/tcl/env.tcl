global home_dir
global fpga_dir
global core_project_dir

# set different directories as variables
set home_dir $::env(HOMEDIR)
set fpga_dir $home_dir/fpga
set core_project_dir $fpga_dir/$::env(TOP)

# set ip project name
set core_project $::env(TOP)
puts "home_dir: $home_dir"
puts "fpga_dir: $fpga_dir"
puts "core_project_dir: $core_project_dir"
