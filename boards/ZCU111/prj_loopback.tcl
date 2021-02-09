# Create project
set proj_name "prj_loopback"
set bd_name "block_design"

create_project $proj_name "./$proj_name" -part xczu28dr-ffvg1517-2-e

# Set the directory path for the new project
set proj_dir [get_property directory [current_project]]

# Set project properties
set_property "board_part"                 "xilinx.com:zcu111:part0:1.1"    [current_project]
set_property "default_lib"                "xil_defaultlib"                  [current_project]
set_property "ip_cache_permissions"       "read write"                      [current_project]
set_property "ip_output_repo"             "$proj_dir/prj_loopback.cache/ip" [current_project]
set_property "sim.ip.auto_export_scripts" "1"                               [current_project]
set_property "simulator_language"         "Mixed"                           [current_project]
set_property "target_language"            "VHDL"                            [current_project]
set_property "xpm_libraries"              "XPM_CDC"                         [current_project]

# Include IP repo in original project
set_property  ip_repo_paths  ../../ip [current_project]
update_ip_catalog

# Generate board design
source ./$bd_name.tcl

## Generate HDL wrapper
make_wrapper -files [get_files ./$proj_name/$proj_name.srcs/sources_1/bd/$bd_name/$bd_name.bd] -top
add_files -norecurse ./$proj_name/$proj_name.srcs/sources_1/bd/$bd_name/hdl/${bd_name}_wrapper.vhd
update_compile_order -fileset sources_1
set_property top ${bd_name}_wrapper [current_fileset]
update_compile_order -fileset sources_1

# Make a bitstream!
launch_runs impl_1 -to_step write_bitstream -jobs 4
# Quickly patch in my .mem files before AGC is compiled (sorry)
exec patch_mem.sh
wait_on_run impl_1

# Export files (including HWH for PYNQ)
file mkdir ./bin
file copy -force ./$proj_name/$proj_name.srcs/sources_1/bd/$bd_name/hw_handoff/$bd_name.hwh ./bin/agc_loopback.hwh
file copy -force ./$proj_name/$proj_name.runs/impl_1/${bd_name}_wrapper.bit ./bin/agc_loopback.bit
