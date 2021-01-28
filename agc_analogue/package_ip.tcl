# Set the reference directory for source file relative paths (by default the value is script directory path)
set origin_dir "."

# Use origin directory path location variable, if specified in the tcl shell
if { [info exists ::origin_dir_loc] } {
  set origin_dir $::origin_dir_loc
}

variable script_file
set script_file "vivado.tcl"

# Set the directory path for the original project from where this script was exported
set orig_proj_dir "[file normalize "$origin_dir/viv_prj"]"


# Create project
create_project viv_prj ./viv_prj -part xczu28dr-ffvg1517-2-e

# Set the directory path for the new project
set proj_dir [get_property directory [current_project]]

# Set project properties
set obj [get_projects viv_prj]
set_property -name "default_lib" -value "xil_defaultlib" -objects $obj
set_property -name "ip_cache_permissions" -value "read write" -objects $obj
set_property -name "target_language" -value "VHDL" -objects $obj

# Add sources
add_files { ../clash/vhdl/ ../hdl/ }
#add_files -fileset constrs_1 -norecurse ./pynq.xdc
update_compile_order -fileset sources_1

# Build IP
ipx::package_project -root_dir ./ip -vendor user.org -library user -taxonomy /UserIP -import_files -set_current false
ipx::unload_core ./ip/component.xml
ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory ./ip ./ip/component.xml
update_compile_order -fileset sources_1
set_property core_revision 1 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::save_core [ipx::current_core]
close_project -delete

# Include IP in original project
set_property  ip_repo_paths  ./ip [current_project]
update_ip_catalog

## Generate board design
#create_bd_design "design_1"
#update_compile_order -fileset sources_1
#startgroup
#create_bd_cell -type ip -vlnv user.org:user:gb_reg_v1_0:1.0 gb_reg_v1_0_0
#endgroup
#startgroup
#create_bd_cell -type ip -vlnv xilinx.com:ip:processing_system7:5.5 processing_system7_0
#endgroup
#apply_bd_automation -rule xilinx.com:bd_rule:axi4 -config {Master "/processing_system7_0/M_AXI_GP0" intc_ip "New AXI Interconnect" Clk_xbar "Auto" Clk_master "Auto" Clk_slave "Auto" }  [get_bd_intf_pins gb_reg_v1_0_0/s00_axi]
#apply_bd_automation -rule xilinx.com:bd_rule:processing_system7 -config {make_external "FIXED_IO, DDR" apply_board_preset "1" Master "Disable" Slave "Disable" }  [get_bd_cells processing_system7_0]
#startgroup
#set_property -dict [list CONFIG.PCW_FPGA0_PERIPHERAL_FREQMHZ {4}] [get_bd_cells processing_system7_0]
#endgroup
#startgroup
#make_bd_pins_external -name aud [get_bd_pins gb_reg_v1_0_0/aud]
#endgroup
#startgroup
#make_bd_pins_external -name aud_sd [get_bd_pins gb_reg_v1_0_0/aud_sd]
#endgroup
#save_bd_design
#
## Generate HDL wrapper
#make_wrapper -files [get_files ./viv_prj/viv_prj.srcs/sources_1/bd/design_1/design_1.bd] -top
#add_files -norecurse ./viv_prj/viv_prj.srcs/sources_1/bd/design_1/hdl/design_1_wrapper.vhd
#update_compile_order -fileset sources_1
#set_property top design_1_wrapper [current_fileset]
#update_compile_order -fileset sources_1
#
## Make a bitstream!
#launch_runs impl_1 -to_step write_bitstream -jobs 4
#wait_on_run impl_1
#
## Export files (board TCL is for easy Pynq Overlays)
#write_bd_tcl ./gb_audio.tcl
#file copy -force ./viv_prj/viv_prj.runs/impl_1/design_1_wrapper.bit ./gb_audio.bit
