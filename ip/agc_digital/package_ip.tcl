set prj "vivado"

# Create project
create_project $prj ./$prj -part xczu28dr-ffvg1517-2-e

# Set project properties
set obj [get_projects $prj]
set_property -name "default_lib" -value "xil_defaultlib" -objects $obj
set_property -name "ip_cache_permissions" -value "read write" -objects $obj
set_property -name "target_language" -value "VHDL" -objects $obj

# Add sources
add_files { clash/vhdl/ hdl/ }
add_files { clash/vhdl/DigitalAGC/digitalAgc/loglut.mem }
add_files { clash/vhdl/DigitalAGC/digitalAgc/antiloglut.mem }
update_compile_order -fileset sources_1

# Build IP
ipx::package_project -root_dir ./ip -vendor user.org -library user -taxonomy /UserIP -import_files -set_current false
ipx::unload_core ./ip/component.xml
ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory ./ip ./ip/component.xml
update_compile_order -fileset sources_1
set_property core_revision 1 [ipx::current_core]
set_property supported_families {zynquplus Production qzynq Beta zynquplus Beta azynq Beta zynq Beta} [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::save_core [ipx::current_core]
close_project -delete
