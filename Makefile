CLASH = clash -fconstraint-solver-iterations=20
BUILD_DIR = build/clash/DigitalAGC.hs
VHDL = build/clash/vhdl/Main/DigitalAgc/digitalAgc.vhd
IP = build/xil/ip/component.xml

all: ip

vhdl: $(VHDL)

ip: $(IP)

build_dir: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir build
	cp -r src/* build

$(VHDL): $(BUILD_DIR)
	cd build/clash; $(CLASH) -fclash-hdlsyn Vivado --vhdl DigitalAGC.hs;

$(IP): $(VHDL)
	cd build/xil; vivado -mode batch -source vivado.tcl

# vivado_loopback: verilog
# 	cd build/xil; vivado -mode tcl -source prj_loopback.tcl
# 
# vivado_raw: $(VERILOG)
# 		cd build/xil; vivado -mode batch -source prj_raw.tcl
# 
# yosys: verilog
# 	cd  build/clash/verilog/Top/fir_rsg; yosys -s ../../../../yosys/view_synth.yosys

clean:
	rm -r build
