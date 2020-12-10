CLASH = clash
BUILD_DIR = build/clash/AGC.hs
VHDL = build/clash/vhdl/Main/agc/agc.vhd

all: vivado_loopback

vhdl: $(VHDL)

build_dir: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir build
	cp -r src/* build

$(VHDL): $(BUILD_DIR)
	cd build/clash; $(CLASH) -fclash-hdlsyn Vivado --vhdl AGC.hs;

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
