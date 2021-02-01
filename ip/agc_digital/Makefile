CLASH = clash -fconstraint-solver-iterations=20
CLASH_SRC = clash/$(CLASH_SRC_NAME)
CLASH_SRC_NAME = DigitalAGC.hs
VHDL = clash/vhdl/DigitalAGC/digitalAgc/digitalAgc.vhdl
IP = ip/component.xml

all: $(IP)

$(VHDL): $(CLASH_SRC)
	cd clash; $(CLASH) --vhdl $(CLASH_SRC_NAME);

$(IP): $(VHDL)
	vivado -mode batch -source package_ip.tcl

clean:
	rm -r clash/vhdl ip vivado.jou vivado.log vivado/; find clash -type f \( -name '*.hi' -o -name '*.o' \) -delete
