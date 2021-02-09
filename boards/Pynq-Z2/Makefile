BITFILE = bin/agc_loopback.bit

all: $(BITFILE)

$(BITFILE):
	vivado -mode batch -source prj_loopback.tcl

clean:
	rm -r bin NA prj_loopback vivado.jou vivado.log
