BITFILE = agc_loopback.bit

all: $(BITFILE)

$(BITFILE):
	vivado -mode batch -source prj_loopback.tcl

clean:
	rm -r *.bit *.hwh NA prj_loopback vivado.jou vivado.log
