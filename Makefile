IP_AAGC = agc_analogue/ip/component.xml
IP_DAGC = agc_digital/ip/component.xml

all: $(IP_AAGC) $(IP_DAGC)

$(IP_AAGC):
	make -C agc_analogue

$(IP_DAGC):
	make -C agc_digital

clean:
	make -C agc_analogue clean; make -C agc_digital clean
