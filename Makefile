IP_AAGC = ip/agc_analogue/ip/component.xml
IP_DAGC = ip/agc_digital/ip/component.xml

all: $(IP_AAGC) $(IP_DAGC)

$(IP_AAGC):
	make -C ip/agc_analogue

$(IP_DAGC):
	make -C ip/agc_digital

clean:
	make -C ip/agc_analogue clean; make -C ip/agc_digital clean
