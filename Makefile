IP_AAGC = ip/agc_analogue/ip/component.xml
IP_DAGC = ip/agc_digital/ip/component.xml
Z2_LOOPBACK = boards/Pynq-Z2/bin/agc_loopback.bit
ZCU111_LOOPBACK = boards/ZCU111/bin/agc_loopback.bit

all: $(IP_AAGC) $(IP_DAGC) $(Z2_LOOPBACK) $(ZCU111_LOOPBACK)

$(IP_AAGC):
	make -C ip/agc_analogue

$(IP_DAGC):
	make -C ip/agc_digital

$(Z2_LOOPBACK): $(Z2_LOOPBACK)
	make -C boards/Pynq-Z2

$(ZCU111_LOOPBACK): $(ZCU111_LOOPBACK)
	make -C boards/ZCU111

clean:
	make -C ip/agc_analogue clean; make -C ip/agc_digital clean; make -C boards/Pynq-Z2 clean; make -C boards/ZCU111 clean;
