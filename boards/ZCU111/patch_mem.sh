#!/bin/bash
cp ../../ip/agc_digital/clash/vhdl/DigitalAGC/digitalAgc/*.mem $(find prj_loopback/prj_loopback.srcs/sources_1/bd/block_design/ipshared -name 'digitalAgc.vhdl' -printf '%h\n')
