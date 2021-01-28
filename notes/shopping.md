Criteria

  1) Interface
  2) Reproducability / Availability
  3) General applicibility for RFSoC applications
  
# 1) nooelec VeGA

## Interface

Has both analog and digital control. Got buttons for a 6-bit digital control...
but there's also a PMOD-esque header at the side, exposing both!

No schematic available but I *think* we can drive these voltages (0 -> 3.3V) via
this header. We could have a 6-bit control with just a couple of jumper wires,
or analog control with an extra DAC (not via the DCs because that's only 1
Vp-p).

With digital control we should have a resolution of roughly 0.5 dB. Doesn't
sound that bad.

What about DC blocker?

## Reproducibility / Availability

Even available on amazon for the same price.

If we use digital control, all someone needs to recreate the setup is a 6 jumper
wires (potentially even a PMOD extender cable).

Very good!

## General applicibility

20dB-50dB is pretty hefty! And it's super wideband (30MHz-4000MHz), so should be
good for most RFSoC applications.

Analog filtering will become more important with such a wideband amp, right? We
need to keep analog input to ADC within 1 Vp-p, so filtering will be really
quite important if we're to get a narrow-ish signal to use much of that range
AFAIK.

# 2) TI LMH6401

Looks good too. Given the price though, I think it might not align so nicely
with Xilinx's Baby RFSoC audience. Main difference is that the gain is
configured via SPI (over USB) and the amplifier is differential rather than
single ended.

# 3) Minicircuits ZFL-2000G+

Looks very professional! Again, the price makes it a little exclusive. Other
issue is the gain control is analog 0 -> 5 V. We would need an extra DAC to
drive this (can't even just sacrifice a DC DAC block for it because that's 1
Vp-p)

# 4) AD603

Looks like it has a maximum bandwidth of 90 MHz. Perhaps it is meant for use at
IF stage only? Don't think this would be widely applicable to RFSoC designs.
