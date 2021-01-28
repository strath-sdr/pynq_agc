2-stage AGC:
  1) Using thresholds to get *analogue* signal within approx range (for good sampling resolution and protection of ADC)
  2) Using a traditional AGC design to keep *digital* signal at a more specific range to assist with synchronisation/receiver stability
  
# 1)

Basic hysteresis and thresholds

Want to allow user to control attack and decay rates.

Note that the datasheet shows us how Vc affects gain... it's not quite linear
*in dB!*. We need to add something to the end of this to translate from ratio ->
db -> bits. While there, add a register to disable the output and handle the 4th
condition of thresholds.

for gain vs v_c
https://www.nooelec.com/store/downloads/dl/file/id/103/product/334/vega_datasheet_revision_1.pdf

for dsp bit widths https://www.xilinx.com/support/documentation/user_guides/ug579-ultrascale-dsp.pdf

for some background on approximations https://www.wiley.com/en-nl/Arithmetic+Circuits+for+DSP+Applications-p-9781119206774

docs for curve fitting https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.curve_fit.html

textbook for the eetimes blog https://www.vlebooks.com/vleweb/Product/Index/40057?page=0
eetimes blog for AGC https://www.eetimes.com/wireless-101-automatic-gain-control-agc/#

# 2)

See this excerpt for good discussion of AGC https://www.eetimes.com/wireless-101-automatic-gain-control-agc/#

Good discussion of different detector types here https://www.analog.com/media/en/training-seminars/tutorials/42575412022953450461111812375Design_and_Operation_of_AGC_Loops.pdf


# Meeting notes

On AGC design, been looking at making our control signal control the gain linearly.
See datasheet plot.

Includes some fun digital arithmetic, picking ranges so we can use certain log approximations, using python to model the response and generate it's inverse. Good stuff.

Physical AGC... Looked like there were two options for our control --- hijacking the buttons or generating our own analog signal. hoped to use the buttons. Doesn't look like I can, but will still make it work with the equipment a hobbyist is likely to have lying around. Without more info, I don't know the RF characteristics. There's a twitter post announcing this board that suggests automatic control is possible. I pestered them and have opened a wee nooelec support ticket. This post also says that the cheap $50 deal is temporary... not sure what the price will go up to.