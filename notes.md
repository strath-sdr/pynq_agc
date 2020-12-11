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

https://www.nooelec.com/store/downloads/dl/file/id/103/product/334/vega_datasheet_revision_1.pdf

# 2)

See this excerpt for good discussion of AGC https://www.eetimes.com/wireless-101-automatic-gain-control-agc/#

Good discussion of different detector types here https://www.analog.com/media/en/training-seminars/tutorials/42575412022953450461111812375Design_and_Operation_of_AGC_Loops.pdf
