import numpy as np
import random as rnd
import os
from pynq import allocate,DefaultHierarchy
from .BasebandAGC import calc_ref,calc_envelope
from .rf_agc.AgcDashController import AgcDashController

# Only import RFSoC libraries if we find this hierarchy
# Makes Pynq-Z2 support a little more simple
if os.getenv('BOARD') == 'ZCU111' or os.getenv('BOARD') == 'RFSoC2x2':
    import xrfdc
    import xrfclk

class RFAGC(DefaultHierarchy):

    @staticmethod
    def checkhierarchy(description):
        if 'analogueAgc_v1_0_0' in description['ip'] \
           and 'axisSync' in description['ip'] \
           and 'dma_rf_rx' in description['ip'] \
           and 'dma_rf_tx' in description['ip'] \
           and 'rf' in description['ip']:
            return True
        return False

    def __init__(self, description, fs_ctrl = 100e6, fs = 1.024e9, N = 4*32768):

        super().__init__(description)


        # TODO Set ref clks for RFSoC with 2x2 reset

        # Input paramters
        self._fs_ctrl = fs_ctrl
        self._fs = fs
        self._N  = N

        # Cache timeseries and reusable random data
        self._random_data = [rnd.randint(0,3) for _ in range(N)]
        self._t           = np.array([i/fs for i in range(N)])

        # Alias for RF DC components
        self.adc_tile = self.rf.adc_tiles[0]
        self.adc_block = self.adc_tile.blocks[0]
        self.dac_tile = self.rf.dac_tiles[1]
        self.dac_block = self.dac_tile.blocks[2]
        self.thres_low = self.adc_block.thresholds[0]
        self.thres_high = self.adc_block.thresholds[1]

        # Allocate buffers
        self._buf_tx  = allocate(shape=(N,), dtype=np.int16)
        self._buf_rx  = allocate(shape=(N,), dtype=np.int16)

    @property
    def N(self):
        return self._N

    @property
    def fs(self):
        return self._fs

    @property
    def fs_ctrl(self):
        return self._fs_ctrl

    @property
    def t(self):
        return self._t

    def ref_signal(self, signal_mode, fc, fm):
        return calc_ref(self.t, signal_mode, fc, fm, self._random_data)[0]

    def envelope(self, handles):
        return calc_envelope(self.t, handles)

    def test_input(self, ref, envelope):
        return ref*envelope

    def calc_fft(self, x):
        freq_y = np.fft.fftshift(20*np.log10(np.abs(np.fft.fft(np.array(x)))))
        freq_x = np.fft.fftshift(np.fft.fftfreq(len(freq_y), 1/self.fs))
        return (freq_x, freq_y)

    def agc_cfg(self, en, atk_step, atk_t, dec_step, dec_t, max_gain):
        ADDR_ATKSTEP = 0
        ADDR_ATKN    = 1 * 4
        ADDR_DECSTEP = 2 * 4
        ADDR_DECN    = 3 * 4
        ADDR_MAXG    = 4 * 4
        ADDR_EN      = 5 * 4
        self.analogueAgc_v1_0_0.write(ADDR_EN, int(en))
        self.analogueAgc_v1_0_0.write(ADDR_MAXG, int(max_gain*2**18))
        self.analogueAgc_v1_0_0.write(ADDR_ATKSTEP, int(atk_step*2**18))
        self.analogueAgc_v1_0_0.write(ADDR_DECSTEP, int(dec_step*2**18))
        self.analogueAgc_v1_0_0.write(ADDR_ATKN, int(atk_t * self.fs_ctrl))
        self.analogueAgc_v1_0_0.write(ADDR_DECN, int(dec_t * self.fs_ctrl))

    def threshold_cfg(self, low, high, hyst_t):

        hyst_cycles = round(hyst_t * self.fs / 8)

        self.thres_low.Settings = {
            'ThresholdMode': xrfdc.TRSHD_HYSTERISIS,
            'ThresholdAvgVal': int(hyst_cycles),
            'ThresholdOverVal': int(low * (2**14-1)),
            'ThresholdUnderVal': int(low * (2**14-1))
        }
        self.thres_high.Settings = {
            'ThresholdMode': xrfdc.TRSHD_HYSTERISIS,
            'ThresholdAvgVal': int(hyst_cycles),
            'ThresholdOverVal': int(high * (2**14-1)),
            'ThresholdUnderVal': int(high * (2**14-1))
        }

    def agc_loopback(self, ins):

        ADDR_PKT_LEN = 0x0
        ADDR_TRANS   = 0x4

        self._buf_tx[:] = np.rint(ins*(2**15-1))

        self.axisSync.write(ADDR_PKT_LEN, int(len(self._buf_rx)/8))
        self.dma_rf_tx.sendchannel.transfer(self._buf_tx)
        self.dma_rf_rx.recvchannel.transfer(self._buf_rx)
        self.axisSync.write(ADDR_TRANS, 1)
        self.dma_rf_rx.recvchannel.wait()
        self.dma_rf_tx.sendchannel.wait()
        self.axisSync.write(ADDR_TRANS, 0)

        outs = np.array(self._buf_rx)/(2**15-1)

        return outs

    def gui(self, *args, **kwargs):
        ctrl = AgcDashController(self)
        return ctrl.show(*args, **kwargs)
