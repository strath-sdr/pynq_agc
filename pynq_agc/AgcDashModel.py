import numpy as np
import random as rnd
import os
import time
from pynq import Overlay,allocate

# Model's pure functions

def calc_envelope(t, dots):
    sorted_dots = dots.copy()
    sorted_dots.sort(key=lambda x: x[0])
    env = np.interp(t, [p[0] for p in sorted_dots],
                       [p[1] for p in sorted_dots])
    return env

def calc_ref(t, mode, fc, data_rate, random_data):
    if mode=='sin':
        return (
            np.cos(2*np.pi*fc*t),
            np.sin(2*np.pi*fc*t)
        )
    if mode=='am':
        data = 0.3*np.cos(2*np.pi*data_rate*t)+0.7
        return (
            data*np.cos(2*np.pi*fc*t),
            data*np.sin(2*np.pi*fc*t)
        )
    if mode=='fm':
        b = 3
        return (
            np.cos(2*np.pi*fc*t + b * np.cos(2*np.pi*data_rate*t)),
            np.sin(2*np.pi*fc*t + b * np.cos(2*np.pi*data_rate*t))
        )
    if mode.startswith('qpsk'):
        data_rate = fc/int(fc/data_rate)
        samples_per_sym = fc/(data_rate)

        #Generate RRC pulse shaping filter
        num_weights = 1000 # This is totally ridiculous! Only this big so we can handle large differences between data rate and f_c
                           # Might become an issue when running on the board?
        alpha = 0.5
        x = 0.9999*np.arange(-int(num_weights/2),int(num_weights/2),1)/samples_per_sym
        raised_cos_weights = np.sinc(x)*(np.cos(alpha*np.pi*x)/(1-((2*alpha*x)**2)))

        #Zero padded symbols
        qpsk_map = [1+1j, 1-1j, -1+1j, -1-1j]
        N = len(t)
        symbols = np.zeros((N,), dtype=np.cdouble)
        for i in range(int((N)/samples_per_sym)):
            symbols[int(i*samples_per_sym)] = qpsk_map[random_data[i]]

        #Modulate
        modulated = np.convolve(symbols,raised_cos_weights, mode='same')
        max_sig = max([max(modulated.real),max(modulated.imag)])
        if mode=='qpsk_if':
            modulated = modulated * np.array([np.cos(2*np.pi*fc*t) + 1j*np.sin(2*np.pi*fc*t) for t in t])
        modulated = modulated / max_sig
        return (modulated.real, modulated.imag)


class AgcDashModel():

    def __init__(self, fs = 1000000, N = 10000-2000):

        # Input paramters
        self._fs = fs
        self._N  = N

        # Cache timeseries and reusable random data
        self._random_data = [rnd.randint(0,3) for _ in range(N)]
        self._t           = np.array([i/fs for i in range(N)])

        # Overlay config
        bit_name  = os.path.dirname(__file__) + '/agc_loopback.bit'
        ol = Overlay(bit_name)
        self._ol = ol

        #Avoid PYNQ's get_attr overhead by aliasing IPs
        self._agc = ol.agc
        self._dma_in_i  = ol.dma_in_i
        self._dma_in_q  = ol.dma_in_q
        self._dma_agc_i = ol.dma_agc_i
        self._dma_agc_q = ol.dma_agc_q
        self._dma_agc_g = ol.dma_agc_g

        # Allocate buffers
        self._buf_in_i  = allocate(shape=(N,), dtype=np.int16)
        self._buf_in_q  = allocate(shape=(N,), dtype=np.int16)
        self._buf_agc_i = allocate(shape=(N,), dtype=np.int16)
        self._buf_agc_q = allocate(shape=(N,), dtype=np.int16)
        self._buf_agc_g = allocate(shape=(N,), dtype=np.uint32)

    @property
    def N(self):
        return self._N

    @property
    def fs(self):
        return self._fs

    @property
    def t(self):
        return self._t

    def ref_signal(self, signal_mode, fc, fm):
        return calc_ref(self.t, signal_mode, fc, fm, self._random_data)

    def envelope(self, handles):
        return calc_envelope(self.t, handles)

    def test_input(self, ref, envelope):
        return (ref[0]*envelope, ref[1]*envelope)

    def calc_fft(self, i, q):
        freq_y = np.fft.fftshift(20*np.log10(np.abs(np.fft.fft(np.array(i) + 1j*np.array(q)))))
        freq_x = np.fft.fftshift(np.fft.fftfreq(len(freq_y), 1/self.fs))
        return (freq_x, freq_y)


    def agc_cfg(self, en, win, ref, alpha):
        ADDR_EN    = 0
        ADDR_WIN   = 4
        ADDR_REF   = 8
        ADDR_ALPHA = 12
        #self._agc.write(ADDR_EN, en + 0)
        #time.sleep(0.01)
        self._agc.write(ADDR_EN, en + 2)
        #time.sleep(0.01)
        self._agc.write(ADDR_WIN, int(win))
        self._agc.write(ADDR_REF, int(np.log10(ref*2**15)*2**12))
        self._agc.write(ADDR_ALPHA, int(alpha*2**6))

    def agc_loopback(self, in_i, in_q):

        for i in range(len(self._buf_in_i)):
            self._buf_in_i[i] = np.rint(in_i[i]*(2**15-1))
            self._buf_in_q[i] = np.rint(in_q[i]*(2**15-1))

        self._dma_agc_i.recvchannel.transfer(self._buf_agc_i)
        self._dma_agc_q.recvchannel.transfer(self._buf_agc_q)
        self._dma_agc_g.recvchannel.transfer(self._buf_agc_g)
        self._dma_in_i.sendchannel.transfer(self._buf_in_i)
        self._dma_in_q.sendchannel.transfer(self._buf_in_q)

        self._dma_in_i.sendchannel.wait()
        self._dma_in_q.sendchannel.wait()
        self._dma_agc_q.recvchannel.wait()
        self._dma_agc_i.recvchannel.wait()
        self._dma_agc_g.recvchannel.wait()

        outs = (np.array(self._buf_agc_i)/(2**15-1),
                np.array(self._buf_agc_q)/(2**15-1),
                np.array(self._buf_agc_g)/(2**15-1))

        return outs
