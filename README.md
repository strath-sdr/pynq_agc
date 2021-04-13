<img src="notebooks/assets/banner.png" width="100%">

# Automatic Gain Control with PYNQ

## Introduction

We present two Automatic Gain Control (AGC) circuits with interactive control of
their parameters. The first is a purely digital loopback system, so no extra
hardware is required. The second is a less complex AGC design that communicates
with the analogue world using a [VeGA
amplifier](https://www.nooelec.com/store/downloads/dl/file/id/103/product/334/vega_datasheet_revision_1.pdf)
from Nooelec and the RFSoC's ADC threshold features. The first example supports
the PYNQ-Z2, ZCU111, and RFSoC2x2 boards, while the second only supports ZCU111
and RFSoC2x2.

We'll generate various input signals, explore some interesting effects of the
AGC algorithm, and practice tweaking our parameters for best performance. The
design of this AGC example will be explored, featuring various hardware
arithmetic approximations for power estimation, logarithms and exponentiation.

![AGC widget in action](./demonstration.gif)

## Quick Start

This repository is compatible with [PYNQ images v2.6 and
greater](https://github.com/Xilinx/PYNQ/releases) for the
[Pynq-Z2](https://www.tul.com.tw/productspynq-z2.html),
[ZCU111](https://www.xilinx.com/products/boards-and-kits/zcu111.html) and the
[RFSoC2x2]().

Connect to the board with **Jupyter Lab** in a browser (not Jupyter Notebook) by
using a web browser `https://<IP address>:9090/lab`.

Open a terminal in Jupyter Lab and run the following command:

```console
root@pynq:/home/xilinx# pip3 install https://github.com/strath-sdr/pynq_agc/releases/download/v0.3/pynq_agc.tar.gz
```

The notebook should now be available in the `pynq_agc` folder in your Jupyter
Workspace.

For the ZCU111 we'll need to patch the `xrfdc` driver in order to access the ADC's thresholding features. See the section below for instructions for patching this driver.

## Patching the ZCU111 xrfdc driver

This procedure will overwrite the `xrfdc`'s `__init__.py`. You will not lose any
xrfdc functionality --- you'll only gain thresholding capabilities and access to
a few new registers.

In the terminal window of Jupyter Lab, run the following to grab the file from our fork:

```console
root@pynq:/home/xilinx# git clone https://github.com/dnorthcote/ZCU111-PYNQ
root@pynq:/home/xilinx# cp ZCU111-PYNQ/ZCU111/packages/xrfdc/pkg/xrfdc/__init__.py /usr/local/lib/python3.6/dist-packages/xrfdc/__init__.py
```

## Building from Source

Our hardware design is written in [Clash](https://clash-lang.org/) and
implemented with Vivado 2020.1.

On Linux the only prerequisite is having Vivado 2020.1 installed and in your
`PATH` variable. We supply a [nix shell](https://nixos.org/) which will handle
the rest of the dependencies for you.

If you don't already have the nix package manager, install it with:

```console
sdr@strath$ curl -L https://nixos.org/nix/install | sh
```

To rebuild our project, we just need to source our nix shell (this might take a
while to run the first time --- it's gathering all of the software we need!) and
run `make`.

```console
sdr@strath$ nix-shell
sdr@strath$ make
```

The final pip-installable archive is found at `./pynq_agc.tar.gz`

For Windows (untested), you will need to [manually install Clash](https://clash-lang.org/install/windows/) and all of the Haskell packages listed in `shell.nix`. Alternatively you can use the Linux instructions with the Windows Subsystem for Linux (WSL).

## License
[BSD 3-Clause](./LICENSE)
