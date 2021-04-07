from .BasebandAGC import BasebandAGC
from .RFAGC import RFAGC
from pynq import Overlay
import os

class AgcOverlay(Overlay):
    """Overlay for AGC demo"""

    def __init__(self, bitfile_name=None, **kwargs):
        """Construct a new AgcOverlay

        bitfile_name: str
            Optional bitstream filename. If None, we use the bitstream supplied with this package.
        """

        # Generate default bitfile name
        if bitfile_name is None:
            this_dir = os.path.dirname(__file__)
            bitfile_name = os.path.join(this_dir, 'agc_loopback.bit')

        super().__init__(bitfile_name, **kwargs)
