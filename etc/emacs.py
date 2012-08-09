"""Wrapper for version-specific implementations of python.el helper
functions """

import sys

if sys.version_info[0] == 3:
    from emacs3 import *
else:
    from emacs2 import *

