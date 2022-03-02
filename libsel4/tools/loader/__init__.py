
# Copyright 2022, seL4 Project a Series of LF Projects, LLC
#
# SPDX-License-Identifier: BSD-2-Clause or GPL-2.0-only

import os
import sys
import pathlib

# Add the parent module to the sys.path, do so in this __init__.py
# so that the code producing the side-effect can occur after import
# statements.  Otherwise autopep8 would want to reformat this code
# so the side-effect occurs after all import statements.
sys.path.append(str(pathlib.Path(os.path.dirname(os.path.realpath(__file__))).parent))
