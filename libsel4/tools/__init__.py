#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

# This file allows the the libsel4 tools directory to be imported as
# a python module.

# The following import of `loader` avoids a python module loading error
# When this is used as a module in manual/tools/libsel4_tools.
# Traceback (most recent call last):
#  File "/path/to/syscall_stub_gen.py", line 46, in <module>
#    from condition import condition_to_cpp
# ModuleNotFoundError: No module named 'condition'
#
# Imports for modules differ from imports for scripts
# Such that when run as a script in tools/ it includes the current
# directory in the search path. But not when imported as a module.
# Importing `loader` will add the current directory to the search path.
#
# It exists as a module so that autopep8 will not reorder the
# code on code formatting. So that the path modification
# needed before import syscall_stub_gen occurs after the import.
#
# Upgrading autopep8 to 1.5.4 would offer an alternate solution:
# fmt: off
# path modification code
# imports.
# fmt: on
from . import loader
from . import condition
from . import syscall_stub_gen
