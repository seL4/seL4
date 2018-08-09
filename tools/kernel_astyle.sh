#!/bin/bash

#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

kernel_sources=$(find ../src ../include ../libsel4 -name '*.[ch]')
if [[ -z $kernel_sources ]]; then
    echo "Unable to find kernel source files"
    exit 1
fi

astyle --max-instatement-indent=120 --style=otbs --pad-header --indent=spaces=4 --pad-oper ${kernel_sources}
