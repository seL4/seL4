#!/bin/bash
#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

if [ 0$V -ge 3 ]; then
    set -x
fi

set -e

xmllint "$@" 2> >(grep -v --regexp='validates$' 1>&2)
