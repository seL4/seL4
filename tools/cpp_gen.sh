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

#
# Concatinate files together, adding in appropriate "#line" directives.
#

while [ $# -ge 1 ]; do
    echo "#line 1 \"$1\""
    cat "$1"
    shift
done

