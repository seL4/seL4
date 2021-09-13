#!/usr/bin/env bash
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

#
# Concatinate files together, adding in appropriate "#line" directives.
#

while [ $# -ge 1 ]; do
    echo "#line 1 \"$1\""
    cat "$1"
    shift
done

