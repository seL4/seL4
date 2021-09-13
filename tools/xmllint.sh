#!/usr/bin/env bash
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

if [ 0$V -ge 3 ]; then
    set -x
fi

set -e

xmllint "$@" 2> >(grep -v --regexp='validates$' 1>&2)
