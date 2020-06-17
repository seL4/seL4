#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#


def align_up(num, bits):
    ''' align a number up to a bit-boundary '''
    boundary = 1 << bits
    return (num + (boundary - 1)) & ~(boundary - 1)


def align_down(num, bits):
    ''' align a number down to a bit-boundary '''
    boundary = 1 << bits
    return num & ~(boundary - 1)
