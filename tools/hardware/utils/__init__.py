#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#


def align_up(num, bits):
    ''' align a number up to a bit-boundary '''
    boundary = 1 << bits
    return (num + (boundary - 1)) & ~(boundary - 1)


def align_down(num, bits):
    ''' align a number down to a bit-boundary '''
    boundary = 1 << bits
    return num & ~(boundary - 1)
