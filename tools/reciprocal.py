#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

# this script can be used to calculate the reciprocal for
# unsigned division of an unknown 64 bit value by a known 64bit
# constant. It is used to calculate the correct magic numbers
# for fixed cpu times on arm in order to do 64 bit division to calculate
# ticks -> microseconds.

# for details on how this script works,
# see Hacker's delight, Chapter 10, unsigned division.
from math import floor, ceil
import argparse
import sys
from past.builtins import xrange

# now unsigned


def magicgu(nmax, d):
    nc = ((nmax + 1)//d)*d - 1
    nbits = len(bin(nmax)) - 2
    for p in range(0, 2*nbits + 1):
        if 2**p > nc*(d - 1 - (2**p - 1) % d):
            m = (2**p + d - 1 - (2**p - 1) % d)//d
            return (m, p)
    print("Can't find p, something is wrong.")
    sys.exit(1)


def do_div(n):
    return ((n + add_ind) * magic) >> shift_amt


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate magic numbers for emulating 64-bit division with multiplication by reciprocal using algorithm from Hacker's Delight, chapter 10.")
    parser.add_argument("--divisor", type=int, required=True,
                        help="Devisor to calculate magic numbers for")
    args = parser.parse_args()

    magic, shift_amt = magicgu(2**32 - 1, args.divisor)
    print("magic number is: %d, shift amount is %d" % (magic, shift_amt))
    add_ind = 0

    print("Doing sanity check")
    # sanity check
    for i in xrange(2**32-1):
        q1, q2 = (i / args.divisor, do_div(i))
        if int(q1) != q2:
            print("Combination failed %d %d %d" % i, q1, q2)
            sys.exit(-1)

    print("Success! Use (n * %d) >> %d to calculate n / %d" % (magic, shift_amt, args.divisor))
    print("magic number is: %d, shift amount is %d" % (magic, shift_amt))
