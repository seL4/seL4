#!/usr/bin/env python
#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#
"""
Script for reporting circular #includes in pre-processed sel4 source.
Exits with a status of 0 if no circular dependencies are found, otherwise
prints circular dependency and exits with a status of -1.
"""

import sys
import re

def main():
    """
    Reads pre-processed sel4 source from standard input.
    If a circular dependency is found, the chain of includes
    resulting in the loop is printed out.
    """

    kernel_all_re = re.compile(r'^# 1 ".*kernel_all\.c"')
    header_re = re.compile(r'^# (\d+) "(.*\..)"')

    file_stack = []

    for line in sys.stdin:

        if kernel_all_re.match(line):
            continue

        match = header_re.match(line)

        if match is None:
            continue

        depth = int(match.group(1))
        header = match.group(2)

        if depth == 1:
            # found a new header
            if header in file_stack:
                print("Circular includes found:")
                print("\n".join(file_stack))
                print(header)
                return -1
            else:
                file_stack.append(header)
        else:
            # popped back up to an earlier header
            while file_stack[-1] != header:
                file_stack.pop()

    return 0

if __name__ == "__main__":

    if len(sys.argv) != 1:
        print("Usage: %s < path/to/kernel_all.c_pp" % sys.argv[0])
        print(__doc__)
        sys.exit(-1)

    sys.exit(main())
