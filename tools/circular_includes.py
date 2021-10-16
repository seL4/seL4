#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
Script for reporting circular #includes in pre-processed sel4 source.
Exits with a status of 0 if no circular dependencies are found, otherwise
prints circular dependency and exits with a status of -1.
"""

import sys
import re
import argparse


def main(parse_args):
    """
    Reads pre-processed sel4 source from standard input.
    If a circular dependency is found, the chain of includes
    resulting in the loop is printed out.
    """

    ignore_re = None
    ignore_args = parse_args.ignore
    if ignore_args and len(ignore_args):
        ignore_args = [re.escape(ignore) for ignore in ignore_args]
        ignore_re_string = '(' + '|'.join(ignore_args) + ')'
        ignore_re = re.compile(r'^# [01] ".*' + ignore_re_string + '"')

    header_re = re.compile(r'^# (\d+) "(.*\..)"')

    file_stack = []

    for line in sys.stdin:

        if ignore_re and ignore_re.match(line):
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
            while (len(file_stack) > 0) and (file_stack[-1] != header):
                file_stack.pop()

    return 0


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('--ignore', nargs='+',
                        help="Files to ignore when parsing the sel4 source")
    args = parser.parse_args()

    sys.exit(main(args))
