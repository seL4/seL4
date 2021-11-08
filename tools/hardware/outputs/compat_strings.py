#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' generate a text file with matched compatible strings from the device tree '''
import argparse
from hardware.config import Config
from hardware.fdt import FdtParser
from hardware.utils.rule import HardwareYaml


def run(tree: FdtParser, hw_yaml: HardwareYaml, config: Config,
        args: argparse.Namespace):
    if not args.compat_strings_out:
        raise ValueError('You need to specify a compat-strings-out to use compat strings output')
    chosen = tree.get_kernel_devices()

    compatibles = set()
    for dev in chosen:
        compatibles.add(hw_yaml.get_matched_compatible(dev))

    args.compat_strings_out.write(';'.join(sorted(compatibles)) + ';\n')
    args.compat_strings_out.close()


def add_args(parser):
    parser.add_argument('--compat-strings-out',
                        help='output file for compat strings list', type=argparse.FileType('w'))
