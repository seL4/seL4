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

''' generate a text file with matched compatible strings from the device tree '''
import argparse

from hardware import config, fdt
from hardware.utils import rule


def run(tree: fdt.FdtParser, hardware: rule.HardwareYaml, config: config.Config,
        args: argparse.Namespace):
    if not args.compat_strings_out:
        raise ValueError('You need to specify a compat-strings-out to use compat strings output')
    chosen = tree.get_kernel_devices()

    compatibles = set()
    for dev in chosen:
        compatibles.add(hardware.get_matched_compatible(dev))

    args.compat_strings_out.write(';'.join(sorted(compatibles)) + ';\n')
    args.compat_strings_out.close()


def add_args(parser):
    parser.add_argument('--compat-strings-out',
                        help='output file for compat strings list', type=argparse.FileType('w'))
