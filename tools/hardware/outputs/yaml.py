#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' generate a yaml file with memory region info from the device tree '''

from __future__ import annotations
import argparse
import yaml
import hardware
from hardware.fdt import FdtParser
from hardware.memory import Region
from hardware.utils.rule import HardwareYaml, KernelInterrupt

# "annotations" exists in __future__ since 3.7.0b1, but even in 3.10 the
# decision to make it mandatory has been postponed.
import sys
assert sys.version_info >= (3, 7)


def make_yaml_list_of_regions(regions: List[Region]) -> List:
    return [
        {
            'start': r.base,
            'end':   r.base + r.size
        }
        for r in regions if r.size > 0
    ]


def create_yaml_file(regions_dict: Dict[str, List[Region]], outputStream):

    yaml.add_representer(
        int,
        lambda dumper, data: yaml.ScalarNode('tag:yaml.org,2002:int', hex(data)))

    with outputStream:
        yaml.dump(
            {
                key: make_yaml_list_of_regions(val)
                for key, val in regions_dict.items()
            },
            outputStream)


def run(tree: FdtParser, hw_yaml: HardwareYaml, args: argparse.Namespace):

    if not args.yaml_out:
        raise ValueError('you need to provide a yaml-out to use the yaml output method')

    # Get the physical memory and device regions.
    phys_mem, res_mem, dev_mem, _ = \
        hardware.utils.memory.get_phys_mem_regions(tree, hw_yaml)

    create_yaml_file(
        {
            'devices':  dev_mem,
            'memory':   phys_mem,
            'reserved': res_mem
        },
        args.yaml_out)


def add_args(parser: argparse.ArgumentParser):
    parser.add_argument('--yaml-out', help='output file for memory yaml',
                        type=argparse.FileType('w'))
