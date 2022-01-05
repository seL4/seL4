#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' generate a yaml file with memory region info from the device tree '''

import argparse
import yaml
from typing import List, Dict
import hardware
from hardware.config import Config
from hardware.fdt import FdtParser
from hardware.memory import Region
from hardware.utils.rule import HardwareYaml


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


def get_kernel_devices(tree: FdtParser, hw_yaml: HardwareYaml):
    kernel_devices = tree.get_kernel_devices()

    groups = []
    for dev in kernel_devices:
        rule = hw_yaml.get_rule(dev)
        groups += rule.get_regions(dev)

    return groups


def run(tree: FdtParser, hw_yaml: HardwareYaml, config: Config,
        args: argparse.Namespace):
    if not args.yaml_out:
        raise ValueError('you need to provide a yaml-out to use the yaml output method')

    phys_mem, reserved, _ = hardware.utils.memory.get_physical_memory(tree, config)
    kernel_devs = get_kernel_devices(tree, hw_yaml)
    dev_mem = hardware.utils.memory.get_addrspace_exclude(
        list(reserved) + phys_mem + kernel_devs, config)

    create_yaml_file(
        {
            'devices':  dev_mem,
            'memory':   phys_mem
        },
        args.yaml_out)


def add_args(parser):
    parser.add_argument('--yaml-out', help='output file for memory yaml',
                        type=argparse.FileType('w'))
