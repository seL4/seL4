#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' generate a yaml file with memory region info from the device tree '''

import argparse
import yaml
from typing import List
from hardware import config, fdt
from hardware.utils import memory, rule
from hardware.memory import Region


def make_yaml_list_of_regions(regions) -> List:
    return [
        {
            'start': r.base,
            'end':   r.base + r.size
        }
        for r in regions if r.size > 0
    ]


def create_yaml_file(dev_mem, phys_mem, outputStream):

    yaml.add_representer(
        int,
        lambda dumper, data: yaml.ScalarNode('tag:yaml.org,2002:int', hex(data)))

    yaml_obj = {
        'devices': make_yaml_list_of_regions(dev_mem),
        'memory':  make_yaml_list_of_regions(phys_mem)
    }

    with outputStream:
        yaml.dump(yaml_obj, outputStream)


def get_kernel_devices(tree: fdt.FdtParser, hw_yaml: rule.HardwareYaml):
    kernel_devices = tree.get_kernel_devices()

    groups = []
    for dev in kernel_devices:
        rule = hw_yaml.get_rule(dev)
        groups += rule.get_regions(dev)

    return groups


def run(tree: fdt.FdtParser, hw_yaml: rule.HardwareYaml, config: config.Config,
        args: argparse.Namespace):
    if not args.yaml_out:
        raise ValueError('you need to provide a yaml-out to use the yaml output method')

    phys_mem, reserved, _ = memory.get_physical_memory(tree, config)
    kernel_devs = get_kernel_devices(tree, hw_yaml)
    dev_mem = memory.get_addrspace_exclude(list(reserved) + phys_mem + kernel_devs, config)

    create_yaml_file(dev_mem, phys_mem, args.yaml_out)


def add_args(parser):
    parser.add_argument('--yaml-out', help='output file for memory yaml',
                        type=argparse.FileType('w'))
