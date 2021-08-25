#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' generate a yaml file with memory region info from the device tree '''

import argparse
import yaml

from hardware import config, fdt
from hardware.utils import memory, rule


def get_kernel_devices(tree: fdt.FdtParser, rules: rule.HardwareYaml):
    kernel_devices = tree.get_kernel_devices()

    groups = []
    for dev in kernel_devices:
        rule = rules.get_rule(dev)
        groups += rule.get_regions(dev)

    return groups


def run(tree: fdt.FdtParser, hardware: rule.HardwareYaml, config: config.Config,
        args: argparse.Namespace):
    if not args.yaml_out:
        raise ValueError('you need to provide a yaml-out to use the yaml output method')
    phys_mem, reserved, _ = memory.get_physical_memory(tree, config)
    kernel_devs = get_kernel_devices(tree, hardware)
    dev_mem = memory.get_addrspace_exclude(list(reserved) + phys_mem + kernel_devs, config)

    yaml.add_representer(int, lambda dumper, data: yaml.ScalarNode(
        'tag:yaml.org,2002:int', hex(data)))
    yaml_obj = {
        'devices': [{'start': r.base, 'end': r.base + r.size} for r in dev_mem if r.size > 0],
        'memory': [{'start': r.base, 'end': r.base + r.size} for r in phys_mem if r.size > 0]
    }

    yaml.dump(yaml_obj, args.yaml_out)
    args.yaml_out.close()


def add_args(parser):
    parser.add_argument('--yaml-out', help='output file for memory yaml',
                        type=argparse.FileType('w'))
