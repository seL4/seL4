#
# Copyright 2023, UNSW
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' Generate a JSON file with memory region info from the device tree '''

import argparse
import json
from typing import List
import hardware
from hardware.config import Config
from hardware.fdt import FdtParser
from hardware.utils.rule import HardwareYaml


def make_json_list_of_regions(regions) -> List:
    return [
        {
            'start': r.base,
            'end':   r.base + r.size
        }
        for r in regions if r.size > 0
    ]


def create_json_file(dev_mem, phys_mem, output_stream):
    json_obj = {
        'devices': make_json_list_of_regions(dev_mem),
        'memory':  make_json_list_of_regions(phys_mem)
    }

    with output_stream:
        json.dump(json_obj, output_stream)


def get_kernel_devices(tree: FdtParser, hw_yaml: HardwareYaml):
    kernel_devices = tree.get_kernel_devices()

    groups = []
    for dev in kernel_devices:
        rule = hw_yaml.get_rule(dev)
        groups += rule.get_regions(dev)

    return groups


def run(tree: FdtParser, hw_yaml: HardwareYaml, config: Config,
        args: argparse.Namespace):
    if not args.json_out:
        raise ValueError('you need to provide a json-out to use the JSON output method')

    phys_mem, reserved, _ = hardware.utils.memory.get_physical_memory(tree, config)
    kernel_devs = get_kernel_devices(tree, hw_yaml)
    dev_mem = hardware.utils.memory.get_addrspace_exclude(
        list(reserved) + phys_mem + kernel_devs, config)

    create_json_file(dev_mem, phys_mem, args.json_out)


def add_args(parser):
    parser.add_argument('--json-out', help='output file for memory represented in JSON',
                        type=argparse.FileType('w'))
