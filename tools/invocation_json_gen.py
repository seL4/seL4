#!/usr/bin/env python3
#
# Copyright 2024, UNSW Sydney
#
# SPDX-License-Identifier: BSD-2-Clause or GPL-2.0-only
#

import argparse
import json
import xml.dom.minidom
from condition import condition_to_bool


def parse_args():
    parser = argparse.ArgumentParser(description='Generate JSON file containing list of seL4 \
                                    invocations.')
    parser.add_argument('--gen_config', type=argparse.FileType('r'),
                        help="Location of gen_config JSON file", required=True)
    parser.add_argument('--invocations', type=argparse.FileType('r'),
                        help='Location of XML file with invocation definitions', required=True)
    parser.add_argument('--arch_invocations', type=argparse.FileType('r'),
                        help='Location of XML file with arch invocation definitions', required=True)
    parser.add_argument('--sel4_arch_invocations', type=argparse.FileType('r'),
                        help='Location of XML file with seL4 arch invocation definitions', required=True)
    parser.add_argument('--dest', type=argparse.FileType('w+'),
                        help='Location of JSON file to store invocations', required=True)

    return parser.parse_args()


def xml_to_json_invocations(xml, gen_config, counter, invocations_dict, ):
    for method in xml.getElementsByTagName("method"):
        label = str(method.getAttribute("id"))
        exists = condition_to_bool(method.getElementsByTagName("condition"), gen_config)
        if exists:
            invocations_dict[label] = counter
            counter += 1

    return counter


if __name__ == "__main__":
    args = parse_args()

    try:
        invocations = xml.dom.minidom.parse(args.invocations)
        arch_invocations = xml.dom.minidom.parse(args.arch_invocations)
        sel4_arch_invocations = xml.dom.minidom.parse(args.sel4_arch_invocations)
    except:
        print('Error: invalid XML file provided', file=sys.stderr)
        sys.exit(1)

    try:
        gen_config = json.load(args.gen_config)
    except:
        print('Error: invalid JSON file provided', file=sys.stderr)
        sys.exit(1)

    invocations_dict = {}
    counter = 1
    counter = xml_to_json_invocations(invocations, gen_config, counter, invocations_dict)
    counter = xml_to_json_invocations(sel4_arch_invocations, gen_config, counter, invocations_dict)
    counter = xml_to_json_invocations(arch_invocations, gen_config, counter, invocations_dict)

    json.dump(invocations_dict, args.dest)
