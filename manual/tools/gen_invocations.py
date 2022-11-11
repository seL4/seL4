#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
Script to generate a c header file containing function prototypes and
doxygen comments from a given interface defined in an xml file.
"""

import os
import sys
import argparse
import operator
import logging
import itertools
from libsel4_tools import syscall_stub_gen
from lxml import etree

# Word size is required by the syscall_stub_gen library, but won't affect the output
WORD_SIZE = 32
FN_DECL_PREFIX = "static inline"
DEFAULT_RETURN_TYPE = "int"


def init_all_types():
    """
    Return an array of all c types involved in the sel4 interface
    """

    data_types = syscall_stub_gen.init_data_types(WORD_SIZE)
    arch_types = list(itertools.chain(*syscall_stub_gen.init_arch_types(WORD_SIZE).values()))

    return data_types + arch_types


def generate_prototype(interface_name, method_name, method_id, inputs, outputs, comment):
    """
    Returns a string containing a commented function prototype based on its arguments
    """

    prefix = FN_DECL_PREFIX
    if syscall_stub_gen.generate_result_struct(interface_name, method_name, outputs):
        return_type = "%s_%s_t" % (interface_name, method_name)
    else:
        return_type = DEFAULT_RETURN_TYPE

    param_list = syscall_stub_gen.generate_param_list(inputs, outputs)
    name = "%s_%s" % (interface_name, method_name)

    return "%s\n%s %s %s(%s);" % (comment, prefix, return_type, name, param_list)


def gen_invocations(input_files, output_file):
    """
    Given a collection of input xml files describing sel4 interfaces,
    generates a c header file containing doxygen-commented function
    prototypes.
    """

    types = init_all_types()

    for input_file in input_files:
        methods, _, api = syscall_stub_gen.parse_xml_file(input_file, types)
        prototypes = []

        # figure out the prefix to use for an interface group id. This makes groups per arch,
        # sel4_arch unique even through the interface name is the same.
        prefix = None
        if "arch_include" in input_file:
            # extract the 2nd last path member
            (path, tail) = os.path.split(os.path.dirname(input_file))
            assert tail == "interfaces"
            (path, prefix) = os.path.split(path)

        # group the methods in each interface
        for interface_name, methods in itertools.groupby(methods, lambda x: x[0]):
            group_id = interface_name if prefix is None else prefix + '_' + interface_name
            group_name = interface_name
            output_file.write("/**\n * @defgroup %s %s\n * @{\n */\n\n" % (group_id, group_name))
            output_file.write("/** @} */\n")
            for (interface_name, method_name, method_id, inputs, outputs, _, comment) in methods:
                prototype = "/**\n * @addtogroup %s %s\n * @{\n */\n\n" % (group_id, group_name)
                prototype += generate_prototype(interface_name,
                                                method_name, method_id, inputs, outputs, comment)
                prototype += "/** @} */\n"
                prototypes.append(prototype)

        prototypes.sort()

        output_file.write("/**\n * @defgroup %s %s\n * @{\n */\n\n" % (api.name, api.name))

        for prototype in prototypes:
            output_file.write(prototype)
            output_file.write("\n\n")

        output_file.write("/** @} */\n")


def process_args():
    usage_str = "%(prog)s [OPTIONS] [FILES]"

    parser = argparse.ArgumentParser(description='Generates doxygen-annotated header '
                                                 'containing object invocation prototypes',
                                     usage=usage_str)

    parser.add_argument("-o", "--output", dest="output", default="/dev/stdout",
                        type=str,
                        help="Output file to write stub to. (default: %(default)s).")
    parser.add_argument("files", metavar="FILES", nargs="+", type=str,
                        help="Input XML files.")

    parser.add_argument("-d", "--dtd", nargs="?", type=str,
                        help="DTD xml schema to validate input files against")

    return parser


def main():
    parser = process_args()
    args = parser.parse_args()

    if args.dtd is not None:
        dtd = etree.XMLSchema(etree.parse(args.dtd))
        for f in args.files:
            xml = etree.parse(f)
            if not dtd.validate(xml):
                logging.error("Failed to validate %s against %s" % (f, args.dtd))
                logging.error(dtd.error_log)
                return -1

    if not os.path.exists(os.path.dirname(args.output)):
        os.makedirs(os.path.dirname(args.output))

    with open(args.output, "w") as output:
        gen_invocations(args.files, output)


if __name__ == "__main__":
    sys.exit(main())
