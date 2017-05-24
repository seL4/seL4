#!/usr/bin/env python
#
# Copyright 2016, Data 61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(D61_BSD)
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
from functools import reduce
from libsel4_tools import syscall_stub_gen
from lxml import etree

# Word size is required by the syscall_stub_gen library, but won't affect the output
WORD_SIZE = 32
FN_DECL_PREFIX = "static inline"
DEFAULT_RETURN_TYPE = "int"

def init_all_types():
    data_types = syscall_stub_gen.init_data_types(WORD_SIZE)
    arch_types = list(itertools.chain(*syscall_stub_gen.init_arch_types(WORD_SIZE).values()))

    return data_types + arch_types

def generate_prototype(interface_name, method_name, method_id, inputs, outputs, comment):
    prefix = FN_DECL_PREFIX
    if syscall_stub_gen.generate_result_struct(interface_name, method_name, outputs):
        return_type = "%s_%s_t" % (interface_name, method_name)
    else:
        return_type = DEFAULT_RETURN_TYPE

    param_list = syscall_stub_gen.generate_param_list(inputs, outputs)
    name = "%s_%s" % (interface_name, method_name)

    return "%s\n%s %s %s(%s);" % (comment, prefix, return_type, name, param_list)

def gen_invocations(input_files, output_file):
    types = init_all_types()


    for input_file in input_files:
        methods, _, api = syscall_stub_gen.parse_xml_file(input_file, types)
        prototypes = []
        for (interface_name, method_name, method_id, inputs, outputs, _, comment) in methods:
            prototype = generate_prototype(interface_name, method_name, method_id, inputs, outputs, comment)
            prototypes.append(prototype)

        prototypes.sort()

        output_file.write("/**\n * @defgroup %s %s\n * @{\n */\n\n" % (api.name, api.name))

        for prototype in prototypes:
            output_file.write(prototype)
            output_file.write("\n\n");

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

def gen_header(output_file):
    output_file.write("""
/**
 * @defgroup ObjectInvocations Object Invocations
 * @{
 */
""")

def gen_footer(output_file):
    output_file.write("""
/** @} */
""")

def main():
    parser = process_args()
    args = parser.parse_args()

    if args.dtd is not None:
        dtd = etree.DTD(args.dtd)
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
