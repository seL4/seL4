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
from functools import reduce
from libsel4_tools import syscall_stub_gen

# Word size is required by the syscall_stub_gen library, but won't affect the output
WORD_SIZE = 32
ARCH_NAMES = ["aarch32", "arm_hyp", "ia32", "x86_64"]
FN_DECL_PREFIX = "static inline"
DEFAULT_RETURN_TYPE = "int"

def init_all_types():
    data_type_arr = syscall_stub_gen.init_data_types(WORD_SIZE)
    arch_type_dict = syscall_stub_gen.init_arch_types(WORD_SIZE)
    arch_type_arr = reduce(operator.add, (arch_type_dict[a] for a in ARCH_NAMES), [])

    return data_type_arr + arch_type_arr

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
    parser.add_argument("files", metavar="FILES", nargs="+", type=argparse.FileType('r'),
                        help="Input XML files.")
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

    if not os.path.exists(os.path.dirname(args.output)):
        os.makedirs(os.path.dirname(args.output))

    with open(args.output, "w") as output:
        gen_invocations(args.files, output)

if __name__ == "__main__":
    sys.exit(main())
