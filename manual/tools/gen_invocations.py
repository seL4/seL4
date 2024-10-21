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
import logging
import itertools
from pathlib import Path
from libsel4_tools import syscall_stub_gen
from lxml import etree

# Word size is required by the syscall_stub_gen library, but won't affect the output
WORD_SIZE = 32
FN_DECL_PREFIX = "static inline"
DEFAULT_RETURN_TYPE = "int"


def init_all_types(args):
    """
    Return an array of all c types involved in the seL4 interface
    """

    # cheriTODO: document CHERI support
    data_types = syscall_stub_gen.init_data_types(WORD_SIZE, None)
    arch_types = list(itertools.chain(*syscall_stub_gen.init_arch_types(WORD_SIZE, args, False).values()))

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


def eval_condition(condition, values):
    """
    Evaluates a method condition string to True or False. Empty values and
    expressions evaluate to False. Raises an exception if the parse failed.
    Values dict must map to strings "True", "False", or to None.

    Accepted grammar:
    condition ::= term ("&&" term)*
    term ::= "defined" "(" identifier ")" | "!" term | "(" condition ")"
    """

    pos = 0

    def accept(string):
        nonlocal pos
        # if we overflow len(condition), the equality will fail
        if condition[pos:pos+len(string)] == string:
            pos += len(string)
            return True
        else:
            return False

    def skip_whitespace():
        nonlocal pos
        while pos < len(condition) and condition[pos].isspace():
            pos += 1

    def parse_defined():
        nonlocal pos
        skip_whitespace()
        if not accept("defined("):
            return None
        skip_whitespace()
        if not condition[pos].isalpha():
            return None
        start = pos
        while pos < len(condition) and (condition[pos].isalnum() or condition[pos] == "_"):
            pos += 1
        end = pos
        if not accept(")"):
            return None
        return values.get(condition[start:end], "False")

    def parse_not():
        nonlocal pos
        skip_whitespace()
        if not accept("!"):
            return None
        term = parse_term()
        if term == "True":
            return "False"
        elif term == "False":
            return "True"
        else:
            return None

    def parse_paren():
        nonlocal pos
        skip_whitespace()
        if not accept("("):
            return None
        result = parse_condition()
        if not accept(")"):
            return None
        return result

    def parse_term():
        return parse_defined() or parse_not() or parse_paren()

    def parse_condition():
        nonlocal pos
        skip_whitespace()
        term = parse_term()
        if not term:
            return None
        skip_whitespace()
        while accept("&&"):
            next_term = parse_term()
            if not next_term:
                return None
            skip_whitespace()
            if next_term == "False":
                term = "False"
        return term

    if condition == '':
        return False
    cond = parse_condition()
    skip_whitespace()
    if not cond or pos != len(condition):
        raise Exception(f"Failed to parse condition '{condition}'")
    if cond == "True":
        return True
    if cond == "False":
        return False
    raise Exception(f"Unexpected value {cond} for condition '{condition}'")


def is_mcs(method_condition):
    """
    Returns whether the condition evaluates to true when CONFIG_KERNEL_MCS is set.
    """
    return eval_condition(method_condition, {"CONFIG_KERNEL_MCS": "True"})


def gen_invocations(input_files, output_dir, args):
    """
    Given a collection of input xml files describing seL4 interfaces,
    generates a c header file containing doxygen-commented function
    prototypes.
    """

    types = init_all_types(args)

    group_file_name = output_dir / "group_defs.h"
    if os.path.exists(group_file_name):
        os.remove(group_file_name)

    for input_file in input_files:
        methods, _, api = syscall_stub_gen.parse_xml_file(input_file, types)

        # dict mapping group id to list of prototypes
        prototypes = {}

        # dict mapping group id to group name
        groups = {}

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
            group_id_mcs = group_id + "_mcs"
            group_name = interface_name

            if group_id in groups:
                if group_name != groups[group_id]:
                    raise Exception(f"Group {group_id} has inconsistent names: {group_name} "
                                    f"!= {groups[group_id]}")
            else:
                groups[group_id] = group_name
                groups[group_id_mcs] = group_name + " (MCS)"

            for (interface_name, method_name, method_id, inputs, outputs, condition,
                 comment) in methods:
                g_id = group_id_mcs if is_mcs(condition) else group_id
                prototype = "/**\n * @addtogroup %s\n * @{\n */\n\n" % g_id
                prototype += generate_prototype(interface_name, method_name, method_id, inputs,
                                                outputs, comment)
                prototype += "\n/** @} */\n"
                if g_id not in prototypes:
                    prototypes[g_id] = [prototype]
                else:
                    prototypes[g_id].append(prototype)

        with open(group_file_name, "a") as groups_file:
            groups_file.write("/**\n * @defgroup %s %s\n * @{\n */\n\n" % (api.name, api.name))
            for group_id, group_name in sorted(groups.items()):
                if group_id in prototypes:
                    groups_file.write("/**\n * @defgroup %s %s\n */\n\n" % (group_id, group_name))
            groups_file.write("/** @} */\n\n")

        for group_id, group_prototypes in prototypes.items():
            group_prototypes.sort()
            output_name = output_dir / f"{group_id}.h"
            with open(output_name, "w") as output_file:
                for prototype in group_prototypes:
                    output_file.write(prototype)
                    output_file.write("\n\n")


def process_args():
    usage_str = "%(prog)s [OPTIONS] [FILES]"

    parser = argparse.ArgumentParser(description='Generates doxygen-annotated header '
                                                 'containing object invocation prototypes',
                                     usage=usage_str)

    parser.add_argument("--x86-vtx-64-bit-guests", dest="x86_vtx_64bit", action="store_true", default=False,
                        help="Whether the vtx VCPU objects need to be large enough for 64-bit guests.")
    parser.add_argument("-o", "--output", dest="output", default="stage",
                        type=str,
                        help="Output directory to write stubs to. (default: %(default)s).")
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
            xml.xinclude()
            if not dtd.validate(xml):
                logging.error("Failed to validate %s against %s" % (f, args.dtd))
                logging.error(dtd.error_log)
                return -1

    if not os.path.exists(args.output):
        os.makedirs(args.output)

    if not os.path.isdir(args.output):
        logging.error(f"{args.output} is not a directory")
        return -1

    gen_invocations(args.files, Path(args.output), args)


if __name__ == "__main__":
    sys.exit(main())
