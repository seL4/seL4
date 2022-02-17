#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause or GPL-2.0-only
#

# seL4 System Call ID Generator
# ==============================

from __future__ import print_function
from jinja2 import Environment, BaseLoader
import argparse
import itertools
import re
import sys
import xml.dom.minidom
import pkg_resources
# We require jinja2 to be at least version 2.10,
# In the past we used the 'namespace' feature from that version.
# other versions of jinja, particularly `minijinja`, don't support namespaces.
# However in case `namespace` is needed in the future require a
# version which supports it.
pkg_resources.require("jinja2>=2.10")


COMMON_HEADER = """
/* This header was generated by kernel/tools/syscall_header_gen.py.
 *
 * To add a system call number, edit kernel/libsel4/include/api/syscall.xml
 *
 */"""

KERNEL_HEADER_TEMPLATE = """/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

""" + COMMON_HEADER + """
#pragma once

#ifdef __ASSEMBLER__

/* System Calls */
{%- for condition, list in assembler  -%}
    {%- for syscall, syscall_number in list %}
#define SYSCALL_{{upper(syscall)}} ({{syscall_number}})
    {%- endfor  %}
{%- endfor  %}

#endif /* __ASSEMBLER__ */

#define SYSCALL_MAX (-1)
#define SYSCALL_MIN ({{syscall_min}})

#ifndef __ASSEMBLER__

enum syscall {
{%- for condition, list in enum %}
   {%- if condition | length > 0 %}
#if {{condition}}
   {%- endif %}
   {%- for syscall, syscall_number in list %}
    Sys{{syscall}} = {{syscall_number}},
   {%- endfor %}
   {%- if condition | length > 0 %}
#endif /* {{condition}} */
   {%- endif %}
{%- endfor %}
};
typedef word_t syscall_t;

/* System call names */
#ifdef CONFIG_DEBUG_BUILD
static char *syscall_names[] UNUSED = {
{%- for condition, list in assembler  -%}
    {%- for syscall, syscall_number in list %}
         [{{syscall_number * -1}}] = "{{syscall}}",
    {%- endfor %}
{%- endfor %}
};
#endif /* CONFIG_DEBUG_BUILD */
#endif /* !__ASSEMBLER__ */

"""

LIBSEL4_HEADER_TEMPLATE = """/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

""" + COMMON_HEADER + """
#pragma once

#include <autoconf.h>

typedef enum {
{%- for condition, list in enum %}
   {%- if condition | length > 0 %}
#if {{condition}}
   {%- endif %}
   {%- for syscall, syscall_number in list %}
       seL4_Sys{{syscall}} = {{syscall_number}},
   {%- endfor %}
   {%- if condition | length > 0 %}
#endif /* {{condition}} */
   {%- endif %}
{%- endfor %}
    SEL4_FORCE_LONG_ENUM(seL4_Syscall_ID)
} seL4_Syscall_ID;

"""


def parse_args():
    parser = argparse.ArgumentParser(description="""Generate seL4 syscall API constants
                                                    and associated header files""")
    parser.add_argument('--xml', type=argparse.FileType('r'),
                        help='Name of xml file with syscall name definitions', required=True)
    parser.add_argument('--kernel_header', type=argparse.FileType('w'),
                        help='Name of file to generate for kernel')
    parser.add_argument('--libsel4_header', type=argparse.FileType('w'),
                        help='Name of file to generate for libsel4')
    parser.add_argument('--mcs', action='store_true',
                        help='Generate MCS api')

    result = parser.parse_args()

    if result.kernel_header is None and result.libsel4_header is None:
        print("Error: must provide either kernel_header or libsel4_header",
              file=sys.stderr)
        parser.print_help()
        exit(-1)

    return result


def parse_syscall_list(element):
    syscalls = []
    for config in element.getElementsByTagName("config"):
        config_condition = config.getAttribute("condition")
        config_syscalls = []
        for syscall in config.getElementsByTagName("syscall"):
            name = str(syscall.getAttribute("name"))
            config_syscalls.append(name)
        syscalls.append((config_condition, config_syscalls))

    # sanity check
    assert len(syscalls) != 0

    return syscalls


def parse_xml(xml_file, mcs):
    # first check if the file is valid xml
    try:
        doc = xml.dom.minidom.parse(xml_file)
    except:
        print("Error: invalid xml file.", file=sys.stderr)
        sys.exit(-1)

    tag = "api-mcs" if mcs else "api-master"
    api = doc.getElementsByTagName(tag)
    if len(api) != 1:
        print("Error: malformed xml. Only one api element allowed",
              file=sys.stderr)
        sys.exit(-1)

    configs = api[0].getElementsByTagName("config")
    if len(configs) != 1:
        print("Error: api element only supports 1 config element",
              file=sys.stderr)
        sys.exit(-1)

    if len(configs[0].getAttribute("name")) != 0:
        print("Error: api element config only supports an empty name",
              file=sys.stderr)
        sys.exit(-1)

    # debug elements are optional
    debug = doc.getElementsByTagName("debug")
    if len(debug) != 1:
        debug_element = None
    else:
        debug_element = debug[0]

    api_elements = parse_syscall_list(api[0])
    debug = parse_syscall_list(debug_element)

    return (api_elements, debug)


def convert_to_assembler_format(s):
    words = re.findall('[A-Z][A-Z]?[^A-Z]*', s)
    return '_'.join(words).upper()


def map_syscalls_neg(syscalls):
    # This function will map a list of (condition, syscall_list) tuples
    # From: [(cond1, ["SyscallOne", "SysCallTwo"]),
    #        (cond2, ["SyscallThree", ...])]
    # Into: [(cond1, [("SyscallOne", -1), ("SyscallTwo", -2)]),
    #        (cond2, [(SyscallThree", -3), ...])]
    r = itertools.count(start=-1, step=-1)
    return [(cond, [(s, next(r)) for s in lst]) for (cond, lst) in syscalls]


def generate_kernel_file(kernel_header, api, debug):
    template = Environment(loader=BaseLoader, trim_blocks=False,
                           lstrip_blocks=False).from_string(KERNEL_HEADER_TEMPLATE)
    data = template.render({'assembler': map_syscalls_neg(api),
                            'enum': map_syscalls_neg(api + debug),
                            'upper': convert_to_assembler_format,
                            'syscall_min': -sum([len(lst) for (cond, lst) in api])})
    kernel_header.write(data)


def generate_libsel4_file(libsel4_header, syscalls):
    template = Environment(loader=BaseLoader, trim_blocks=False,
                           lstrip_blocks=False).from_string(LIBSEL4_HEADER_TEMPLATE)
    data = template.render({'enum': map_syscalls_neg(syscalls)})
    libsel4_header.write(data)


if __name__ == "__main__":
    args = parse_args()

    (api, debug) = parse_xml(args.xml, args.mcs)
    args.xml.close()

    if (args.kernel_header is not None):
        generate_kernel_file(args.kernel_header, api, debug)
        args.kernel_header.close()

    if (args.libsel4_header is not None):
        generate_libsel4_file(args.libsel4_header, api + debug)
        args.libsel4_header.close()
