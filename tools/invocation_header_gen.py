#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause or GPL-2.0-only
#

# seL4 Invocation ID Generator
# ============================

from __future__ import print_function
from jinja2 import Environment, BaseLoader
import argparse
import sys
import xml.dom.minidom
import pkg_resources
# We require jinja2 to be at least version 2.10 as we use the 'namespace' feature from
# that version
pkg_resources.require("jinja2>=2.10")


COMMON_HEADER = """
/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
{%- if libsel4 -%}
 * SPDX-License-Identifier: BSD-2-Clause
{%- else -%}
 * SPDX-License-Identifier: GPL-2.0-only
{%- endif %}
 */

/* This header was generated by kernel/tools/invocation_header_gen.py.
 *
 * To add an invocation call number, edit libsel4/include/interfaces/sel4.xml.
 *
 */"""

INVOCATION_TEMPLATE = COMMON_HEADER + """
#ifndef __{{header_title}}_INVOCATION_H
#define __{{header_title}}_INVOCATION_H

enum invocation_label {
    InvalidInvocation,
    {%- for label, condition in invocations %}
    {%- if condition %}
#if {{condition}}
    {%- endif %}
    {{label}},
    {%- if condition %}
#endif
    {%- endif %}
    {%- endfor %}
    nInvocationLabels
};

{%- if libsel4 %}
#include <sel4/sel4_arch/invocation.h>
#include <sel4/arch/invocation.h>
{%- endif %}

#endif /* __{{header_title}}_INVOCATION_H */

"""

SEL4_ARCH_INVOCATION_TEMPLATE = COMMON_HEADER + """
#ifndef __{{header_title}}_SEL4_ARCH_INVOCATION_H
#define __{{header_title}}_SEL4_ARCH_INVOCATION_H

{%- if not libsel4 %}
#include <api/invocation.h>
{%- endif %}

{%- set ns = namespace(first=True) %}
enum sel4_arch_invocation_label {
    {%- for label, condition in invocations %}
        {%- if condition %}
            {%- if ns.first %}
#error "First sel4_arch invocation label cannot be conditional"
            {%- endif %}
#if {{condition}}
        {%- endif %}
        {%- if ns.first %}
            {%- set ns.first = False %}
    {{label}} = nInvocationLabels,
        {%- else %}
    {{label}},
        {%- endif %}
        {%- if condition %}
#endif
        {%- endif %}
    {%- endfor %}
    {%- if ns.first %}
    nSeL4ArchInvocationLabels = nInvocationLabels
    {%- else %}
    nSeL4ArchInvocationLabels
    {%- endif %}
};

#endif /* __{{header_title}}_SEL4_ARCH_INVOCATION_H */

"""

ARCH_INVOCATION_TEMPLATE = COMMON_HEADER + """
#ifndef __{{header_title}}_ARCH_INVOCATION_H
#define __{{header_title}}_ARCH_INVOCATION_H

{%- if not libsel4 %}
#include <arch/api/sel4_invocation.h>
{%- endif %}

{%- set ns = namespace(first=1) %}
enum arch_invocation_label {
    {%- for label, condition in invocations %}
    {%- if condition %}
    {%- if ns.first  %}
#error "First arch invocation label cannot be conditional"
    {%- endif %}
#if {{condition}}
    {%- endif %}
    {%- if ns.first %}
    {{label}} = nSeL4ArchInvocationLabels,
    {%- set ns.first = False %}
    {%- else %}
    {{label}},
    {%- endif %}
    {%- if condition %}
#endif
    {%- endif %}
    {%- endfor %}
    {%- if ns.first %}
    nArchInvocationLabels = nSeL4ArchInvocationLabels
    {%- else %}
    nArchInvocationLabels
    {%- endif %}
};

#endif /* __{{header_title}}_ARCH_INVOCATION_H */

"""


def parse_args():
    parser = argparse.ArgumentParser(description='Generate seL4 invocation API \
        constants and header files')
    parser.add_argument('--xml', type=argparse.FileType('r'),
                        help='Name of xml file with invocation definitions', required=True)
    parser.add_argument('--dest', type=argparse.FileType('w'),
                        help='Name of file to create', required=True)
    parser.add_argument('--libsel4', action='store_true',
                        help='Is this being generated for libsel4?')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--arch', action='store_true',
                       help='Is this being generated for the arch layer?')
    group.add_argument('--sel4_arch', action='store_true',
                       help='Is this being generated for the seL4 arch layer?')

    return parser.parse_args()


def parse_xml(xml_file):
    try:
        doc = xml.dom.minidom.parse(xml_file)
    except:
        print("Error: invalid xml file", file=sys.stderr)
        sys.exit(-1)

    invocation_labels = []
    for method in doc.getElementsByTagName("method"):
        invocation_labels.append((str(method.getAttribute("id")),
                                  str(method.getAttribute("condition"))))

    return invocation_labels


def generate(args, invocations):

    header_title = "API"
    if args.libsel4:
        header_title = "LIBSEL4"

    if args.arch:
        template = Environment(loader=BaseLoader).from_string(ARCH_INVOCATION_TEMPLATE)

    elif args.sel4_arch:
        template = Environment(loader=BaseLoader).from_string(SEL4_ARCH_INVOCATION_TEMPLATE)
    else:
        template = Environment(loader=BaseLoader).from_string(INVOCATION_TEMPLATE)

    data = template.render({'header_title': header_title, 'libsel4': args.libsel4,
                            'invocations': invocations, 'num_invocations': len(invocations)})
    args.dest.write(data)

    args.dest.close()


if __name__ == "__main__":
    args = parse_args()

    invocations = parse_xml(args.xml)
    args.xml.close()

    generate(args, invocations)
