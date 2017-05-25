#!/usr/bin/env python
#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

# Script for generating latex from doxygen-generated xml files.
# The generatetd latex files are compatible with the seL4 manual.

import xml.dom.minidom
import argparse
import sys
import os
import re

# Dict mapping characters to their escape sequence in latex
LATEX_ESCAPE_PATTERNS = {
    "_": "\\_",
}
LATEX_ESCAPE_REGEX = re.compile('|'.join(LATEX_ESCAPE_PATTERNS.keys()))

# Returns the latex doc for the return value of a function
# implied by its return type
def default_return_doc(ret_type):
    if ret_type == "void":
        return "\\noret"

    return ""

# Return a string with latex special characters escaped
def latex_escape(string):
    return LATEX_ESCAPE_REGEX.sub(lambda p: LATEX_ESCAPE_PATTERNS[p.group()], string)

# Return the first node with a given tag inside parent
def get_node(parent, tagname):
    return parent.getElementsByTagName(tagname)[0]

# Return a string containing a concatenation of a nodes text node
# children, recursing into non-text nodes or escaping latex if
# necessary.
def get_text(node, recur=False, escape=True):
    output = ""
    for n in node.childNodes:
        if n.nodeType == xml.dom.Node.TEXT_NODE:
            if escape:
                output += latex_escape(n.data)
            else:
                output += n.data
        elif recur:
            output += get_text(n, True, escape)

    return output

# Parse a paragraph node, handling special doxygen node types
# that may appear inside a paragraph.
def parse_para(para_node, ref_dict={}):
    output = ""
    for n in para_node.childNodes:
        if n.nodeType == xml.dom.Node.TEXT_NODE:
            output += latex_escape(n.data)
        elif n.tagName == "para":
            output += parse_para(n, ref_dict)
        elif n.tagName == "computeroutput":
            output += "\\texttt{%s}" % get_text(n)
        elif n.tagName == "texttt":
            output += "\\texttt{%s}" % latex_escape(n.getAttribute("text"))
        elif len(ref_dict) != 0 and n.tagName == "ref":
            refid = n.getAttribute("refid")
            ref = ref_dict[refid]
            output += "\\apifunc{%(name)s}{%(label)s}" % ref
        elif n.tagName == "nameref":
            name = n.getAttribute("name")
            ref = ref_dict[name]
            output += "\\apifunc{%(name)s}{%(label)s}" % ref
        elif n.tagName == "autoref":
            output += "\\autoref{sec:%s}" % n.getAttribute("sec")
        elif n.tagName == "shortref":
            output += "\\ref{sec:%s}" % n.getAttribute("sec")
        elif n.tagName == "obj":
            output += "\\obj{%s}" % n.getAttribute("name")
        elif n.tagName == "errorenumdesc":
            output += "\\errorenumdesc"

    return output

# Parse the "brief description" section of a doxygen member.
def parse_brief(parent):
    para_nodes = get_node(parent, "briefdescription").getElementsByTagName("para")
    para_text = "\n\n".join([parse_para(n) for n in para_nodes])

    return para_text

# Parse the "detailed description" section of a doxygen member.
def parse_detailed_desc(parent, ref_dict):

    param_nodes = parent.getElementsByTagName("param")
    params = {}
    param_order = []
    for n in param_nodes:
        param_type = get_text(n.getElementsByTagName("type")[0], True)
        if param_type == "void":
            continue
        param_name = get_text(n.getElementsByTagName("declname")[0], True)
        params[param_name] = {"type": param_type}
        param_order.append(param_name)

    detailed_desc = parent.getElementsByTagName("detaileddescription")[0]

    param_items = detailed_desc.getElementsByTagName("parameteritem")
    for param_item in param_items:
        param_name_node = param_item.getElementsByTagName("parametername")[0]
        param_desc_node = param_item.getElementsByTagName("parameterdescription")[0]

        param_name = parse_para(param_name_node, ref_dict)
        param_desc = parse_para(param_desc_node, ref_dict)

        params[param_name]["desc"] = param_desc

    if len(params) == 0:
        params_str = "\\param{void}{}{}"
    else:
        params_str = ""
        for param_name in param_order:
            param_info = params[param_name]
            if "desc" in param_info:
                params_str += "\\param{%(type)s}{%(name)s}{%(desc)s}\n" % {
                    "type": param_info["type"],
                    "name": param_name,
                    "desc": param_info["desc"].strip(),
                }

    details = ""
    for n in detailed_desc.childNodes:
        if n.nodeType == xml.dom.Node.ELEMENT_NODE and \
                n.tagName == "para":

            details += parse_para(n, ref_dict)

    ret_str = get_text(parent.getElementsByTagName("type")[0], recur=True, escape=False)
    ret = default_return_doc(ret_str.split()[-1])
    simplesects = detailed_desc.getElementsByTagName("simplesect")
    for n in simplesects:
        if n.nodeType == xml.dom.Node.ELEMENT_NODE and \
                n.getAttribute("kind") == "return":
            ret = parse_para(n, ref_dict)
            break

    return (details, params_str, ret)

# Extract a function prototype from a doxygen member.
def parse_prototype(parent):
    inline = parent.getAttribute("inline") == "yes"
    static = parent.getAttribute("static") == "yes"
    ret_type = get_text(parent.getElementsByTagName("type")[0], recur=True)
    name = get_text(parent.getElementsByTagName("name")[0])

    output = "%s %s" % (ret_type, name)
    if inline:
        output = "inline " + output
    if static:
        output = "static " + output

    return output

# Return a dict mapping reference ids and reference names
# to details about the referee.
def build_ref_dict(doc):
    ret = {}
    for member in doc.getElementsByTagName("memberdef"):
        manual_node = get_node(member, "manual")
        name = get_text(get_node(member, "name"), escape=False)
        manual_node = get_node(member, "manual")
        label = manual_node.getAttribute("label")
        ref_id = member.getAttribute("id")
        data = {
            "name": latex_escape(name),
            "label": label,
            "ref": ref_id,
        }

        ret[ref_id] = data
        ret[name] = data

    return ret

# Takes a path to a file containing doxygen-generated xml,
# and return a string containing latex suitable for inclusion
# in the sel4 manual.
def generate_general_syscall_doc(input_file_name, level):
    with open(input_file_name, "r") as f:
        output = ""
        doc = xml.dom.minidom.parse(f)
        ref_dict = build_ref_dict(doc)
        elements = doc.getElementsByTagName("memberdef")

        if len(elements) == 0:
            return "No methods."

        for member in elements:
            manual_node = get_node(member, "manual")
            details, params, ret = parse_detailed_desc(member, ref_dict)
            output += """
\\apidoc
[{%(level)s}]
{%(label)s}
{%(name)s}
{%(brief)s}
{%(prototype)s}
{%(params)s}
{%(ret)s}
{%(details)s}
            """ % {
                "level": level,
                "label": manual_node.getAttribute("label"),
                "name": latex_escape(manual_node.getAttribute("name")),
                "brief": parse_brief(member),
                "prototype": parse_prototype(member),
                "params": params,
                "ret": ret,
                "details": details,
            }

        return output

def process_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("-i", "--input", dest="input", type=str,
                        help="File containing doxygen-generated xml.")
    parser.add_argument("-o", "--output", dest="output", type=str,
                        help="Output latex file.")

    parser.add_argument("-l", "--level", choices=["subsection", "subsubsection"],
                        help="LaTeX section level for each method")

    return parser

def main():
    args = process_args().parse_args()

    if not os.path.exists(os.path.dirname(args.output)):
        os.makedirs(os.path.dirname(args.output))

    output_str = generate_general_syscall_doc(args.input, args.level)

    with open(args.output, "w") as output_file:
        output_file.write(output_str)

if __name__ == "__main__":
    sys.exit(main())
