#!/usr/bin/env python
#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

"""
Script for generating latex from doxygen-generated xml files.
The generatetd latex files are compatible with the seL4 manual.
"""
import argparse
import sys
import os
import re
from bs4 import BeautifulSoup
import six

# Dict mapping characters to their escape sequence in latex
LATEX_ESCAPE_PATTERNS = {
    "_": "\\_",
}
LATEX_ESCAPE_REGEX = re.compile('|'.join(LATEX_ESCAPE_PATTERNS.keys()))

def default_return_doc(ret_type):
    """
    Returns the latex doc for the return value of a function
    implied by its return type
    """

    if ret_type == "void":
        return "\\noret"

    return ""

def latex_escape(string):
    """
    Return a string with latex special characters escaped
    """

    return LATEX_ESCAPE_REGEX.sub(lambda p: LATEX_ESCAPE_PATTERNS[p.group()], string)

def get_text(soup, escape=True):
    """
    Return a string containing a concatenation of a nodes text node
    children, recursing into non-text nodes or escaping latex if
    necessary.
    """

    if isinstance(soup, str):
        string = soup
    elif isinstance(soup, six.string_types):
        string = str(soup)
    elif soup.string:
        string = str(soup.string)
    else:
        string = soup.get_text()

    if string is not None:
        if escape:
            return latex_escape(string)
        else:
            return string

def ref_tex(refid, ref_dict):
    """Lookup refid in ref_dict and output the latex for an apifunc ref"""
    ref = ref_dict[refid]
    return "\\apifunc{%(name)s}{%(label)s}" % ref

def ref_to_tex(para, ref_dict):
    """Convert a reference by id to a latex command by looking up refid in para"""
    if len(ref_dict) > 0:
        return ref_tex(para["refid"], ref_dict)
    return ""

def nref_to_tex(para, ref_dict):
    """Convert a reference by name to a latex command by looking up refid in para"""
    if len(ref_dict) > 0:
        return ref_tex(para["name"], ref_dict)
    return ""

def parse_list(para, ref_dict, tag):
    """Parse an ordered list element"""
    output = '\\begin{%s}\n' % tag
    for item in para.contents:
        output += parse_para(item, ref_dict)
    output += '\\end{%s}\n' % tag
    return output

def parse_ordered_list(para, ref_dict):
    """orderedlist --> enumerate"""
    return parse_list(para, ref_dict, 'enumerate')

def parse_itemized_list(para, ref_dict):
    """itemizedlist --> itemize"""
    return parse_list(para, ref_dict, 'itemize')

def parse_recurse(para, ref_dict):
    """Recursively parse a para element"""
    # recurse on the contents
    output = ""
    for item in para.contents:
        output += parse_para(item, ref_dict)
    return output

# table of translations of xml children of 'para' elements
PARSE_TABLE = {
    'para'          : parse_recurse,
    'computeroutput': lambda p, r: '\\texttt{%s}' % get_text(p),
    'texttt'        : lambda p, r: '\\texttt{%s}' % get_text(p['text']),
    'ref'           : ref_to_tex,
    'nameref'       : nref_to_tex,
    'shortref'      : lambda p, r: "\\ref{sec:%s}" % p['sec'],
    'obj'           : lambda p, r: "\\obj{%s}" % p['name'],
    'errorenumdesc' : lambda p, r: "\\errorenumdesc",
    'orderedlist'   : parse_ordered_list,
    'listitem'      : lambda p, r: "\\item " + parse_para(p.para, r) + "\n",
    'itemizedlist'  : parse_itemized_list,
    'autoref'       : lambda p, r: "\\autoref{%s}" % p['label'],
}

def parse_para(para_node, ref_dict={}):
    """
    Parse a paragraph node, handling special doxygen node types
    that may appear inside a paragraph. Unhandled cases are
    not parsed and result in an empty string.
    """
    if para_node.name is None:
        return get_text(para_node, escape=True)
    elif para_node.name in PARSE_TABLE:
        return PARSE_TABLE[para_node.name](para_node, ref_dict)
    else:
        return ""

def parse_brief(parent):
    """
    Parse the "brief description" section of a doxygen member.
    """
    para_nodes = parent.find('briefdescription').find_all('para')
    return "\n\n".join([parse_para(n) for n in para_nodes])

def parse_detailed_desc(parent, ref_dict):
    """
    Parse the "detailed description" section of a doxygen member.
    """
    # parse the function parameters
    params = {}
    param_order = []
    types_iter = iter(parent.find_all('type'))
    names = parent.find_all('declname')

    # the first type is the return type
    ret_type = six.next(types_iter)

    # the rest are parameters
    for n in names:
        param_type = six.next(types_iter).text
        if param_type == "void":
            continue
        params[str(n.text)] = {"type": param_type}
        param_order.append(str(n.text))

    param_items = parent.find_all("parameteritem")
    for param_item in param_items:
        param_name_node = param_item.find("parametername")
        param_desc_node = param_item.find("parameterdescription")

        param_name = get_text(param_name_node, escape=False)
        param_desc = parse_para(param_desc_node.find('para'), ref_dict)

        params[param_name]["desc"] = param_desc

    if len(params) == 0:
        params_str = "\\param{void}{}{}"
    else:
        params_str = ""
        for param_name in param_order:
            param_info = params[param_name]
            params_str += "\\param{%(type)s}{%(name)s}{%(desc)s}\n" % {
                "type": get_text(param_info["type"]),
                "name": get_text(param_name),
                "desc": todo_if_empty(param_info.get("desc", "").strip()),
            }


    details = ""
    for n in parent.detaileddescription.find_all('para', recursive=False):
        if not n.parameterlist:
            details += parse_para(n, ref_dict)
            details += "\n\n"

    ret_str = get_text(ret_type, escape=False)
    ret = default_return_doc(ret_str.split()[-1])
    simplesects = parent.find_all("simplesect")
    for n in simplesects:
        if n['kind'] == "return":
            ret = parse_para(n.find('para'), ref_dict)
            break
    return (todo_if_empty(details.strip()), params_str, todo_if_empty(ret.strip()))

def parse_prototype(parent):
    """
    Extract a function prototype from a doxygen member.
    """

    inline = parent["inline"] == "yes"
    static = parent["static"] == "yes"
    ret_type = get_text(parent.find("type"))
    name = get_text(parent.find("name"))

    output = "%s %s" % (ret_type, name)
    if inline:
        output = "inline " + output
    if static:
        output = "static " + output

    return output

def build_ref_dict(soup):
    """
    Return a dict mapping reference ids and reference names
    to details about the referee.
    """

    ret = {}
    for member in soup.find_all("memberdef"):
        name = str(member.find('name').string)
        label = member.manual['label']
        ref_id = member['id']
        data = {
            "name": latex_escape(name),
            "label": label,
            "ref": ref_id,
        }

        ret[ref_id] = data
        ret[name] = data

    return ret

def todo_if_empty(s):
    """
    Returns its argument if its argument is non-none and non-empty,
    otherwise returns "\\todo"
    """
    return s if s else "\\todo"

def generate_general_syscall_doc(input_file_name, level):
    """
    Takes a path to a file containing doxygen-generated xml,
    and return a string containing latex suitable for inclusion
    in the sel4 manual.
    """

    with open(input_file_name, "r") as f:
        output = ""
        soup = BeautifulSoup(f, "lxml")
        ref_dict = build_ref_dict(soup)
        elements = soup.find_all("memberdef")
        summary = soup.find('compounddef')
        # parse any top level descriptions
        for ddesc in summary.find_all('detaileddescription', recursive=False):
            if ddesc.para:
                output += parse_para(ddesc.para)

        # parse all of the function definitions
        if len(elements) == 0:
            return "No methods."

        for member in elements:
            manual_node = member.manual
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
                "label": manual_node["label"],
                "name": latex_escape(manual_node["name"]),
                "brief": todo_if_empty(parse_brief(member)),
                "prototype": parse_prototype(member),
                "params": params,
                "ret": ret,
                "details": details,
            }

        return output

def process_args():
    """Process script arguments"""
    parser = argparse.ArgumentParser()

    parser.add_argument("-i", "--input", dest="input", type=str,
                        help="File containing doxygen-generated xml.")
    parser.add_argument("-o", "--output", dest="output", type=str,
                        help="Output latex file.")

    parser.add_argument("-l", "--level", choices=["subsection", "subsubsection"],
                        help="LaTeX section level for each method")

    return parser

def main():
    """Convert doxygen xml into a seL4 API LaTeX manual format"""
    args = process_args().parse_args()

    if not os.path.exists(os.path.dirname(args.output)):
        os.makedirs(os.path.dirname(args.output))

    output_str = generate_general_syscall_doc(args.input, args.level)

    with open(args.output, "w") as output_file:
        output_file.write(output_str)

if __name__ == "__main__":
    sys.exit(main())
