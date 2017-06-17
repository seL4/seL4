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

"""
Script for generating latex from doxygen-generated xml files.
The generatetd latex files are compatible with the seL4 manual.
"""
import argparse
import sys
import os
import re
from bs4 import BeautifulSoup
from bs4 import Tag
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

class NoSuchNode(Exception):
    pass

def latex_escape(string):
    """
    Return a string with latex special characters escaped
    """

    return LATEX_ESCAPE_REGEX.sub(lambda p: LATEX_ESCAPE_PATTERNS[p.group()], string)

def get_text(soup, recur=False, escape=True):
    """
    Return a string containing a concatenation of a nodes text node
    children, recursing into non-text nodes or escaping latex if
    necessary.
    """

    output = ""
    string = None
    if type(soup) == str:
        string = soup
    elif soup.string:
        string = str(soup.string)

    if string is not None:
        if escape:
            output += latex_escape(string)
        else:
            output += string
    elif recur:
        for child in soup.contents:
            output += get_text(child, True, escape)

    return output

def parse_para(para_node, ref_dict={}):
    """
    Parse a paragraph node, handling special doxygen node types
    that may appear inside a paragraph.
    """
    # recursive case
    if para_node.name == 'para':
        # recurse on the contents
        output = ""
        for n in para_node.contents:
            output += parse_para(n, ref_dict)
        return output

    # base cases
    if isinstance(para_node, str):
    	return latex_escape(str(para_node))
    elif isinstance(para_node, unicode):
    	return latex_escape(str(para_node))
    elif para_node.string:
        return latex_escape(str(para_node.string))
    elif para_node.name == 'computeroutput':
        return "\\texttt{%s}" % get_text(para_node)
    elif para_node.name == 'texttt':
         return "\\texttt{%s}" % latex_escape(para_node['text'])
    elif len(ref_dict) != 0 and para_node.name == 'ref':
        refid = para_node.ref["refid"]
        ref = ref_dict[refid]
        return "\\apifunc{%(name)s}{%(label)s}" % ref
    elif len(ref_dict) != 0 and para_node.name == 'nameref':
	name = para_node["name"]
        ref = ref_dict[name]
        return "\\apifunc{%(name)s}{%(label)s}" % ref
    elif para_node.name == 'autoref':
        return "\\autoref{sec:%s}" % para_node['sec']
    elif para_node.name == 'shortref':
        return "\\ref{sec:%s}" % para_node['sec']
    elif para_node.name == 'obj':
        return "\\obj{%s}" % para_node['name']
    elif para_node.name == 'errorenumdesc':
        return "\\errorenumdesc"
    elif para_node.name == 'orderedlist':
        output = '\\begin{enumerate}\n'
        for n in para_node.find_all('listitem'):
            output += '\\item '
            output += parse_para(n.para, ref_dict)
            output += '\n'
        output += '\\end{enumerate}\n'
        return output
    else:
        return ""


def parse_brief(parent):
    """
    Parse the "brief description" section of a doxygen member.
    """

    para_text = "\n\n"
    if 'briefdescription' in parent:
    	para_nodes = parent.briefdescription.find_all('para')
    	para_text = "\n\n".join([parse_para(n) for n in para_nodes])

    return para_text

def parse_detailed_desc(parent, ref_dict):
    """
    Parse the "detailed description" section of a doxygen member.
    """

    # parse the function parameters
    param_nodes = parent.find_all("param")
    params = {}
    param_order = []
    types_iter = iter(parent.find_all('type'))
    names = parent.find_all('declname')

    # the first type is the return type, so skip it
    types = list(types_iter.next())

    for n in names:
        param_type = types_iter.next().text
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
                "type": latex_escape(param_info["type"]),
                "name": latex_escape(param_name),
                "desc": todo_if_empty(param_info.get("desc", "").strip()),
            }


    details = ""
    for n in parent.detaileddescription.contents:
        if n and isinstance(n, Tag) and n.name == 'para' and not n.parameterlist:
            details += parse_para(n, ref_dict)
            details += "\n\n"

    ret_str = get_text(parent.find_all("type")[0], recur=True, escape=False)
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
    ret_type = get_text(parent.find_all("type")[0], recur=True)
    name = get_text(parent.find_all("name")[0])

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

        # parse any top level descriptions
        for n in soup.doxygen.compounddef.contents:
            if n.name == 'detaileddescription':
                if n.para:
                    output += parse_para(n.para)

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
