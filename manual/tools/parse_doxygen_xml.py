#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
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


class Generator(object):
    # Dict mapping characters to their escape sequence in latex
    ESCAPE_PATTERNS = {}

    def get_parse_table(self):
        # table of translations of xml children of 'para' elements
        parse_table = {
            'para': self.parse_recurse,
            'computeroutput': lambda p, r: '%s' % self.get_text(p),
            'texttt': lambda p, r: '%s' % self.get_text(p['text']),
            'ref': self.ref_to_format,
            'nameref': self.nref_to_format,
            'shortref': lambda p, r: "%s" % p['sec'],
            'obj': lambda p, r: "%s" % p['name'],
            'errorenumdesc': lambda p, r: "",
            'orderedlist': self.parse_ordered_list,
            'listitem': lambda p, r: self.parse_para(p.para, r),
            'itemizedlist': self.parse_itemized_list,
            'autoref': lambda p, r: "%s" % p['label'],
            'docref': self.parse_recurse
        }
        return parse_table

    def default_return_doc(self, ret_type):
        """
        Returns the latex doc for the return value of a function
        implied by its return type
        """

        return ""

    def text_escape(self, string):
        """
        Return a string with latex special characters escaped
        """
        escape_regex = re.compile(re.escape('|'.join(self.ESCAPE_PATTERNS.keys())))
        return escape_regex.sub(lambda p: self.ESCAPE_PATTERNS[p.group()], string)

    def get_text(self, soup, escape=True):
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
            # HACK: Due to the extra spacing that gets inserted by our scripts between XML
            # elements to ensure doxygen can read xmlonly tags, we can no longer tell here
            # if the text after an xml tag should have been flush against the result
            # of the xml tag or not.
            #
            # For example, the following in an IDL file:
            #    Testing <texttt text="1"/>, 2, 3
            # Generates these C comments:
            #    Testing  @xmlonly <texttt text="1"/> @endxmlonly , 2, 3
            # Which generates this doxygen output:
            #    Testing <texttt text="1"/>  , 2, 3
            #
            # To deal with this, just pick out punctuation that looks like it should have been
            # flush against the xml and remove the leading spaces.
            if string.startswith("  ,") or string.startswith("  ."):
                string = string[2:]

            if escape:
                return self.text_escape(string)
            else:
                return string

    def ref_format(self, refid, ref_dict):
        """Lookup refid in ref_dict and output the api function reference"""
        return ""

    def ref_to_format(self, para, ref_dict):
        """Convert a reference by id to a latex command by looking up refid in para"""
        if len(ref_dict) > 0:
            return self.ref_format(para["refid"], ref_dict)
        return ""

    def nref_to_format(self, para, ref_dict):
        """Convert a reference by name to a latex command by looking up refid in para"""
        if len(ref_dict) > 0:
            return self.ref_format(para["name"], ref_dict)
        return ""

    def parse_list(self, para, ref_dict, tag):
        return ""

    def parse_ordered_list(self, para, ref_dict):
        """orderedlist --> enumerate"""
        return self.parse_list(para, ref_dict, 'enumerate')

    def parse_itemized_list(self, para, ref_dict):
        """itemizedlist --> itemize"""
        return self.parse_list(para, ref_dict, 'itemize')

    def parse_recurse(self, para, ref_dict):
        """Recursively parse a para element"""
        # recurse on the contents
        output = ""
        for item in para.contents:
            output += self.parse_para(item, ref_dict)
        return output

    def parse_para(self, para_node, ref_dict={}):
        """
        Parse a paragraph node, handling special doxygen node types
        that may appear inside a paragraph. Unhandled cases are
        not parsed and result in an empty string.
        """
        parse_table = self.get_parse_table()
        if para_node.name is None:
            return self.get_text(para_node, escape=True)
        elif para_node.name in parse_table:
            return parse_table[para_node.name](para_node, ref_dict)
        else:
            return ""

    def parse_brief(self, parent):
        """
        Parse the "brief description" section of a doxygen member.
        """
        para_nodes = parent.find('briefdescription').find_all('para')
        return "\n\n".join([self.parse_para(n) for n in para_nodes])

    def parse_detailed_desc(self, parent, ref_dict):
        """
        Parse the "detailed description" section of a doxygen member.
        """
        # parse the function parameters
        params = {}
        param_order = []
        types_iter = iter(parent.find_all('type'))
        names = parent.find_all('declname')
        errors = {}

        # the first type is the return type
        ret_type = six.next(types_iter)

        # the rest are parameters
        for n in names:
            param_type = six.next(types_iter).text
            if param_type == "void":
                continue
            params[str(n.text)] = {"type": param_type}
            param_order.append(str(n.text))

        param_items = parent.find_all(
            lambda e: e.name == "parameteritem" and e.parent["kind"] == "param")

        for param_item in param_items:
            param_name_node = param_item.find("parametername")
            param_desc_node = param_item.find("parameterdescription")

            param_name = self.get_text(param_name_node, escape=False)
            param_desc = self.parse_para(param_desc_node.find('para'), ref_dict)

            params[param_name]["desc"] = param_desc

        if len(params) == 0:
            params_str = self.generate_empty_param_string()
        else:
            params_str = ""
            for param_name in param_order:
                param_info = params[param_name]
                params_str += self.generate_param_string(param_info, param_name)

        error_items = parent.find_all(
            lambda e: e.name == "parameteritem" and e.parent["kind"] == "retval")

        for error_item in error_items:
            error_name_node = error_item.find("parametername")
            error_desc_node = error_item.find("parameterdescription")

            error_name = self.get_text(error_name_node, escape=False)
            error_desc = self.parse_para(error_desc_node.find('para'), ref_dict)

            errors[error_name] = {"desc": error_desc}

        if len(errors) == 0:
            errors_str = self.generate_empty_error_string()
        else:
            errors_str = ""
            for error_name in sorted(errors):
                error_info = errors[error_name]
                errors_str += self.generate_error_string(error_info, error_name)

        details = ""
        for n in parent.detaileddescription.find_all('para', recursive=False):
            if not n.parameterlist:
                details += self.parse_para(n, ref_dict)
                details += "\n\n"

        ret_str = self.get_text(ret_type, escape=False)
        ret = self.default_return_doc(ret_str.split()[-1])
        simplesects = parent.find_all("simplesect")
        for n in simplesects:
            if n['kind'] == "return":
                ret = self.parse_para(n.find('para'), ref_dict)
                break
        return (self.todo_if_empty(details.strip()), params_str, errors_str, self.todo_if_empty(ret.strip()))

    def parse_prototype(self, parent, escape=True):
        """
        Extract a function prototype from a doxygen member.
        """

        inline = parent["inline"] == "yes"
        static = parent["static"] == "yes"
        ret_type = self.get_text(parent.find("type"), escape)
        name = self.get_text(parent.find("name"), escape)

        output = "%s %s" % (ret_type, name)
        if inline:
            output = "inline " + output
        if static:
            output = "static " + output

        return output

    def build_ref_dict(self, soup):
        """
        Return a dict mapping reference ids and reference names
        to details about the referee.
        """

        ret = {}
        for member in soup.find_all("memberdef"):
            name = str(member.find('name').string)
            label = member.manual['label']
            heading = member.manual['name']
            ref_id = member['id']
            data = {
                "name": self.text_escape(name),
                "original_name": name,
                "label": label,
                "ref": ref_id,
                "heading": heading,
            }

            ret[ref_id] = data
            ret[name] = data

        return ret

    def generate_param_string(self, param_info, param_name):
        return ""

    def generate_empty_param_string(self):
        return ""

    def generate_error_string(self, error_info, error_name):
        return ""

    def generate_empty_error_string(self):
        return ""

    def generate_api_doc(self, level, member, params, ret, details, errors):
        return ""

    def todo_if_empty(self, s):
        """
        Returns its argument if its argument is non-none and non-empty,
        otherwise returns "TODO"
        """
        return s if s else "TODO"


class LatexGenerator(Generator):
    """
    A class that represents the generator for Doxygen to Latex. A child of the Generator class.
    """

    # Dict mapping characters to their escape sequence in latex
    ESCAPE_PATTERNS = {
        "_": "\\_",
    }

    def get_parse_table(self):
        parse_table = super(LatexGenerator, self).get_parse_table()
        parse_table['computeroutput'] = lambda p, r: '\\texttt{%s}' % self.get_text(p)
        parse_table['texttt'] = lambda p, r: '\\texttt{%s}' % self.get_text(p['text'])
        parse_table['shortref'] = lambda p, r: "\\ref{sec:%s}" % p['sec']
        parse_table['obj'] = lambda p, r: "\\obj{%s}" % p['name']
        parse_table['errorenumdesc'] = lambda p, r: "\\errorenumdesc"
        parse_table['listitem'] = lambda p, r: "\\item " + self.parse_para(p.para, r) + "\n"
        parse_table['autoref'] = lambda p, r: "\\autoref{%s}" % p['label']
        return parse_table

    def default_return_doc(self, ret_type):
        """
        Returns the latex doc for the return value of a function
        implied by its return type
        """

        if ret_type == "void":
            return "\\noret"
        return ""

    def ref_format(self, refid, ref_dict):
        """Lookup refid in ref_dict and output the formatted latex reference"""

        ref = ref_dict[refid]
        return "\\apifunc{%(name)s}{%(label)s}" % ref

    def parse_list(self, para, ref_dict, tag):
        """Parse an ordered list element"""

        output = '\\begin{%s}\n' % tag
        for item in para.contents:
            output += self.parse_para(item, ref_dict)
        output += '\\end{%s}\n' % tag
        return output

    def todo_if_empty(self, s):
        return s if s else "\\todo"

    def generate_param_string(self, param_info, param_name):
        return "\\param{%(type)s}{%(name)s}{%(desc)s}\n" % {
            "type": self.get_text(param_info["type"]),
            "name": self.get_text(param_name),
            "desc": self.todo_if_empty(param_info.get("desc", "").strip()),
        }

    def generate_empty_param_string(self):
        return "\\param{void}{}{}"

    def generate_errors(self, error_string):
        """
        Wraps the errors in an \errortable
        """

        if error_string:
            return "\errortable{%s}" % error_string
        return ""

    def generate_error_string(self, error_info, error_name):
        return "\\error{%(name)s}{%(desc)s}\n" % {
            "name": self.get_text(error_name),
            "desc": self.todo_if_empty(error_info.get("desc", "").strip()),
        }

    def generate_api_doc(self, level, member, params, ret, details, errors):
        manual_node = member.manual
        return """
\\apidoc
[{%(level)s}]
{%(label)s}
{%(name)s}
{%(brief)s}
{%(prototype)s}
{%(params)s}
{%(ret)s}
{%(details)s}
{%(errors)s}
        """ % {
            "level": self.level_to_header(level),
            "label": manual_node["label"],
            "name": self.text_escape(manual_node["name"]),
            "brief": self.todo_if_empty(self.parse_brief(member)),
            "prototype": self.parse_prototype(member),
            "params": params,
            "ret": ret,
            "errors": self.generate_errors(errors),
            "details": details,
        }

    def level_to_header(self, level):
        if level == 0:
            return 'chapter'
        elif level == 1:
            return 'section'
        elif level == 2:
            return 'subsection'
        elif level == 3:
            return 'subsubsection'
        else:
            return 'paragraph'

    def level_to_heading(self, level, name):
        return '\\' + self.level_to_header(level) + '{' + self.text_escape(name) + '}'

    def gen_label(self, label):
        return '\\label{' + label + '}\n'


class MarkdownGenerator(Generator):
    """
    A class that represents the generator for Doxygen to Markdown. A child of the Generator class
    """

    # Dict mapping characters to their escape sequence in markdown
    ESCAPE_PATTERNS = {
        "`": "\`",
        "#": "\#",
        "_": "\_",
        "*": "\*",
        "[": "\[",
        "]": "\]",
        "-": "\-",
        "+": "\+",
        "!": "\!",
    }

    def get_parse_table(self):
        parse_table = super(MarkdownGenerator, self).get_parse_table()
        parse_table['computeroutput'] = lambda p, r: '`%s`' % self.get_text(p, escape=False)
        parse_table['texttt'] = lambda p, r: '`%s`' % self.get_text(p['text'], escape=False)
        parse_table['obj'] = lambda p, r: '**%s**' % p['name']
        parse_table['errorenumdesc'] = lambda p, r: '%s' % self.get_error_num_description()
        parse_table['listitem'] = lambda p, r: self.parse_para(p.para, r) + "\n\n"
        parse_table['autoref'] = lambda p, r: "autoref[%s]" % p['label']
        parse_table['docref'] = lambda p, r: "DOCREF"
        return parse_table

    def default_return_doc(self, ret_type):
        """
        Returns the description for the return value of a function
        implied by its return type
        """

        if ret_type == "void":
            return "This method does not return anything."
        return ""

    def ref_format(self, refid, ref_dict):
        """
        Lookup refid in ref_dict and output the formatted Markdown reference
        Creates a Markdown link
        """

        ref = ref_dict[refid]
        ref_anchor = (ref['heading'].lower()).replace(" ", "-")
        return "[`%s`](#%s)" % (ref['original_name'], ref_anchor)

    def get_error_num_description(self):
        return "A return value of `0` indicates success. A non-zero value indicates that an error occurred."

    def generate_itemize_list(self, para, ref_dict, output):
        """ Returns a Markdown item list """

        for item in para.contents:
            parsed_item = self.parse_para(item, ref_dict)
            output += "* %s" % parsed_item if parsed_item.rstrip() else ""
        return output

    def generate_enumerate_list(self, para, ref_dict, output):
        """ Returns a Markdown number list """

        for num, item in zip(range(sys.maxsize), para.contents):
            parsed_item = self.parse_para(item, ref_dict)
            output += "%d. %s" % (num, parsed_item) if parsed_item.rstrip() else ""
        return output

    def parse_list(self, para, ref_dict, tag):
        """Parse an ordered list element"""

        if tag == "enumerate":
            list_generator = self.generate_enumerate_list
        elif tag == "itemize":
            list_generator = self.generate_itemize_list
        output = '\n'
        output += list_generator(para, ref_dict, output)
        return output

    def todo_if_empty(self, s):
        return s if s else "*TODO*"

    def generate_params(self, param_string):
        """
        Returns the params in a formatted Markdown table
        """

        if param_string:
            return """
Type | Name | Description
--- | --- | ---
%s
            """ % param_string
        return ""

    def generate_param_string(self, param_info, param_name):
        return "`%(type)s` | `%(name)s` | %(desc)s\n" % {
            "type": self.get_text(param_info["type"], escape=False),
            "name": self.get_text(param_name, escape=False),
            "desc": self.todo_if_empty(param_info.get("desc", "").strip()),
        }

    def generate_errors(self, error_string):
        """
        Returns the errors in a formatted Markdown table
        """

        if error_string:
            return """
Error Code | Possible Cause
--- | ---
%s
            """ % error_string
        return ""

    def generate_error_string(self, error_info, error_name):
        return "`%(name)s` | %(desc)s\n" % {
            "name": self.get_text(error_name, escape=False),
            "desc": self.todo_if_empty(error_info.get("desc", "").strip()),
        }

    def generate_api_doc(self, level, member, params, ret, details, errors):
        manual_node = member.manual

        # Descriptions that just contain a document reference are removed.
        # Found by the 'DOCREF' symbol
        match_details = re.match(r'^DOCREF$', details, re.M | re.I)
        if match_details:
            details_string = ""
        else:
            details_string = "**Description:** " + re.sub(r"\n(?!\n)", " ", details)

        ret_string = "**Return value:** " + re.sub("\n(?!\n)", " ", ret)

        # Removed any DOCREF symbols from the return, details and param strings
        ret_string = re.sub(r'DOCREF', "", ret_string)
        details_string = re.sub(r'DOCREF', "", details_string)
        params_string = re.sub(r'DOCREF', "", params)
        errors_string = re.sub(r'DOCREF', "", errors)

        return """
%(hash)s %(name)s
`%(prototype)s`

%(brief)s
%(params)s
%(ret)s

%(errors)s

%(details)s
""" % {
            "hash": self.level_to_header(level),
            "name": self.text_escape(manual_node["name"]),
            "label": manual_node["label"],
            "brief": self.todo_if_empty(self.parse_brief(member)),
            "prototype": self.parse_prototype(member, escape=False),
            "params": self.generate_params(params_string),
            "ret": ret_string,
            "errors": self.generate_errors(errors_string),
            "details": details_string,
        }

    def level_to_header(self, level):
        return (level + 1) * '#'

    def level_to_heading(self, level, name):
        return self.level_to_header(level) + ' ' + self.text_escape(name) + '\n'

    def gen_label(self, label):
        return ''


def generate_general_syscall_doc(generator, input_file_name, level, ref_dict):
    """
    Takes a path to a file containing doxygen-generated xml,
    and return a string containing latex suitable for inclusion
    in the sel4 manual.
    """

    dir_name = os.path.dirname(input_file_name)
    with open(input_file_name, "r") as f:
        output = ""
        soup = BeautifulSoup(f, features="lxml-xml")
        elements = soup.find_all("memberdef")
        summary = soup.find('compounddef')
        # parse any top level descriptions
        for ddesc in summary.find_all('detaileddescription', recursive=False):
            if ddesc.para:
                output += generator.parse_para(ddesc.para)

        # parse any nested groups
        for inner_group in soup.find_all("innergroup"):
            new_input_file_name = inner_group["refid"] + '.xml'
            new_input_file = os.path.join(dir_name, new_input_file_name)
            output += generator.level_to_heading(level, inner_group.text)
            output += generator.gen_label(inner_group["refid"])
            output += generate_general_syscall_doc(generator, new_input_file, level + 1, ref_dict)

        # parse all of the function definitions
        if len(elements) == 0 and output == "":
            return "No methods."

        for member in elements:
            manual_node = member.manual
            details, params, errors, ret = generator.parse_detailed_desc(member, ref_dict)
            output += generator.generate_api_doc(level, member, params, ret, details, errors)
        return output


def process_args():
    """Process script arguments"""
    parser = argparse.ArgumentParser()

    parser.add_argument("-f", "--format", choices=["latex", "markdown"],
                        default="latex", help="Format of doxygen output")

    parser.add_argument("-i", "--input", dest="input", type=str,
                        help="File containing doxygen-generated xml.")
    parser.add_argument("-o", "--output", dest="output", type=str,
                        help="Output latex file.")

    parser.add_argument("-l", "--level", type=int,
                        help="Level for each method, 0 = top level")

    return parser


def main():
    """Convert doxygen xml into a seL4 API LaTeX manual format"""
    args = process_args().parse_args()

    if not os.path.exists(os.path.dirname(args.output)):
        os.makedirs(os.path.dirname(args.output))

    if args.format == "latex":
        generator = LatexGenerator()
    elif args.format == "markdown":
        generator = MarkdownGenerator()

    dir_name = os.path.dirname(args.input)

    # create the refdict from all the group__*SystemCalls.xml files
    ref_dict = {}
    for (r, d, files) in os.walk(dir_name):
        for f in files:
            if "SystemCalls" not in f:
                continue
            with open(os.path.join(dir_name, f), "r") as source:
                soup = BeautifulSoup(source, features="lxml-xml")
                ref_dict.update(generator.build_ref_dict(soup))

    output_str = generate_general_syscall_doc(generator, args.input, args.level, ref_dict)

    with open(args.output, "w") as output_file:
        output_file.write(output_str)


if __name__ == "__main__":
    sys.exit(main())
