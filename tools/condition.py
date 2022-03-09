# Copyright 2022, seL4 Project a Series of LF Projects, LLC
#
# SPDX-License-Identifier: BSD-2-Clause or GPL-2.0-only
#

def remove_ws_comments(node):
    for child in list(node.childNodes):
        if child.nodeType == node.COMMENT_NODE or (child.nodeType == node.TEXT_NODE and len(child.data.strip()) == 0):
            node.removeChild(child)


def condition_to_cpp(conditions):
    n = len(conditions)
    # Expect zero or one <condition> tag in the conditions list.
    assert n <= 1
    if n == 0:
        return ""

    remove_ws_comments(conditions[0])
    children = conditions[0].childNodes
    if not children or len(children) == 0:
        return ""
    # Expect that a condition tag has exactly one child node.
    assert len(children) == 1

    def helper(expr):
        remove_ws_comments(expr)
        if expr.tagName == "config":
            cfg_var = expr.getAttribute("var")
            if not cfg_var:
                raise Exception("Missing or empty config variable")
            return "defined({})".format(cfg_var)
        elif expr.tagName == "not":
            return "!{}".format(helper(expr.firstChild))
        else:
            op_str = {'and': ' && ', 'or': ' || '}.get(expr.tagName)
            if op_str:
                return '(' + op_str.join([helper(e) for e in expr.childNodes]) + ')'

            raise Exception("Unrecognized element `{}` in condition".format(expr.tagName))

    return helper(children[0])
