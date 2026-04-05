# Copyright 2022, seL4 Project a Series of LF Projects, LLC
#
# SPDX-License-Identifier: BSD-2-Clause or GPL-2.0-only
#

# Utility functions for processing condition tags in the seL4 xml files

from __future__ import annotations

from typing import List, Sequence, cast
from xml.dom.minidom import Element


def remove_ws_comments(node: Element) -> None:
    """Remove whitespace and comment nodes from the children of the given node.
    Assume being called on a condition tag that only has one child element apart
    from whitespace and comments."""
    for child in list(node.childNodes):
        if child.nodeType == node.COMMENT_NODE or (child.nodeType == node.TEXT_NODE and len(child.data.strip()) == 0):
            node.removeChild(child)
    # childNodes are of type _ElementChildren where
    # _ElementChildren: TypeAlias = Element | ProcessingInstruction | Comment | Text | CDATASection
    # At most one node of type Element should be left. Length is checked elsewhere, only assert type here.
    assert all(isinstance(child, Element) for child in node.childNodes)


def condition_to_cpp(conditions: Sequence[Element]) -> str:
    """Convert the given condition tags list (0 or 1 tags) to a C preprocessor expression."""
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

    def expr_to_cpp(expr: Element) -> str:
        remove_ws_comments(expr)
        child_elems: list[Element] = cast(List[Element], list(expr.childNodes))
        if expr.tagName == "config":
            cfg_var = expr.getAttribute("var")
            if not cfg_var:
                raise Exception("Missing or empty config variable")
            return "defined({})".format(cfg_var)
        elif expr.tagName == "not":
            return "!{}".format(expr_to_cpp(child_elems[0]))
        else:
            op_str = {'and': ' && ', 'or': ' || '}.get(expr.tagName)
            if op_str:
                return '(' + op_str.join([expr_to_cpp(e) for e in child_elems]) + ')'

            raise Exception("Unrecognized element `{}` in condition".format(expr.tagName))

    return expr_to_cpp(cast(Element, children[0]))


def remove_prefix(text: str, prefix: str) -> str:
    if text.startswith(prefix):
        return text[len(prefix):]
    return text


def expr_to_bool(expr: Element, values: dict[str, bool]) -> bool:
    """Evaluate the given condition expression in the provided values environment."""
    # Cast is safe, because the expr element comes out of remove_ws_comments
    child_elems: list[Element] = cast(List[Element], list(expr.childNodes))
    if expr.tagName == "and":
        for child in child_elems:
            if not expr_to_bool(child, values):
                return False
        return True
    elif expr.tagName == "or":
        for child in child_elems:
            if expr_to_bool(child, values):
                return True
        return False
    elif expr.tagName == "not":
        assert len(child_elems) == 1
        return not expr_to_bool(child_elems[0], values)
    elif expr.tagName == "config":
        cfg_var = expr.getAttribute("var")
        if not cfg_var:
            raise Exception("Missing or empty config variable")

        return values[remove_prefix(cfg_var, "CONFIG_")]
    raise Exception("Unrecognized element `{}` in condition".format(expr.tagName))


def condition_to_bool(conditions: Sequence[Element], values: dict[str, bool]) -> bool:
    """Evaluate the given condition tags list (0 or 1 tags) in the provided values environment."""
    n = len(conditions)
    # Expect zero or one <condition> tag in the conditions list.
    assert n <= 1
    if n == 0:
        return True

    remove_ws_comments(conditions[0])
    children = conditions[0].childNodes
    if not children or len(children) == 0:
        return True
    # Expect that a condition tag has exactly one child node.
    assert len(children) == 1

    child = cast(Element, children[0])
    remove_ws_comments(child)

    return expr_to_bool(child, values)
