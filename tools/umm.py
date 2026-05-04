#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import annotations

import sys
from functools import reduce
from typing import Callable, Dict, IO, Iterable, Iterator, Tuple, TypeVar, Union
from typing_extensions import TypeAlias

UmmType: TypeAlias = Union[
    Tuple[str],                    # Unit
    Tuple[str, str],               # Word (size) or Base (name)
    Tuple[str, 'UmmType'],         # Ptr (type)
    Tuple[str, 'UmmType', str],    # Array of (type, size)
]

Field: TypeAlias = Tuple[str, UmmType]
TypeMap: TypeAlias = Dict[str, Iterable[Field]]

T = TypeVar('T')


# We assume length tp > 0

def parse_type(tps: list[str]) -> UmmType:
    def helper(tps: list[str]) -> tuple[UmmType, list[str]]:
        tp = tps[0]
        rest = tps[1:]

        if tp == 'Word':
            return ('Word', rest[0]), rest[1:]

        elif tp == 'Ptr':
            tp2, rest = helper(rest)
            return ('Ptr', tp2), rest

        elif tp == 'Unit':
            return ('Unit',), rest

        elif tp == 'Array':
            tp2, rest = helper(rest)
            # arrays are Array, type, size
            return ('Array', tp2, rest[0]), rest[1:]

        else:
            return ('Base', tp), rest

    return helper(tps)[0]


def splitBy(f: Callable[[T], bool], xs: Iterable[T]) -> list[list[T]]:
    def fold(acc: tuple[list[list[T]], list[T]], v: T) -> tuple[list[list[T]], list[T]]:
        if f(v):
            acc[0].append(acc[1])
            return (acc[0], [])
        else:
            acc[1].append(v)
            return acc

    return (reduce(fold, xs, ([], [])))[0]


def handle_one_struct(s: list[str]) -> tuple[str, Iterator[Field]]:
    def hdl_fld(f: str) -> Field:
        fl, tp = f.split(':')
        return (fl.lstrip(), parse_type(tp.split(' ')))

    name = s[0]
    return (name, map(hdl_fld, s[1:]))


def dict_from_list(ls: Iterable[tuple[str, Iterable[Field]]]) -> TypeMap:
    a: TypeMap = {}
    for k, v in ls:
        a[k] = v

    return a


def is_base(x: UmmType) -> bool:
    return (x[0] == 'Base')


def base_name(x: UmmType) -> str:
    assert is_base(x) and len(x) == 2 and isinstance(x[1], str)
    return x[1]


def paths_to_type(
    mp: TypeMap,
    f: Callable[[UmmType], bool],
    start: str,
) -> list[tuple[list[str], UmmType]]:
    # This assumes that membership is a DAG which is the case in C

    def handle_one(fld: Field) -> list[tuple[list[str], UmmType]]:
        name, tp = fld
        if f(tp):
            return [([start + '.' + name], tp)]
        elif is_base(tp):
            res = paths_to_type(mp, f, base_name(tp))
            # prepend paths by name (why no Cons in python? grrr)
            map(lambda x: (x[0].insert(0, name)), res)
            return res
        else:
            return []

    # init case
    start_tp: UmmType = ('Base', start)
    if f(start_tp):
        return [([], start_tp)]
    else:
        res = map(handle_one, mp[start])
        return (reduce(lambda x, y: x + y, res))


def build_types(file: str) -> TypeMap:
    in_file = open(file, 'r', encoding='utf-8')

    lines = map(lambda x: x.rstrip(), in_file.readlines())

    in_file.close()

    grps = splitBy(lambda x: x == '', lines)

    # the order information will be important if we want _CL for all types
    sts = dict_from_list(map(handle_one_struct, grps))

    return sts


def print_graph(filename: str, out_file: IO[str]) -> None:
    mp = build_types(filename)

    print('digraph types {', file=out_file)
    for k, flds in mp.items():
        for fld, tp in flds:
            # if is_base(tp):
            print('\t "%s" -> "%s" [label="%s"]' % (k, base_name(tp), fld),
                  file=out_file)
    print('}', file=out_file)


if __name__ == '__main__':
    print_graph('umm_types.txt', sys.stdout)
