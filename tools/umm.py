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

import sys
import os.path
import optparse
import re

## We assume length tp > 0
def parse_type(tps):
    def helper(tps):
        tp   = tps[0]
        rest = tps[1:]

        if tp == 'Word':
            return ('Word', rest[0]), rest[1:]

        elif tp == 'Ptr':
            tp2, rest = helper(rest)
            return ('Ptr', tp2), rest

        elif tp == 'Unit':
            return ('Unit',) , rest
        
        elif tp == 'Array':
            tp2, rest = helper(rest)
            # arrays are Array ... sz
            return ('Array', tp2, rest[0]), rest[1:]        

        else:
            return ('Base', tp), rest

    return helper(tps)[0]

def splitBy(f, xs):
    def fold(acc, v):
        if f(v):
            acc[0].append(acc[1])
            return (acc[0], [])
        else:
            acc[1].append(v)
            return acc

    return (reduce(fold, xs, ([], [])))[0]

def handle_one_struct(s):
    def hdl_fld(f):
        fl, tp = f.split(':')
        return (fl.lstrip(), parse_type(tp.split(' ')))
        
    name = s[0]
    return (name, map(hdl_fld, s[1:]))

def dict_from_list(ls):
    a = {}
    for k, v in ls:
        a[k] = v
        
    return a

def is_base(x):
    return (x[0] == 'Base')

def base_name(x):
    return x[1]

# This assumes that membership is a DAG which is the case in C
# We could memoize, doesn't seem worth it ...
def paths_to_type(mp, f, start):
    def handle_one(fld):
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
    start_tp = ('Base', start)
    if f(start_tp):
        return [([], start_tp)]
    else:
        res  = map(handle_one, mp[start])    
        return (reduce(lambda x, y: x + y, res))

def build_types(file):
    in_file = open(file, 'r')

    lines = map(lambda x: x.rstrip(), in_file.readlines())

    in_file.close()
    
    grps = splitBy(lambda x: x == '', lines)
    
    # the order information will be important if we want _CL for all types
    sts = dict_from_list(map(handle_one_struct, grps))

    return sts


def print_graph(filename, out_file):
    mp = build_types(filename)

    print >>out_file, 'digraph types {'
    for k, flds in mp.iteritems():
        for fld, tp in flds:
            #if is_base(tp):
            print >> out_file, '\t "%s" -> "%s" [label="%s"]' % \
                               (k, base_name(tp), fld)
    print >>out_file, '}'    
    
## Toplevel

if __name__ == '__main__':
    print_graph('umm_types.txt', sys.stdout)
