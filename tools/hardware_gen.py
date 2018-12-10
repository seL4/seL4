#!/usr/bin/env python
#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

from __future__ import print_function, division
import sys
import os.path
import argparse
import logging
import pyfdt.pyfdt
import yaml
from jinja2 import Environment, BaseLoader

try:
    from jsonschema import validate
except ImportError:
    print("Skipping hardware YAML validation, `pip install jsonschema` to validate")
    def validate(*args, **kwargs):
        pass

def align_down(addr, boundary):
    return addr & ~(boundary - 1)

def align_up(addr, boundary):
    return (addr + (boundary - 1)) & ~(boundary - 1)

def make_number(cells, array):
    """ make a number with "cells" cells from the given list """
    ret = 0
    for i in range(cells):
        ret = (ret << 32)
        ret |= array.pop(0)
    return ret

class Region:
    def __init__(self, start, size, name, index=0):
        self.start = start
        self.size = size

        if isinstance(name, Region):
            other = name
            self.names = set(other.names)
            self.index = other.index
            self.kaddr = other.kaddr
            self.kernel_var = other.kernel_var
            self.kernel_macro = other.kernel_macro
            self.user_macro = other.kernel_macro
        else:
            self.names = set()
            self.names.add(name)
            self.index = index
            self.kaddr = 0
            self.kernel_var = ""
            self.kernel_macro = ""
            self.user_macro = False

    def __eq__(self, other):
        if other.start == self.start and other.size == self.size:
            return True
        return False

    def __hash__(self):
        return hash((self.start, self.size))

    def __str__(self):
        return '%s(0x%x-0x%x)'%('+'.join(sorted(self.names)), self.start, self.start + self.size - 1)

    def __repr__(self):
        return str(self)

    def isempty(self):
        return self.size <= 0

    def overlaps(self, other):
        sstart = self.start
        send = self.start + self.size
        ostart = other.start
        oend = other.start + other.size
        return (sstart <= ostart and send > ostart) or \
                (ostart <= sstart and oend > sstart)

    def remove_subregions(self, subregions):
        new = []
        for i in range(len(subregions)):
            reg = subregions[i]
            base = reg['address']
            end = reg['size'] + base

            if self.start <= base < (self.start + self.size):
                newsize = base - self.start
                newstart = end
                if newstart < (self.start + self.size):
                    newreg = Region(newstart, (self.start + self.size - newstart), self)
                    new += newreg.remove_subregions(subregions[i+1:])
                self.size = newsize
        new.append(self)
        return filter(lambda a: not a.isempty(), new)


class Device:
    def __init__(self, node, parent, name):
        self.props = {}
        self.node = node
        self.parent = parent
        self.name = name
        for o in node:
            if isinstance(o, pyfdt.pyfdt.FdtNop):
                continue
            self.props[o.get_name()] = o

    def get_addr_cells(self):
        if '#address-cells' in self.props:
            return self.props['#address-cells'].words[0]
        return 2

    def get_size_cells(self):
        if '#size-cells' in self.props:
            return self.props['#size-cells'].words[0]
        return 1

    def translate_child_address(self, addr):
        if self.parent is None:
            # the root node does not need to do any translation of addresses
            return addr
        if 'ranges' not in self.props or not isinstance(self.props['ranges'], pyfdt.pyfdt.FdtPropertyWords):
            return self.parent.translate_child_address(addr)

        # ranges is a list with the following format:
        # <child-bus-address> <parent-bus-address> <length>
        # child-bus-address is self.get_addr_cells() cells long
        # parent-bus-address is self.parent.get_addr_cells() cells long
        # length is self.get_size_cells() cells long
        child_addr_cells = self.get_addr_cells()
        parent_addr_cells = self.parent.get_addr_cells()
        size_cells = self.get_size_cells()

        # We assume that a child's region
        # will stay within one "ranges" entry.
        data = list(self.props['ranges'].words)
        while len(data) > 0:
            cbase = make_number(child_addr_cells, data)
            pbase = make_number(parent_addr_cells, data)
            length = make_number(size_cells, data)

            if cbase <= addr < (cbase + length):
                return self.parent.translate_child_address(addr - cbase + pbase)
        # if we get here, the device tree is probably wrong - print out a warning
        # but continue as though the address was identity-mapped.
        logging.warning("Untranslatable address 0x%x in %s, not translating"%(addr, self.name))
        return self.parent.translate_child_address(addr)

    def is_memory(self):
        return 'device_type' in self.props and self.props['device_type'].strings[0] == 'memory'

    def regions(self, config, by_phandle):
        addr_cells = self.parent.get_addr_cells()
        size_cells = self.parent.get_size_cells()

        if addr_cells == 0 or size_cells == 0 or 'reg' not in self:
            return (set(), set())

        regions = []
        prop = self['reg']
        data = list(prop.words)
        while len(data) > 0:
            base = make_number(addr_cells, data)
            length = make_number(size_cells, data)

            regions.append(Region(self.parent.translate_child_address(base), length, self.node.get_name()))

        if not self.is_memory() and 'compatible' in self.props:
            (user, kernel) = config.split_regions(self, regions, by_phandle)
        else:
            user = set(regions)
            kernel = set()

        return (user, kernel)

    def __getitem__(self, val):
        return self.props[val]

    def __contains__(self, key):
        return key in self.props

class Config:
    def __init__(self, blob):
        self.devices = {}
        self.blob = blob
        self.chosen = None
        self.aliases = None
        self.buses = blob['buses']
        # wrangle the json a little so it's easier
        # to figure out which rules apply to a given device
        for dev in blob['devices']:
            for compat in dev['compatible']:
                if compat in self.devices:
                    self.devices[compat].append(dev)
                else:
                    self.devices[compat] = [dev]

    def set_chosen(self, chosen):
        self.chosen = chosen

    def set_aliases(self, aliases):
        self.aliases = aliases

    def _apply_rule(self, region, rule):
        if 'kernel' in rule and rule['kernel'] != False:
            region.kernel_var = rule['kernel']
            region.kernel_macro = rule.get('macro', None)
            region.executeNever = rule['executeNever']
            region.user_macro = (not rule.get('user', False)) and region.kernel_macro is not None
        return region

    def _lookup_alias(self, name):
        if self.aliases is None:
            return ''

        if name not in self.aliases:
            return ''

        return self.aliases[name].strings[0]

    def _is_chosen(self, device, rule, by_phandle):
        if 'chosen' not in rule:
            return True
        if self.chosen is None:
            return False

        prop = rule['chosen']
        if prop not in self.chosen:
            return False

        val = self.chosen[prop]
        # a "chosen" field will either have a phandle, or
        # a path or an alias, then a ":", then other data.
        # the path/alias may not contain a ":".
        if isinstance(val, pyfdt.pyfdt.FdtPropertyWords):
            return 'phandle' in device and device['phandle'].words[0] == val.words[0]
        val = val.strings[0].split(':')[0]

        # path starts with '/'.
        if val[0] == '/':
            return device.name == val
        else:
            return device.name == self._lookup_alias(val)

    def split_regions(self, device, regions, by_phandle):
        compat = device['compatible']
        for compatible in compat.strings:
            if compatible not in self.devices:
                continue

            user = set()
            kernel = set()

            default_user = True
            for rule in self.devices[compatible]:
                regs = []
                if not self._is_chosen(device, rule, by_phandle):
                    continue
                if 'regions' in rule:
                    for reg_rule in rule['regions']:
                        if 'index' not in reg_rule:
                            default_user = reg_rule['user']
                            continue
                        if reg_rule['index'] >= len(regions):
                            continue
                        reg = self._apply_rule(regions[reg_rule['index']], reg_rule)
                        if reg.user_macro or ('user' in reg_rule and reg_rule['user'] == True):
                            user.add(reg)
                        regs.append(reg)

                kernel.update(set(regs))

            if default_user:
                user = user.union(set(regions).difference(kernel))

            return (user, kernel)
        return (set(regions), set())

def is_compatible(node, compatibles):
    """ returns True if node matches a compatible in the given list """
    try:
        prop = node.index("compatible")
    except ValueError:
        return False
    for c in compatibles:
        if c in node[prop].strings:
            return True
    return False

def should_parse_regions(root, node, buses):
    """ returns True if we should parse regions found in this node. """
    parent = node.get_parent_node()

    return parent == root or is_compatible(parent, buses)

def find_devices(dtb, cfg):
    devices = {}
    nodes = {}
    by_phandle = {}
    chosen = None
    aliases = None
    root = dtb.get_rootnode()
    devices[root] = Device(root, None, '/')
    nodes[root] = devices[root]
    for child in root.walk(): # walk every node in the whole tree
        name = child[0]
        child = child[1]
        if not isinstance(child, pyfdt.pyfdt.FdtNode):
            continue
        if name == '/chosen':
            cfg.set_chosen(Device(child, None, name))
        elif name == '/aliases':
            cfg.set_aliases(Device(child, None, name))
        if should_parse_regions(root, child, cfg.buses):
            devices[child] = Device(child, devices[child.get_parent_node()], name)
            nodes[child] = devices[child]
        else:
            nodes[child] = Device(child, nodes[child.get_parent_node()], name)

        try:
            idx = child.index('phandle')
        except ValueError:
            continue

        prop = child[idx]
        by_phandle[prop.words[0]] = nodes[child]
    # the root node is not actually a device, so remove it.
    del devices[root]
    return {d.name: d for d in devices.values()}, by_phandle

def fixup_device_regions(regions, pagesz, merge=False):
    """ page align all regions and check for overlapping regions """
    ret = list()
    for r in regions:
        r.start = align_down(r.start, pagesz)
        r.size = align_up(r.size, pagesz)
        # we abuse the fact that regions will be
        # "equal" if they have different names but the same range.
        if r in ret:
            ret[ret.index(r)].names.update(r.names)
        else:
            ret.append(r)

    if merge:
        ret = sorted(ret, key=lambda a: a.start)
        i = 1
        while i < len(ret):
            if ret[i].start == ret[i-1].start + ret[i-1].size:
                ret[i-1].size += ret[i].size
                ret[i-1].names.update(ret[i].names)
                del ret[i]
            else:
                i += 1


    return set(ret)


HEADER_TEMPLATE = """
/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
/*
 * This file is autogenerated by kernel/tools/hardware_gen.py from
 * {{args.dtb.name}}.
 */

#ifndef __PLAT_DEVICES_GEN_H
#define __PLAT_DEVICES_GEN_H

#define physBase        {{ "0x%x" % physBase }}

#if 0
static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {%- for reg in kernel %}
    {%- if reg.kernel_macro %}
#ifdef {{ reg.kernel_macro }}
    {%- endif %}
#define {{ reg.kernel_var }} {{ "0x%x" % reg.kaddr }}
    { /* {{ ' '.join(sorted(reg.names)) }} */
      {{ "0x%x" % reg.start }},
      {{ reg.kernel_var }},
      {{ "true" if reg.executeNever else "false" }} /* armExecuteNever */
    },
    {%- if reg.kernel_macro %}
#endif /* {{ reg.kernel_macro }} */
    {%- endif %}
    {%- endfor %}
};
#endif

static const p_region_t BOOT_RODATA avail_p_regs[] = {
    {%- for reg in memory %}
    { {{ "0x%x" % reg.start }}, {{ "0x%x" % (reg.start + reg.size) }} }, /* {{ ' '.join(sorted(reg.names)) }} */
    {%- endfor %}
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    {%- for reg in devices %}
    {%- if reg.user_macro %}
#ifndef {{ reg.kernel_macro }}
    {%- endif %}
    { {{ "0x%x" % reg.start }}, {{ "0x%x" % (reg.start + reg.size) }} }, /* {{ ' '.join(sorted(reg.names)) }} */
    {%- if reg.user_macro %}
#endif /* {{ reg.kernel_macro }} */
    {%- endif %}
    {%- endfor %}
};

#endif /* __PLAT_DEVICES_GEN_H */
"""

def output_regions(args, devices, memory, kernel, fp):
    """ generate the device list for the C header file """
    memory = sorted(memory, key=lambda a: a.start)
    kernel = sorted(kernel, key=lambda a: a.start)
    addr = 0x0 # args.kernel_base
    for reg in kernel:
        reg.kaddr = addr
        addr += align_up(reg.size, 1 << args.page_bits)
    # make sure physBase is at least 16MB (supersection) aligned for the ELF loader's sake.
    # TODO: this may need to be larger for aarch64. It seems to work OK on currently supported platforms though.
    paddr = align_up(memory[0].start, 1 << args.phys_align)
    memory[0].size -= paddr - memory[0].start
    memory[0].start = paddr
    template = Environment(loader=BaseLoader, trim_blocks=False, lstrip_blocks=False).from_string(HEADER_TEMPLATE)
    data = template.render({'args': args, 'devices': sorted(devices, key=lambda a: a.start), 'sorted': sorted,
        'memory': memory, 'physBase': paddr, 'kernel': kernel})
    fp.write(data)

def main(args):
    with open(args.schema) as fp:
        schema = yaml.load(fp)
    with open(args.config) as blob:
        kernel_devices = yaml.load(blob)
        validate(kernel_devices, schema)
        cfg = Config(kernel_devices)
    memory, user, kernel = set(), set(), set()
    fdt = pyfdt.pyfdt.FdtBlobParse(args.dtb).to_fdt()
    devices, by_phandle = find_devices(fdt, cfg)
    for d in devices.values():
        if d.is_memory():
            m, _ = d.regions(cfg, by_phandle) # second set is always empty for memory
            res = set()
            for e in m:
                res.update(set(e.remove_subregions(fdt.reserve_entries)))
            memory.update(res)
        else:
            (u, k) = d.regions(cfg, by_phandle)
            user.update(u)
            kernel.update(k)

    user = fixup_device_regions(user, 1 << args.page_bits, merge=True)
    kernel = fixup_device_regions(kernel, 1 << args.page_bits)
    output_regions(args, user, memory, kernel, args.output)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--dtb', help='device tree blob to use for generation', required=True, type=argparse.FileType())
    parser.add_argument('--output', help='output file for generated header', required=True, type=argparse.FileType('w'))
    parser.add_argument('--page-bits', help='number of bits per page', default=12, type=int)
    parser.add_argument('--phys-align', help='alignment in bits of the base address of the kernel', default=24, type=int)
    #parser.add_argument('--kernel-base', help='first address to use for kernel device mappings', type=lambda a: int(a, 0), required=True)
    parser.add_argument('--config', help='kernel device configuration', required=True)
    parser.add_argument('--schema', help='config file schema for validation', required=True)

    args = parser.parse_args()
    main(args)
