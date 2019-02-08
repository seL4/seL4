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
import functools
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

def parse_arm_gic_irq(self, child, by_phandle, data):
    # 3 cells:
    # first cell is 1 if PPI, 0 if SPI
    # second cell: PPI or SPI number
    # third cell: interrupt trigger flags, ignored by us
    is_spi = data.pop(0) == 0
    number = data.pop(0)
    flags = data.pop(0)

    number += 16 # SGI takes 0-15
    if is_spi:
        number += 16 # PPI is 16-31

    return number

def parse_raw_irq(self, child, by_phandle, data):
    # first cells is interrupt number, may have flags or other data
    # we don't need afterwards.
    num = data.pop(0)
    cells = self.get_interrupt_cells(by_phandle)
    while cells > 1:
        data.pop(0)
        cells -= 1
    return num

def parse_bcm2836_armctrl_irq(self, child, by_phandle, data):
    # two cells, first is bank, second is number within bank
    bank = data.pop(0)
    num = data.pop(0)

    bank += 1
    return (32 * bank) + num

def parse_passthrough_irq(self, child, by_phandle, data):
    # IRQ just passes through to this node's interrupt-parent.
    parent = self.get_interrupt_parent(by_phandle)

    return parent.parse_interrupt(child, by_phandle, data)

interrupt_controllers = {
    'arm,cortex-a15-gic': parse_arm_gic_irq,
    'arm,cortex-a7-gic': parse_arm_gic_irq,
    'arm,cortex-a9-gic': parse_arm_gic_irq,
    'arm,gic-400': parse_arm_gic_irq,
    'arm,gic-v3': parse_arm_gic_irq,
    'brcm,bcm2836-l1-intc': parse_raw_irq,
    'brcm,bcm2836-armctrl-ic': parse_bcm2836_armctrl_irq, # maybe not actually needed?
    'fsl,avic': parse_raw_irq,
    'fsl,imx6q-gpc': parse_passthrough_irq,
    'fsl,imx7d-gpc': parse_passthrough_irq,
    'nvidia,tegra124-ictlr': parse_passthrough_irq,
    'qcom,msm-qgic2': parse_arm_gic_irq,
    'ti,am33xx-intc': parse_raw_irq,
    'ti,omap3-intc': parse_raw_irq,
}

class Interrupt:
    def __init__(self, num, name, macro=None, prio=0):
        self.num = num
        self.name = name
        self.macro = macro
        self.priority = prio
        self.desc = ""

    def get_macro_name(self):
        return self.macro[1:] if self.macro[0] == '!' else self.macro

    def get_macro_conditional(self):
        return 'ifdef' if self.macro[0] != '!' else 'ifndef'

    def __hash__(self):
        return hash(self.num)

    def __str__(self):
        return 'Interrupt(num=%d,name=%s,macro=%s)'%(self.num, self.name, self.macro)

    def __repr__(self):
        return str(self)

@functools.total_ordering
class Offset:
    def __init__(self, name, offset, macro=""):
        self.name = name
        self.offset = offset
        self.macro = macro
        self.base = -1

    def __eq__(self, other):
        return self.base == other.base and self.offset == other.offset

    def __ne__(self, other):
        # This is needed on Python 2, but on Python 3 this is implicit.
        return not self.__eq__(other)

    def __gt__(self, other):
        return (self.base + self.offset) > (other.base + other.offset)

class Region:
    def __init__(self, start, size, name, index=0, kaddr=0):
        self.start = start
        self.size = size

        self.macro_string = None
        if isinstance(name, Region):
            other = name
            self.names = set(other.names)
            self.index = other.index
            self.kaddr = other.kaddr
            self.kernel_size = other.kernel_size
            self.kernel_var = other.kernel_var[:]
            self.var_names = other.var_names[:]
        else:
            self.names = set()
            self.names.add(name)
            self.index = index
            self.kaddr = kaddr
            self.kernel_size = 0
            self.kernel_var = []
            self.var_names = []
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

    def get_macro_string(self, invert=False):
        ret = ''
        if self.macro_string:
            return self.macro_string
        for v in self.kernel_var:
            if v.macro:
                ret += 'defined(' + v.macro + ') ||'
            else:
                # if any region has no macro, we want to do this unconditionally
                return ''
        if len(ret) > 0:
            ret = ret[:-3]
            if invert:
                ret = '!(' + ret + ')'
            ret = '#if ' + ret

        return ret

    def get_macro_end(self):
        if len(self.get_macro_string()) > 0:
            return '#endif'
        return ''

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

    def get_interrupt_cells(self, by_phandle):
        if '#interrupt-cells' in self.props:
            return self.props['#interrupt-cells'].words[0]
        parent = self.get_interrupt_parent(by_phandle)
        return parent.get_interrupt_cells(by_phandle)

    def get_interrupt_parent(self, by_phandle):
        if 'interrupt-parent' not in self.props:
            return self.parent.get_interrupt_parent(by_phandle)
        phandle = self.props['interrupt-parent'].words[0]
        return by_phandle[phandle]

    def get_interrupts(self, config, by_phandle):
        irqs = []
        if 'compatible' not in self.props:
            return set()
        if 'interrupts' in self.props:
            interrupt_parent = self.get_interrupt_parent(by_phandle)
            data = list(self.props['interrupts'].words)
            while len(data) > 0:
                irqs.append(interrupt_parent.parse_interrupt(self, by_phandle, data))
        elif 'interrupts-extended' in self.props:
            data = list(self.props['interrupts-extended'].words)
            while len(data) > 0:
                phandle = data.pop(0)
                interrupt_parent = by_phandle[phandle]
                irqs.append(interrupt_parent.parse_interrupt(self, by_phandle, data))
        if len(irqs) > 0:
            affinities = []
            if 'interrupt-affinity' in self.props:
                affinities = list(self.props['interrupt-affinity'].words)
            return set(config.get_irqs(self, irqs, affinities, by_phandle))
        return set()

    def _recursive_get_addr_cells(self):
        if '#address-cells' in self:
            return self['#address-cells'].words[0]
        return self.parent._recursive_get_addr_cells()

    def _parse_interrupt_nexus(self, child, by_phandle, data):
        nexus_data = list(self.props['interrupt-map'].words)

        # interrupt-map is a list of the following:
        # <<child unit address> <child interrupt specifier> <interrupt parent>
        #  <parent unit address> <parent interrupt specifier>>

        # "child unit address" seems to be special: the docs say one thing, but
        # Linux implements something else. We go with the Linux implementation here:
        # child unit address size is specified by '#address-cells' in the nexus node,
        # or the first '#address-cells' specified in a parent node. (note: not interrupt parent)
        chaddr_cells = self._recursive_get_addr_cells()
        chint_cells = self.props['#interrupt-cells'].words[0]

        # we only care about the first address, like Linux.
        addr = make_number(chaddr_cells, list(child['reg'].words)) if 'reg' in child else 0
        addr_mask = (1 << (32 * chaddr_cells)) - 1
        intspec = make_number(chint_cells, data)
        int_mask = (1 << (32 * chint_cells)) - 1
        if 'interrupt-map-mask' in self.props:
            masks = list(self.props['interrupt-map-mask'].words)
            addr_mask = make_number(chaddr_cells, masks)
            int_mask = make_number(chint_cells, masks)

        addr &= addr_mask
        intspec &= int_mask

        # next: figure out which interrupt it is that we're interested in.
        ok = False
        while len(nexus_data) > 0:
            my_addr = make_number(chaddr_cells, nexus_data) & addr_mask
            my_intspec = make_number(chint_cells, nexus_data) & int_mask
            node = by_phandle[nexus_data.pop(0)]
            # found it!
            if my_addr == addr and my_intspec == intspec:
                ok = True
                break

            # not yet: keep going. get address-cells and interrupt-cells so we know how much to skip
            cells = node['#address-cells'].words[0] if '#address-cells' in node else 0
            cells += node['#interrupt-cells'].words[0]
            if cells > len(nexus_data):
                logging.warning("malformed device tree!")
                return -1
            # slice off specifier for this entry
            nexus_data = nexus_data[cells:]

        if not ok:
            logging.warning("could not find match in interrupt nexus :(")
            return -1

        # okay. now we figure out what the number is
        return node.parse_interrupt(child, by_phandle, nexus_data)

    def parse_interrupt(self, child, by_phandle, data):
        if 'interrupt-map' in self.props:
            # this is an "interrupt nexus". Actual interrupt numbers are determined elsewhere,
            # we just need to decode the interrupt info and pass it off.
            return self._parse_interrupt_nexus(child, by_phandle, data)

        if 'compatible' not in self.props:
            if '#interrupt-cells' in self.props:
                for i in range(self.props['#interrupt-cells'].words[0]):
                    data.pop(0)
            else:
                data.pop(0)
            return -1

        for compat in self.props['compatible'].strings:
            if compat in interrupt_controllers:
                return interrupt_controllers[compat](self, child, by_phandle, data)

        logging.info('Unknown interrupt controller: "%s"'%'", "'.join(self.props['compatible'].strings))
        if '#interrupt-cells' in self.props:
            for i in range(self.props['#interrupt-cells'].words[0]):
                data.pop(0)
        else:
            data.pop(0)
        return -1

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
            # for PCI, skip the high cell (per of_bus_pci_map in linux/drivers/of/address.c).
            if 'device_type' in self and self['device_type'].strings[0] == 'pci':
                addr &= (1 << (4 * child_addr_cells)) - 1
                data.pop(0)
                cbase = make_number(child_addr_cells - 1, data)
            else:
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
            region.kernel_var.append(Offset(rule['kernel'], 0, rule.get('macro', None)))
            region.executeNever = rule['executeNever']
            region.kernel_size = rule.get('kernel_size', 0)
            region.user_macro = (not rule.get('user', False)) and 'macro' in rule
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

    def get_irqs(self, device, irqs, affinities, by_phandle):
        ret = set()
        compat = device['compatible']
        for compatible in compat.strings:
            if compatible not in self.devices:
                continue

            for rule in self.devices[compatible]:
                if 'interrupts' not in rule or not self._is_chosen(device, rule, by_phandle):
                    continue
                for irq in rule['interrupts']:
                    irq_rule = rule['interrupts'][irq]
                    if type(irq_rule) is dict:
                        idx = irq_rule['index']
                        macro = irq_rule.get('macro', None)
                        prio = irq_rule.get('priority', 0)
                    else:
                        idx = irq_rule
                        macro = None
                        prio = 0
                    if idx == 'boot-cpu':
                        if self.chosen is not None and 'seL4,boot-cpu' in self.chosen:
                            bootcpu = self.chosen['seL4,boot-cpu'].words[0]
                            if bootcpu in affinities:
                                num = affinities.index(bootcpu)
                            else:
                                # skip this rule - no matching IRQ.
                                continue
                        else:
                            num = 0
                    elif type(idx) is dict:
                        res = Interrupt(irqs[idx['defined']], irq, idx['macro'], prio)
                        res.desc = "%s '%s' IRQ %d"%(device.name, compatible, idx['defined'])
                        ret.add(res)

                        res = Interrupt(irqs[idx['undefined']], irq, '!' + idx['macro'], prio)
                        res.desc = "%s '%s' IRQ %d"%(device.name, compatible, idx['undefined'])
                        ret.add(res)
                        continue
                    elif type(irq_rule) is int:
                        num = irq_rule
                    else:
                        num = idx
                    if num < len(irqs):
                        irq = Interrupt(irqs[num], irq, macro if macro != 'all' else None, prio)
                        irq.desc = "%s '%s' IRQ %d"%(device.name, compatible, num)
                        ret.add(irq)
            if len(ret) > 0:
                break
        return ret

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

def should_parse_regions(root, node):
    """ returns True if we should parse regions found in this node. """
    parent = node.get_parent_node()

    if parent == root:
        return True

    try:
        # a ranges property indicates that children of the parent node
        # are accessible by the grandparent node. If this property holds
        # all the way up the tree, children of the parent will be addressable
        # by the CPU.
        idx = parent.index('ranges')
        return should_parse_regions(root, parent)
    except ValueError:
        return False

def find_devices(dtb, cfg):
    devices = {}
    nodes = {}
    by_phandle = {}
    chosen = None
    aliases = None
    root = dtb.get_rootnode()
    devices[root.name] = Device(root, None, '/')
    nodes[root.name] = devices[root.name]
    for child in root.walk(): # walk every node in the whole tree
        name = child[0]
        child = child[1]
        if not isinstance(child, pyfdt.pyfdt.FdtNode):
            continue
        if name == '/chosen':
            cfg.set_chosen(Device(child, None, name))
        elif name == '/aliases':
            cfg.set_aliases(Device(child, None, name))
        if should_parse_regions(root, child):
            devices[child.name] = Device(child, devices[child.get_parent_node().name], name)
            nodes[child.name] = devices[child.name]
        else:
            nodes[child.name] = Device(child, nodes[child.get_parent_node().name], name)

        try:
            idx = child.index('phandle')
        except ValueError:
            continue

        prop = child[idx]
        by_phandle[prop.words[0]] = nodes[child.name]

    # the root node is not actually a device, so remove it.
    del devices[root.name]
    return devices, by_phandle

def fixup_device_regions(regions, pagesz, merge=False):
    """ page align all regions and check for overlapping regions """
    ret = list()
    # first, make sure all regions are page aligned
    for r in regions:
        new_start = align_down(r.start, pagesz)
        new_size = align_up(r.size, pagesz)
        for v in r.kernel_var:
            v.offset += r.start - new_start
        r.start = new_start
        r.size = new_size
        # we abuse the fact that regions will be
        # "equal" if they have different names but the same range.
        if r in ret:
            idx = ret.index(r)
            ret[idx].names.update(r.names)
            ret[idx].kernel_var += r.kernel_var
        else:
            ret.append(r)

    # combine overlapping regions
    if merge:
        ret = sorted(ret, key=lambda a: a.start)
        i = 1
        while i < len(ret):
            # don't combine regions which are conditionally exposed to userspace
            # as we might end up either (a) losing the condition and exposing them
            # or (b) hiding other regions that shouldn't be hidden
            # FIXME: this will break some proof invariants if conditional regions overlap
            # with unconditional regions. We don't handle this case for now.
            if (ret[i].user_macro and ret[i].get_macro_string()) or \
                (ret[i-1].user_macro and ret[i-1].get_macro_string()):
                i += 1
                continue
            # check if this region overlaps with the previous region.
            # regions are ordered by start address, so ret[i-1].start <= ret[i].start is always true.
            if ret[i].start <= ret[i-1].start + ret[i-1].size:
                # figure out how much overlap there is
                overlap = (ret[i-1].start + ret[i-1].size) - ret[i].start
                # if the region is bigger than the overlap,
                # then we need to extend the previous region.
                # Otherwise, we can just leave the previous region
                # as-is.
                if ret[i].size > overlap:
                    ret[i-1].size += ret[i].size - overlap
                ret[i-1].names.update(ret[i].names)
                # The current region is now covered by the previous region,
                # so we can safely delete it.
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
#include <linker.h>
#ifndef KDEV_PPTR
#include <mode/hardware.h>
#endif
#define physBase        {{ "0x%x" % physBase }}
{% for var in sorted(macros.values()) -%}
{%- if var.macro %}
#ifdef {{ var.macro }}
{%- endif %}
#define {{ var.name }} (KDEV_PPTR + {{ "0x%x" % (var.base + var.offset) }})
{%- if var.macro %}
#endif
{%- endif %}
{%- endfor %}

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
{% for reg in kernel %}{{ reg.get_macro_string() }}
    { /* {{ ' '.join(sorted(reg.names)) }} */
      {{ "0x%x" % reg.start }},
      {% if reg.kaddr in macros -%}
      {{ macros[reg.kaddr].name }},
      {%- else -%}
      /* region contains {{ ', '.join(reg.var_names) }} */
      {{ "KDEV_PPTR + 0x%x" % reg.kaddr }},
      {%- endif %}
      {{ "true" if reg.executeNever else "false" }} /* armExecuteNever */
    },
{% if reg.get_macro_end() -%}
{{ reg.get_macro_end()}}
{% endif -%}
{% endfor %}
};

static const p_region_t BOOT_RODATA avail_p_regs[] = {
    {%- for reg in memory %}
    { {{ "0x%x" % reg.start }}, {{ "0x%x" % (reg.start + reg.size) }} }, /* {{ ' '.join(sorted(reg.names)) }} */
    {%- endfor %}
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    {%- for reg in devices %}
    {%- if reg.user_macro %}
{{ reg.get_macro_string(True) }}
    {%- endif %}
    { {{ "0x%x" % reg.start }}, {{ "0x%x" % (reg.start + reg.size) }} }, /* {{ ' '.join(sorted(reg.names)) }} */
    {%- if reg.user_macro %}
#endif /* {{ reg.get_macro_string(True)[len('#if '):] }} */
    {%- endif %}
    {%- endfor %}
};

/* INTERRUPTS */
{%- for irq in interrupts %}
    {%- if irq.macro %}
#{{ irq.get_macro_conditional() }} {{ irq.get_macro_name() }}
    {%- endif %}
    {%- if irq.num == -1 %}
/*#define {{ irq.name }} {{ irq.num }} {{ irq.desc }} */
    {%- else %}
#define {{ irq.name }} {{ irq.num }} /* {{ irq.desc }} */
    {%- endif %}
    {%- if irq.macro %}
#endif
    {%- endif %}
{%- endfor %}

#endif /* __PLAT_DEVICES_GEN_H */
"""

def output_regions(args, devices, memory, kernel, irqs, fp):
    """ generate the device list for the C header file """
    memory = sorted(memory, key=lambda a: a.start)
    kernel = sorted(kernel, key=lambda a: a.start)
    extra_kernel = []
    addr = 0
    macros = {}
    for reg in kernel:
        reg.kaddr = addr
        addr += 1 << args.page_bits
        if reg.size > (1 << args.page_bits) and reg.kernel_size == 0x0:
            # print out a warning if the region has more than one page and max size is not set
            logging.warning('Only mapping 0x%x bytes of 0x%x for kernel region at 0x%x (%s), set "kernel_size" in YAML to silence'%
                    (1 << args.page_bits, reg.size, reg.start, ' '.join(sorted(reg.names))))

        size = 1 << args.page_bits
        while size < reg.kernel_size and size < reg.size:
            extra_kernel.append(Region(reg.start + size, 1 << args.page_bits,
                'above region continued...', kaddr=addr))
            addr += 1 << args.page_bits
            size += 1 << args.page_bits

        for var in reg.kernel_var:
            var.base = reg.kaddr
            if var.base + var.offset in macros:
                logging.warning('Multiple kernel device macros with the same address 0x%x (%s, %s), ignoring %s.'%
                    (var.base + var.offset, var.name, macros[var.base + var.offset].name, var.name))
                continue
            macros[var.base + var.offset] = var
        reg.var_names = sorted(i.name for i in reg.kernel_var)

    kernel += extra_kernel
    kernel.sort(key=lambda a: a.start)
    # make sure physBase is at least 16MB (supersection) aligned for the ELF loader's sake.
    # TODO: this may need to be larger for aarch64. It seems to work OK on currently supported platforms though.
    paddr = align_up(memory[0].start, 1 << args.phys_align)
    memory[0].size -= paddr - memory[0].start
    memory[0].start = paddr

    template = Environment(loader=BaseLoader, trim_blocks=False, lstrip_blocks=False).from_string(HEADER_TEMPLATE)
    data = template.render(dict(
        __builtins__.__dict__,
        **{
            'args': args,
            'devices': sorted(devices, key=lambda a: a.start),
            'memory': memory,
            'physBase': paddr,
            'kernel': kernel,
            'interrupts': irqs,
            'macros': macros
        }))
    fp.write(data)

def main(args):
    schema = yaml.load(args.schema)
    kernel_devices = yaml.load(args.config)
    validate(kernel_devices, schema)
    cfg = Config(kernel_devices)

    memory, user, kernel = set(), set(), set()
    fdt = pyfdt.pyfdt.FdtBlobParse(args.dtb).to_fdt()
    devices, by_phandle = find_devices(fdt, cfg)
    kernel_irqs = set()
    for d in devices.values():
        kernel_irqs.update(d.get_interrupts(cfg, by_phandle))
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

    irq_dict = {}
    for el in kernel_irqs:
        if el.name in irq_dict:
            irq_dict[el.name].append(el)
        else:
            irq_dict[el.name] = [el]

    kernel_irqs = []
    for el in irq_dict:

        irq_dict[el].sort(key=lambda a: a.priority, reverse=True)
        max_prio = irq_dict[el][0].priority
        for irq in irq_dict[el]:
            if irq.priority != max_prio:
                break
            kernel_irqs.append(irq)

    user = fixup_device_regions(user, 1 << args.page_bits, merge=True)
    kernel = fixup_device_regions(kernel, 1 << args.page_bits)
    output_regions(args, user, memory, kernel, kernel_irqs, args.output)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--dtb', help='device tree blob to use for generation', required=True, type=argparse.FileType('rb'))
    parser.add_argument('--output', help='output file for generated header', required=True, type=argparse.FileType('w'))
    parser.add_argument('--page-bits', help='number of bits per page', default=12, type=int)
    parser.add_argument('--phys-align', help='alignment in bits of the base address of the kernel', default=24, type=int)
    parser.add_argument('--config', help='kernel device configuration', required=True, type=argparse.FileType())
    parser.add_argument('--schema', help='config file schema for validation', required=True, type=argparse.FileType())

    args = parser.parse_args()
    main(args)
