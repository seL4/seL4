#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from collections import defaultdict
from functools import lru_cache
from typing import Dict, List

import logging

from hardware.config import Config
from hardware.device import WrappedNode
from hardware.fdt import FdtParser
from hardware.memory import Region


def get_macro_str(macro: str) -> str:
    ''' Helper function that returns the appropriate C preprocessor line for a given macro '''
    if macro is None:
        return ''

    if macro[0] == '!':
        return '#ifndef ' + macro[1:]
    return '#ifdef ' + macro


def get_endif(macro: str) -> str:
    ''' Helper function that returns the appropriate endif line for a given macro '''
    if macro is None:
        return ''

    return '#endif /* {} */'.format(macro)


class KernelRegionGroup:
    ''' wraps a contiguous region of memory that is mapped into the kernel. '''

    def __init__(self, region: Region, kernel_name: str, page_bits: int, max_size: int, condition_macro: str = None, user_ok: bool = False):
        self.macro = condition_macro
        self.desc = region.owner.path if region.owner else 'dynamically generated region'
        self.kernel_offset = -1
        self.page_bits = page_bits
        self.labels = {}  # dict of label => offset within region.
        self.user_ok = user_ok

        region.size = min(max_size, region.size)
        aligned = region.align_size(page_bits)
        self.size = aligned.size
        self.base = aligned.base
        self.regions = aligned.make_chunks(1 << page_bits)
        self.labels[kernel_name] = region.base - aligned.base

    def has_macro(self):
        ''' True if this group has a macro '''
        return self.macro is not None

    def take_labels(self, other_group: 'KernelRegionGroup'):
        ''' Take another group's labels and add them to our own '''
        if self != other_group:
            raise ValueError('need to have equal size and base to take labels')
        for (k, v) in other_group.labels.items():
            self.labels[k] = v
        self.desc += ', ' + other_group.desc

    def get_macro(self):
        ''' Get the #ifdef line for this region group '''
        return get_macro_str(self.macro)

    def get_endif(self):
        ''' Get the #endif line for this region group '''
        return get_endif(self.macro)

    def set_kernel_offset(self, offset):
        ''' Set the base offset that this region is mapped at in the kernel.
            Returns the next free address in the kernel (i.e. base offset + region size) '''
        self.kernel_offset = offset
        return offset + self.size

    def get_labelled_addresses(self) -> Dict:
        ''' Get a dict of address -> label for the kernel '''
        ret = {}
        for (k, v) in self.labels.items():
            ret[v + self.kernel_offset] = k
        return ret

    def get_map_offset(self, reg):
        ''' Get the offset that the given region is mapped at. '''
        index = self.regions.index(reg)
        return self.kernel_offset + (index * (1 << self.page_bits))

    def get_desc(self):
        ''' Get this region group's description '''
        return self.desc

    def __repr__(self):
        return 'KernelRegion(reg={},labels={})'.format(self.regions, self.labels)

    def __eq__(self, other):
        return other.base == self.base and other.size == self.size


class KernelInterrupt:
    ''' Represents an interrupt that is used by the kernel. '''

    def __init__(self, label: str, irq: int, prio: int = 0, sel_macro: str = None, false_irq: int = -1, enable_macro: str = None, desc: str = None):
        self.label = label
        self.irq = irq
        self.prio = prio
        self.sel_macro = sel_macro
        self.false_irq = false_irq
        self.enable_macro = enable_macro
        self.desc = desc

    def get_enable_macro_str(self):
        ''' Get the enable macro #ifdef line '''
        return get_macro_str(self.enable_macro)

    def has_enable(self):
        ''' True if this interrupt has an enable macro '''
        return self.enable_macro is not None

    def get_enable_endif(self):
        ''' Get the enable macro #endif line '''
        return get_endif(self.enable_macro)

    def get_sel_macro_str(self):
        ''' Get the select macro #ifdef line '''
        return get_macro_str(self.sel_macro)

    def has_sel(self):
        ''' True if this interrupt has a select macro '''
        return self.sel_macro is not None

    def get_sel_endif(self):
        ''' Get the select macro #endif line '''
        return get_endif(self.sel_macro)

    def __repr__(self):
        return 'KernelInterrupt(label={},irq={},sel_macro={},false_irq={})'.format(self.label, self.irq, self.sel_macro, self.false_irq)


class DeviceRule:
    ''' Represents a single rule in hardware.yml '''

    def __init__(self, rule: dict, config: Config):
        self.rule = rule
        self.regions: Dict[int, Dict] = {}
        self.interrupts = rule.get('interrupts', {})
        self.config = config

        for reg in rule.get('regions', []):
            self.regions[reg['index']] = reg

    @lru_cache()
    def get_regions(self, node: WrappedNode) -> List[KernelRegionGroup]:
        ''' Returns a list of KernelRegionGroups that this rule specifies should be mapped into the kernel for this device. '''
        ret = []
        regions = node.get_regions()

        for (i, rule) in self.regions.items():
            if i >= len(regions):
                # XXX: skip this rule silently
                continue
            reg = regions[i]

            kernel_name = rule['kernel']
            user = rule.get('user', False)
            macro = rule.get('macro', None)
            max_size = 1 << self.config.get_device_page_bits()
            if 'kernel_size' in rule:
                max_size = rule['kernel_size']
            elif max_size < reg.size:
                logging.warning(
                    "Only mapping {}/{} bytes from node {}, region {}. Set kernel_size in YAML to silence.".format(max_size, reg.size, node.path, i))
            ret.append(KernelRegionGroup(reg, kernel_name,
                                         self.config.get_device_page_bits(), max_size, macro, user))

        return ret

    @lru_cache()
    def get_interrupts(self, tree: FdtParser, node: WrappedNode) -> List[KernelInterrupt]:
        ''' Returns a list of KernelInterrupts that this rule says are used by the kernel for this device. '''
        ret = []
        interrupts = node.get_interrupts(tree)

        for name, rule in self.interrupts.items():
            irq_desc = '{} generated from {}'.format(name, node.path)
            if type(rule) == dict:
                en_macro = rule.get('enable_macro', None)
                if rule['index'] >= len(interrupts):
                    # XXX: skip this rule silently.
                    continue
                defaultIrq = interrupts[rule['index']]
                sel_macro = rule.get('sel_macro', None)
                falseIrq = interrupts[rule['undef_index']] if 'undef_index' in rule else -1
                prio = rule.get('priority', 0)
                irq = KernelInterrupt(name, defaultIrq, prio, sel_macro,
                                      falseIrq, en_macro, desc=irq_desc)
            elif type(rule) == int:
                if rule >= len(interrupts):
                    # XXX: skip this rule silently.
                    continue
                irq = KernelInterrupt(name, interrupts[rule], desc=irq_desc)
            else:  # rule == 'boot-cpu'
                affinities = node.get_interrupt_affinities()
                boot_cpu = tree.get_boot_cpu()
                idx = affinities.index(boot_cpu)
                irq = KernelInterrupt(name, interrupts[idx])
            ret.append(irq)
        return ret


class HardwareYaml:
    ''' Represents the hardware configuration file '''

    def __init__(self, yaml: dict, config: Config):
        self.rules = {}
        for dev in yaml['devices']:
            rule = DeviceRule(dev, config)
            for compat in dev['compatible']:
                self.rules[compat] = rule

    def get_rule(self, device: WrappedNode) -> DeviceRule:
        ''' Returns the matching DeviceRule for this device. '''
        if not device.has_prop('compatible'):
            raise ValueError(
                'Not sure what to do with node {} with no compatible!'.format(device.path))

        for compat in device.get_prop('compatible').strings:
            if compat in self.rules:
                return self.rules[compat]

        raise ValueError('Failed to match compatibles "{}" for node {}!'.format(
            ', '.join(device.get_prop('compatible').strings), device.path))

    def get_matched_compatible(self, device: WrappedNode) -> str:
        ''' Returns the best matching compatible string for this device '''
        if not device.has_prop('compatible'):
            raise ValueError(
                'Not sure what to do with node {} with no compatible!'.format(device.path))
        for compat in device.get_prop('compatible').strings:
            if compat in self.rules:
                return compat
        return None
