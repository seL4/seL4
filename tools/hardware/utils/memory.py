#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from typing import List, Set

import hardware.utils as utils

from hardware.config import Config
from hardware.device import WrappedNode
from hardware.fdt import FdtParser
from hardware.memory import Region
from hardware.utils.rule import KernelRegionGroup


def get_memory_regions(tree: FdtParser):
    ''' Get all regions with device_type = memory in the tree '''
    regions = set()

    def visitor(node: WrappedNode):
        if node.has_prop('device_type') and node.get_prop('device_type').strings[0] == 'memory':
            regions.update(node.get_regions())
    tree.visit(visitor)
    return regions


def parse_reserved_regions(node: WrappedNode) -> Set[Region]:
    ''' Parse a reserved-memory node, looking for regions that are
        unusable by OS (e.g. reserved for firmware/bootloader) '''
    if node is None:
        return set()

    ret = set()
    for child in node:
        if child.has_prop('reg') and child.has_prop('no-map'):
            ret.update(child.get_regions())
    return ret


def reserve_regions(regions: Set[Region], reserved: Set[Region]) -> Set[Region]:
    ''' Given a set of regions, and a set of reserved regions,
        return a new set that is the first set of regions minus the second set. '''
    ret = set(regions)

    while len(reserved) > 0:
        reserve = reserved.pop()
        new_ret = set()
        for el in ret:
            r = el.reserve(reserve)
            new_ret.update(r)
        ret = new_ret
    return ret


def align_memory(regions: Set[Region], config: Config) -> List[Region]:
    ''' Given a set of regions, sort them and align the first so that the ELF loader will be able to load the kernel into it. Will return the aligned
        memory region list, a set of any regions of memory that were aligned out
        and the physBase value that the kernel will use. '''
    ret = sorted(regions)
    extra_reserved = set()

    if config.arch == 'riscv':
        # RISC-V is special: it expects physBase to be
        # the address that the bootloader is loaded at.
        physBase = ret[0].base

    if config.get_bootloader_reserve() > 0:
        resv = Region(ret[0].base, config.get_bootloader_reserve(), None)
        extra_reserved.add(resv)
        ret[0].base += config.get_bootloader_reserve()
        ret[0].size -= config.get_bootloader_reserve()

    if config.get_kernel_phys_align() != 0:
        new = ret[0].align_base(config.get_kernel_phys_align())
        resv = Region(ret[0].base, new.base - ret[0].base, None)
        extra_reserved.add(resv)
        ret[0] = new

    if config.arch != 'riscv':
        # ARM (and presumably other architectures)
        # want physBase to be the physical load address of the kernel.
        physBase = ret[0].base
    return ret, extra_reserved, physBase


def get_physical_memory(tree: FdtParser, config: Config) -> List[Region]:
    ''' returns a list of regions representing physical memory as used by the kernel '''
    regions = get_memory_regions(tree)
    reserved = parse_reserved_regions(tree.get_path('/reserved-memory'))
    regions = reserve_regions(regions, reserved)
    regions, extra_reserved, physBase = align_memory(regions, config)

    return regions, reserved.union(extra_reserved), physBase


def get_addrspace_exclude(regions: List[Region], config: Config):
    ''' Returns a list of regions that represents the inverse of the given region list. '''
    ret = set()
    as_max = utils.align_down(config.addrspace_max, config.get_page_bits())
    ret.add(Region(0, as_max, None))

    for reg in regions:
        if type(reg) == KernelRegionGroup:
            if reg.user_ok:
                continue
        new_ret = set()
        for el in ret:
            new_ret.update(el.reserve(reg))

        ret = new_ret
    return sorted(ret, key=lambda a: a.base)
