#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from typing import List, Set

import hardware
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


def merge_memory_regions(regions: Set[Region]) -> Set[Region]:
    ''' Check all region and merge adjacent ones '''
    all_regions = [dict(idx=idx, region=region, right_adj=None, left_adj=None)
                   for (idx, region) in enumerate(regions)]

    # Find all right contiguous regions
    for dreg in all_regions:
        for dnreg in all_regions[dreg['idx']+1:]:
            if dreg['region'].owner == dnreg['region'].owner:
                if dnreg['region'].base == dreg['region'].base + dreg['region'].size:
                    dreg['right_adj'] = dnreg
                    dnreg['left_adj'] = dreg
                elif dreg['region'].base == dnreg['region'].base + dnreg['region'].size:
                    dnreg['right_adj'] = dreg
                    dreg['left_adj'] = dnreg

    # Find all the left-most contiguous regions
    contiguous_regions = set()
    for reg in all_regions:
        if reg['left_adj'] is None:
            size = reg['region'].size
            r_adj = reg['right_adj']
            while r_adj is not None:
                size += r_adj['region'].size
                r_adj = r_adj['right_adj']
            contiguous_regions.add(Region(reg['region'].base, size, reg['region'].owner))

    return contiguous_regions


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


def get_physical_memory(tree: FdtParser, config: Config) -> List[Region]:
    ''' returns a list of regions representing physical memory as used by the kernel '''
    regions = merge_memory_regions(get_memory_regions(tree))
    reserved = parse_reserved_regions(tree.get_path('/reserved-memory'))
    regions = reserve_regions(regions, reserved)
    regions, extra_reserved, physBase = config.align_memory(regions)

    return regions, reserved.union(extra_reserved), physBase


def get_addrspace_exclude(regions: List[Region], config: Config):
    ''' Returns a list of regions that represents the inverse of the given region list. '''
    ret = set()
    # We can't create untypeds that exceed the addrspace_max, so we round down to the smallest
    # untyped size alignment so that the kernel will be able to turn the entire range into untypeds.
    as_max = hardware.utils.align_down(config.addrspace_max,
                                       config.get_smallest_kernel_object_alignment())
    ret.add(Region(0, as_max))

    for reg in regions:
        if type(reg) == KernelRegionGroup:
            if reg.user_ok:
                continue
        new_ret = set()
        for el in ret:
            new_ret.update(el.reserve(reg))

        ret = new_ret
    return sorted(ret, key=lambda a: a.base)
