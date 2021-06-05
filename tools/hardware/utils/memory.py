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


def carve_out_region(regions: Set[Region], reserved_reg: Region) -> Set[Region]:
    ''' Update regions to to carve out a reserved region '''
    updated_regions = set()

    for reg in regions:
        updated_regions.update(reg.reserve(reserved_reg))

    return updated_regions


def get_physical_memory(tree: FdtParser, config: Config) -> List[Region]:
    '''
    returns a list of regions representing physical memory as used by the kernel
    '''

    regions = get_memory_regions(tree)

    # find reserved memory, there might be none
    reserved_regions = set()
    node = tree.get_path('/reserved-memory')
    if node:
        for child in node:
            if child.has_prop('reg') and child.has_prop('no-map'):
                reserved_reg = child.get_regions()
                if reserved_reg:
                    reserved_regions.update(reserved_reg)
                    for r in reserved_reg:
                        regions = carve_out_region(regions, r)

    # ensure the regions are properly ordered
    regions = sorted(regions)

    # align the first region, which holds the kernel image
    kernel_phys_align = config.get_kernel_phys_align()
    if kernel_phys_align != 0:
        reg0 = regions[0]
        reg_new = reg0.align_base(kernel_phys_align)
        reserved_regions.add(Region(reg0.base, reg_new.base - reg0.base, None))
        regions[0] = reg_new

    return regions, reserved_regions


def get_addrspace_exclude(regions: List[Region], config: Config):
    '''
    Returns a list of regions that represents the inverse of the given region
    list.
    '''

    # initialize a set with one region for the whole address space
    regions_ret = {
        Region(
            0,
            utils.align_down(config.addrspace_max, config.get_page_bits()),
            None)
    }

    for reg in regions:
        if not (type(reg) == KernelRegionGroup and reg.user_ok):
            regions_ret = carve_out_region(regions_ret, reg)

    return sorted(regions_ret, key=lambda a: a.base)
