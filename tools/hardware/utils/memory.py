#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from typing import List, Set

import hardware
from hardware.device import WrappedNode
from hardware.fdt import FdtParser
from hardware.memory import Region
from hardware.utils.rule import HardwareYaml, KernelRegionGroup


def merge_regions(regions: Set[Region]) -> Set[Region]:
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


def carve_out_region(regions: List[Region], reserved_reg: Region) -> Set[Region]:
    '''
    Returns a set of regions with the reserved area carved out. None of the
    regions in the returned set will overlap with the reserved area.
    '''
    ret_regions = set()

    for r in regions:
        reg_list = r.reserve(reserved_reg)
        ret_regions.update(reg_list)

    return ret_regions


def get_phys_mem_regions(tree: FdtParser, hw_yaml: HardwareYaml) \
        -> (List[Region], List[Region], int):
    '''
    Returns a list of regions representing physical memory as used by the kernel
    '''

    mem_regions = set()
    reserved_regions = set()

    # Get all regions with 'device_type = memory' in the tree.
    def visitor(node: WrappedNode):
        if node.has_prop('device_type') and node.get_prop('device_type').strings[0] == 'memory':
            # ToDo: Check if any regions are beyond the physical memory range
            #       we support (config.addrspace_max) and drop these ranges.
            mem_regions.update(node.get_regions())

    tree.visit(visitor)

    # Check device tree for reserved memory that we can't use.
    node = tree.get_path('/reserved-memory')
    if node:
        for child in node:
            if not child.has_prop('reg') or not child.has_prop('no-map'):
                continue
            tree_reserved_regs = child.get_regions()
            if tree_reserved_regs:
                reserved_regions.update(tree_reserved_regs)
                # carve out all reserved regions from the memory
                for r in tree_reserved_regs:
                    mem_regions = carve_out_region(mem_regions, r)

    # Check config for memory alignment and reservation needs. Needs to be done
    # after we have removed the reserved region from the device tree, because we
    # also get 'kernel_phys_base' here. And once we have that, the memory
    # regions can't the modified any longer.
    cfg_reserved_regs, kernel_phys_base = hw_yaml.config.align_memory(mem_regions)
    for r in cfg_reserved_regs:
        mem_regions = carve_out_region(mem_regions, r)
    reserved_regions.update(cfg_reserved_regs)

    # Merge adjacent regions and create a properly ordered list from the sets.
    mem_region_list = sorted(merge_regions(mem_regions),
                             key=lambda r: r.base)
    reserved_region_list = sorted(merge_regions(reserved_regions),
                                  key=lambda r: r.base)

    # Build the device regions by starting with a set with one region for the
    # whole address space and then remove know memory, reserved regions and
    # kernel devices. Since we can't create untypeds that exceed the
    # addrspace_max, we round down to the smallest untyped size alignment so
    # that the kernel will be able to turn the entire range into untypeds.
    all_regions = {
        Region(
            0,
            hardware.utils.align_down(
                2**hw_yaml.config.get_phys_addr_space_bits(),
                hw_yaml.config.get_smallest_kernel_object_alignment()))
    }

    for reg in mem_region_list + reserved_region_list:
        all_regions = carve_out_region(all_regions, reg)
    # if there are hardware rules, process them
    if hw_yaml:
        for dev in tree.get_kernel_devices():
            hw_regions = hw_yaml.get_rule(dev).get_regions(dev)
            for reg in hw_regions:
                if (type(reg) != KernelRegionGroup or not reg.user_ok):
                    all_regions = carve_out_region(all_regions, reg)

    # create a properly ordered list from the set or regions
    dev_region_list = sorted(all_regions, key=lambda r: r.base)

    return mem_region_list, dev_region_list, kernel_phys_base
