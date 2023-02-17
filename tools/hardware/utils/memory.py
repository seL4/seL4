#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from __future__ import annotations
import hardware
from hardware.device import WrappedNode
from hardware.fdt import FdtParser
from hardware.memory import Region
from hardware.utils.rule import HardwareYaml, KernelRegionGroup

# "annotations" exists in __future__ since 3.7.0b1, but even in 3.10 the
# decision to make it mandatory has been postponed.
import sys
assert sys.version_info >= (3, 7)

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


def carve_out_region(regions: Set[Region], reserved_reg: Region) -> Set[Region]:
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

    # Using a set instead of a list as the advantage that duplicate regions are
    # ignored automatically when adding them
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

    # Merge adjacent regions and create a properly ordered list from the
    # available memory regions set
    mem_region_list = sorted(merge_regions(mem_regions),
                             key=lambda r: r.base)

    # Check config for memory alignment and reservation needs. Needs to be done
    # after we have removed the reserved region from the device tree, because we
    # also get 'kernel_phys_base' here. And once we have that, the memory
    # regions can't the modified any longer.
    # By convention, the first region holds the kernel image. But there is no
    # strict requirement that it must be in the first region, any region would
    # do. An exception is thrown if the region is too small to satisfy the
    # kernel's alignment requirement. Currently there is no solution for this,
    # so this must be solved by the first platform that runs into this problem.
    # During the boot process, the kernel will mark its own memory as reserved,
    # so there is no need to split the region here in a part before the kernel
    # and a part that has the kernel in the beginning.
    # Unfortunately, we do not know the kernel size here, so there is no
    # guarantee the kernel really fits into that region. For now, we leave this
    # issue to be resolved by the boot flow. Loading the ELF loader will fail
    # anyway if there is not enough space.
    kernel_phys_align = hw_yaml.config.get_kernel_phys_align()
    if kernel_phys_align != 0:
        # Align the first so that the ELF loader will be able to load the kernel
        # into it. Will return the aligned memory region list, a set of any
        # regions of memory that were
        new_first_reg = mem_region_list[0].align_base(kernel_phys_align)
        reserved_regions.add(
            Region(mem_region_list[0].base,
                   new_first_reg.base - mem_region_list[0].base))
        mem_region_list[0] = new_first_reg

    kernel_phys_base = mem_region_list[0].base

    # Merge adjacent regions and create a properly ordered list from the
    # reserved memory regions set
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
                hw_yaml.config.addrspace_max,
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

    return mem_region_list, reserved_region_list, dev_region_list, kernel_phys_base
