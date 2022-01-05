#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#
from typing import List, Set
from hardware.memory import Region


class Config:
    ''' Abstract config class '''
    arch = 'unknown'

    def __init__(self, sel4arch, addrspace_max):
        self.sel4arch = sel4arch
        self.addrspace_max = addrspace_max

    def get_page_bits(self) -> int:
        ''' Get page size in bits for this arch '''
        return 12  # 4096-byte pages

    def get_smallest_kernel_object_alignment(self) -> int:
        return 4  # seL4_MinUntypedBits is 4 for all configurations

    def get_device_page_bits(self) -> int:
        ''' Get page size in bits for mapping devices for this arch '''
        return self.get_page_bits()

    def align_memory(self, regions: Set[Region]) -> (List[Region], int):
        '''
        Given a set of regions, reserve memory to ensure proper alignment of the
        first region so that the ELF loader will be able to load the kernel into
        it. Will return a set of memory regions that are reserved (ie aligned
        out) and the kernel_phys_base value that the kernel will use (it's
        called 'phys_base' in the kernel headers). '''
        pass


class ARMConfig(Config):
    ''' Config class for ARM '''
    arch = 'arm'
    SUPERSECTION_BITS = 24  # 2^24 = 16 MiByte

    def align_memory(self, regions: Set[Region]) -> (List[Region], int):
        '''
        On ARM the kernel is put at the start of a supersection. The memory
        before that is marked as reserved and lost.
        '''
        region_list = sorted(regions, key=lambda r: r.base)
        reg0 = region_list[0]
        reg_aligned = reg0.align_base(self.SUPERSECTION_BITS)
        kernel_phys_base = reg_aligned.base
        reserved_list = {Region(reg0.base, reg_aligned.base - reg0.base)}
        return reserved_list, kernel_phys_base


class RISCVConfig(Config):
    ''' Config class for RISCV '''
    arch = 'riscv'
    MEGAPAGE_BITS_RV32 = 22  # 2^22 = 4 MiByte
    MEGAPAGE_BITS_RV64 = 21  # 2^21 = 2 MiByte

    def align_memory(self, regions: Set[Region]) -> (List[Region], int):
        '''
        The boot process on RISC-V is still a bit of a hack and rv32 seem to
        copy what rv64 does. The first memory region starts at a rv64 megapage
        boundary (2 MiByte). The common boot loader is OpenSBI, which reserves
        the fist 2 MiByte of the physical memory. It puts the ELF loader with
        the system image after itself (ie, an offset of 2 MiByte) and starts it
        from there. The kernel expects kernel_phys_base to be the address where
        the bootloader is and a 4 MiByte offset is added internally in the
        kernel headers. The ELF loader has to put the kernel as this 4 MiByte
        offset. This leaves 2 MiByte for the ELF loader and the system image,
        otherwise the offset must be increased. Since this boot flow is quite
        difficult to understand and also fragile, this needs to be reworked.
        '''
        region_list = sorted(regions, key=lambda r: r.base)
        reg0 = region_list[0]
        bootloader_reserved = 2 ** self.MEGAPAGE_BITS_RV64
        reg_aligned = reg0.align_base(self.MEGAPAGE_BITS_RV64)
        if reg_aligned.base != reg0.base:
            raise ValueError(
                '{} must be 2 Mibyte aligned'.format(reg0))
        if reg_aligned.size < bootloader_reserved:
            raise ValueError(
                '{} too small, need at lest {} bytes'.format(
                    reg0, bootloader_reserved))
        kernel_phys_base = reg_aligned.base
        reserved_list = {Region(reg_aligned.base, bootloader_reserved)}
        return reserved_list, kernel_phys_base

    def get_device_page_bits(self) -> int:
        ''' Get page size in bits for mapping devices for this arch '''
        if (self.sel4arch == 'riscv32'):
            # 4MiB device pages
            return self.MEGAPAGE_BITS_RV32
        elif (self.sel4arch == 'riscv64'):
            # 2MiB device pages for sv39 and sv48
            return self.MEGAPAGE_BITS_RV64
        raise ValueError('Unsupported sel4arch "{}" specified.'.format(self.sel4arch))


def get_arch_config(sel4arch: str, addrspace_max: int) -> Config:
    ''' Return an appropriate Config object for the given architecture '''
    if sel4arch in ['aarch32', 'aarch64', 'arm_hyp']:
        return ARMConfig(sel4arch, addrspace_max)
    elif sel4arch in ['riscv32', 'riscv64']:
        return RISCVConfig(sel4arch, addrspace_max)
    else:
        raise ValueError('Unsupported sel4arch "{}" specified.'.format(sel4arch))
