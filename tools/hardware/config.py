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

    def __init__(self, addrspace_max):
        self.addrspace_max = addrspace_max

    def get_kernel_phys_align(self) -> int:
        ''' Used to align the base of physical memory. Returns alignment size in bits. '''
        return 0

    def get_bootloader_reserve(self) -> int:
        ''' Used to reserve a fixed amount of memory for the bootloader. Offsets
            the kernel load address by the amount returned in bytes. '''
        return 0

    def get_page_bits(self) -> int:
        ''' Get page size in bits for this arch '''
        return 12  # 4096-byte pages

    def get_smallest_kernel_object_alignment(self) -> int:
        return 4  # seL4_MinUntypedBits is 4 for all configurations

    def get_device_page_bits(self) -> int:
        ''' Get page size in bits for mapping devices for this arch '''
        return self.get_page_bits()

    def align_memory(self, regions: Set[Region]) -> List[Region]:
        ''' Given a set of regions, sort them and align the first so that the
        ELF loader will be able to load the kernel into it. Will return the
        aligned memory region list, a set of any regions of memory that were
        aligned out and the physBase value that the kernel will use. memory
        region list, a set of any regions of memory that were aligned out and
        the physBase value that the kernel will use. '''
        pass


class ARMConfig(Config):
    ''' Config class for ARM '''
    SUPERSECTION_BITS = 24
    arch = 'arm'

    def get_kernel_phys_align(self) -> int:
        ''' on ARM the ELF loader expects to be able to map a supersection page to load the kernel. '''
        return self.SUPERSECTION_BITS

    def align_memory(self, regions: Set[Region]) -> List[Region]:
        ''' Arm wants physBase to be the physical load address of the kernel. '''
        ret = sorted(regions)
        extra_reserved = set()

        new = ret[0].align_base(self.get_kernel_phys_align())
        resv = Region(ret[0].base, new.base - ret[0].base, None)
        extra_reserved.add(resv)
        ret[0] = new

        physBase = ret[0].base

        return ret, extra_reserved, physBase


class RISCVConfig(Config):
    ''' Config class for RISCV '''
    MEGA_PAGE_SIZE = 0x200000
    arch = 'riscv'

    def get_bootloader_reserve(self) -> int:
        ''' on RISC-V OpenSBI is loaded at the start
        of physical memory. Mark it as unavailable. '''
        return self.MEGA_PAGE_SIZE

    def align_memory(self, regions: Set[Region]) -> List[Region]:
        ''' Currently the RISC-V port expects physBase to be the address that the
        bootloader is loaded at. To be generalised in the future. '''
        ret = sorted(regions)
        extra_reserved = set()

        physBase = ret[0].base

        resv = Region(ret[0].base, self.get_bootloader_reserve(), None)
        extra_reserved.add(resv)
        ret[0].base += self.get_bootloader_reserve()
        ret[0].size -= self.get_bootloader_reserve()

        return ret, extra_reserved, physBase

    def get_device_page_bits(self) -> int:
        ''' Get page size in bits for mapping devices for this arch '''
        if self.addrspace_max > (1 << 32):
            # rv39 and rv48 use 2MiB device pages
            return 21
        else:
            # rv32 uses 4MiB device pages
            return 22


def get_arch_config(arch: str, addrspace_max: int) -> Config:
    ''' Return an appropriate Config object for the given architecture '''
    if arch == 'arm':
        return ARMConfig(addrspace_max)
    elif arch == 'riscv':
        return RISCVConfig(addrspace_max)
    raise ValueError('Unsupported arch specified.')
