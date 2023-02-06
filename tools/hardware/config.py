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
    arch = 'arm'
    SUPERSECTION_BITS = 24  # 2^24 = 16 MiByte

    def get_kernel_phys_align(self) -> int:
        ''' on ARM the ELF loader expects to be able to map a supersection page to load the kernel. '''
        return self.SUPERSECTION_BITS

    def align_memory(self, regions: Set[Region]) -> List[Region]:
        ''' Arm wants physBase to be the physical load address of the kernel. '''
        ret = sorted(regions)
        extra_reserved = set()

        new = ret[0].align_base(self.get_kernel_phys_align())
        resv = Region(ret[0].base, new.base - ret[0].base)
        extra_reserved.add(resv)
        ret[0] = new

        physBase = ret[0].base

        return ret, extra_reserved, physBase


class RISCVConfig(Config):
    ''' Config class for RISCV '''
    arch = 'riscv'
    MEGAPAGE_BITS_RV32 = 22  # 2^22 = 4 MiByte
    MEGAPAGE_BITS_RV64 = 21  # 2^21 = 2 MiByte
    MEGA_PAGE_SIZE_RV64 = 2**MEGAPAGE_BITS_RV64

    def get_bootloader_reserve(self) -> int:
        ''' OpenSBI reserved the first 2 MiByte of physical memory on rv64,
        which is exactly a megapage. For rv32 we use the same value for now, as
        this seems to work nicely - even if this is just half of the 4 MiByte
        magepages that exist there. '''
        return self.MEGA_PAGE_SIZE_RV64

    def align_memory(self, regions: Set[Region]) -> List[Region]:
        ''' Currently the RISC-V port expects physBase to be the address that the
        bootloader is loaded at. To be generalised in the future. '''
        ret = sorted(regions)
        extra_reserved = set()

        physBase = ret[0].base

        resv = Region(ret[0].base, self.get_bootloader_reserve())
        extra_reserved.add(resv)
        ret[0].base += self.get_bootloader_reserve()
        ret[0].size -= self.get_bootloader_reserve()

        return ret, extra_reserved, physBase

    def get_device_page_bits(self) -> int:
        ''' Get page size in bits for mapping devices for this arch '''
        if (self.sel4arch == 'riscv32'):
            # 4MiB device pages
            return self.MEGAPAGE_BITS_RV32
        elif (self.sel4arch == 'riscv64'):
            # 2MiB device pages for sv39 and sv48
            return self.MEGAPAGE_BITS_RV64
        raise ValueError('Unsupported sel4arch "{}" specified.'.format(self.sel4arch))

class LoongarchConfig(Config):
    ''' Config class for Loongarch '''
    arch = 'loongarch'
    # RESERVED_SPACE = 2**26  # 2^26= 64 MiByte
    RESERVED_SPACE = 0

    def get_bootloader_reserve(self) -> int:
        return self.RESERVED_SPACE

    def align_memory(self, regions: Set[Region]) -> List[Region]:
        ''' loongarch wants physBase to be the physical load address of the kernel. '''
        ret = sorted(regions)
        extra_reserved = set()

        physBase = ret[0].base

        resv = Region(ret[0].base, self.get_bootloader_reserve())
        extra_reserved.add(resv)
        ret[0].base += self.get_bootloader_reserve()
        ret[0].size -= self.get_bootloader_reserve()
        
        return ret, extra_reserved, physBase   


def get_arch_config(sel4arch: str, addrspace_max: int) -> Config:
    ''' Return an appropriate Config object for the given architecture '''
    if sel4arch in ['aarch32', 'aarch64', 'arm_hyp']:
        return ARMConfig(sel4arch, addrspace_max)
    elif sel4arch in ['riscv32', 'riscv64']:
        return RISCVConfig(sel4arch, addrspace_max)
    elif sel4arch in ['loongarch64']:
        return LoongarchConfig(sel4arch, addrspace_max)
    else:
        raise ValueError('Unsupported sel4arch "{}" specified.'.format(sel4arch))
