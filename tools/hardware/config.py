#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#


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

    def get_device_page_bits(self) -> int:
        ''' Get page size in bits for mapping devices for this arch '''
        return self.get_page_bits()


class ARMConfig(Config):
    ''' Config class for ARM '''
    SUPERSECTION_BITS = 24
    arch = 'arm'

    def get_kernel_phys_align(self) -> int:
        ''' on ARM the ELF loader expects to be able to map a supersection page to load the kernel. '''
        return self.SUPERSECTION_BITS


class RISCVConfig(Config):
    ''' Config class for RISCV '''
    MEGA_PAGE_SIZE = 0x200000
    arch = 'riscv'

    def get_bootloader_reserve(self) -> int:
        ''' on RISC-V the BBL is loaded at the start
        of physical memory. Mark it as unavailable. '''
        return self.MEGA_PAGE_SIZE

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
