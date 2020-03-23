/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>

#include <mode/hardware.h>

/* Privileged CSR definitions */
#define SSTATUS_SPIE  0x00000020
#define SSTATUS_SPP   0x00000100

#define SATP_MODE_OFF  0
#define SATP_MODE_SV32 1
#define SATP_MODE_SV39 8
#define SATP_MODE_SV48 9

#ifndef __ASSEMBLER__

#include <config.h>
#include <linker.h>

#include <arch/types.h>
#include <sel4/sel4_arch/constants.h>

/* The size is for HiFive Unleashed */
#define L1_CACHE_LINE_SIZE_BITS     6
#define L1_CACHE_LINE_SIZE          BIT(L1_CACHE_LINE_SIZE_BITS)

/* The highest valid physical address that can be indexed in the kernel window */
#define PADDR_TOP (KERNEL_BASE - PPTR_BASE + PADDR_BASE)
/* A contiguous region of physical address space at PADDR_LOAD is mapped
 * to KERNEL_ELF_BASE, and the size of this region is KDEV_BASE-KERNEL_ELF_BASE.
 * PADDR_HIGH_TOP is the end of this physical address region. */
#define PADDR_HIGH_TOP (KDEV_BASE - KERNEL_ELF_BASE + PADDR_LOAD)

/* Translates from a physical address and a value in the kernel image */
#define KERNEL_BASE_OFFSET (KERNEL_ELF_BASE - PADDR_LOAD)

/* Convert our values into general values expected by the common code */
#define kernelBase KERNEL_BASE
/* This is the top of the kernel window, not including the kernel image */
#define PPTR_TOP KERNEL_BASE
#define PPTR_USER_TOP seL4_UserTop
#define BASE_OFFSET (PPTR_BASE - PADDR_BASE)

#define PAGE_BITS seL4_PageBits

#define MODE_RESERVED 0

/* MMU RISC-V related definitions. See RISC-V manual priv-1.10 */

/* Extract the n-level PT index from a virtual address. This works for any
 * configured RISC-V system with CONFIG_PT_LEVEL (which can be 2 on Sv32,
 * 3 on Sv38, or 4 on Sv48)
 */
#define RISCV_GET_PT_INDEX(addr, n)  (((addr) >> (((PT_INDEX_BITS) * (((CONFIG_PT_LEVELS) - 1) - (n))) + seL4_PageBits)) & MASK(PT_INDEX_BITS))
#define RISCV_GET_LVL_PGSIZE_BITS(n) (((PT_INDEX_BITS) * (((CONFIG_PT_LEVELS) - 1) - (n))) + seL4_PageBits)
#define RISCV_GET_LVL_PGSIZE(n)      BIT(RISCV_GET_LVL_PGSIZE_BITS((n)))
/*
 * These values are defined in RISC-V priv-1.10 manual, they represent the
 * exception codes saved in scause register (by the hardware) on traps.
 */
enum vm_fault_type {
    RISCVInstructionMisaligned = 0,
    RISCVInstructionAccessFault = 1,
    RISCVInstructionIllegal = 2,
    RISCVBreakpoint = 3,
    /* reserved */
    RISCVLoadAccessFault = 5,
    RISCVAddressMisaligned = 6,
    RISCVStoreAccessFault = 7,
    RISCVEnvCall = 8,
    /* 9-11 reserved */
    RISCVInstructionPageFault = 12,
    RISCVLoadPageFault = 13,
    /* 14 - reserved */
    RISCVStorePageFault = 15
                          /* >= 16 reserved */
};
typedef uint32_t vm_fault_type_t;

enum frameSizeConstants {
    RISCVPageBits        = seL4_PageBits,
    RISCVMegaPageBits    = seL4_LargePageBits,
#if CONFIG_PT_LEVELS > 2
    RISCVGigaPageBits    = seL4_HugePageBits,
#endif
#if CONFIG_PT_LEVELS > 3
    RISCVTeraPageBits    = seL4_TeraPageBits
#endif
};

enum vm_page_size {
    RISCV_4K_Page,
    RISCV_Mega_Page,
    RISCV_Giga_Page,
    RISCV_Tera_Page
};
typedef uint32_t vm_page_size_t;

static inline unsigned int CONST pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case RISCV_4K_Page:
        return RISCVPageBits;

    case RISCV_Mega_Page:
        return RISCVMegaPageBits;

#if CONFIG_PT_LEVELS > 2
    case RISCV_Giga_Page:
        return RISCVGigaPageBits;
#endif

#if CONFIG_PT_LEVELS > 3
    case RISCV_Tera_Page:
        return RISCVTeraPageBits;
#endif

    default:
        fail("Invalid page size");
    }
}
#endif /* __ASSEMBLER__ */

#define LOAD_S STRINGIFY(LOAD)
#define STORE_S STRINGIFY(STORE)

#define IPI_MEM_BARRIER \
    do { \
        asm volatile("fence rw,rw" ::: "memory"); \
    } while (0)

