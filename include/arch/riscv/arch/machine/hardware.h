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
#define SSTATUS_FS    0x00006000

#define SSTATUS_FS_CLEAN    0x00004000
#define SSTATUS_FS_INITIAL  0x00002000
#define SSTATUS_FS_DIRTY    0x00006000

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

#define PAGE_BITS seL4_PageBits

/* MMU RISC-V related definitions. See RISC-V manual priv-1.10 */

/* Extract the n-level PT index from a virtual address. This works for any
 * configured RISC-V system with CONFIG_PT_LEVEL (which can be 2 on Sv32,
 * 3 on Sv39, or 4 on Sv48)
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
    RISCVStorePageFault = 15,
    RISCVCheriFault = 0x1c
                          /* >= 16 reserved */
};
typedef word_t vm_fault_type_t;

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
typedef word_t vm_page_size_t;

static inline word_t CONST pageBitsForSize(vm_page_size_t pagesize)
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

static inline void arch_clean_invalidate_caches(void)
{
    /* RISC-V doesn't have an architecture defined way of flushing caches */
}
#endif /* __ASSEMBLER__ */

#define LOAD_S STRINGIFY(LOAD)
#define STORE_S STRINGIFY(STORE)

#define IPI_MEM_BARRIER \
    do { \
        asm volatile("fence rw,rw" ::: "memory"); \
    } while (0)

