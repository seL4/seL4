/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 * 
 * Derived from:
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */


#pragma once

#include <util.h>

#include <mode/hardware.h>

/* Privileged CSR definitions */
#define CSR_PRMD_PIE   0x00000004
#define CSR_PRMD_PPLV0 0X00000000
#define CSR_PRMD_PPLV3 0X00000003
// #ifndef __ASSEMBLER__

// #include <config.h>
// #include <linker.h>

// #include <arch/types.h>
// #include <sel4/sel4_arch/constants.h>

/* The size is for Loongson 3A5000`s cache line size, which is recorded in cpucfg.11.Linesize-log2[bit30:24] */
#define L1_CACHE_LINE_SIZE_BITS     6
#define L1_CACHE_LINE_SIZE          BIT(L1_CACHE_LINE_SIZE_BITS)

#define PAGE_BITS seL4_PageBits

/* MMU Loongarch related definitions. */

#ifdef __ASSEMBLER__
#define PT_INDEX_BITS 11
#define PTRS_PER_PGD (UL_CONST(1) << PT_INDEX_BITS)
#endif /*__ASSEMBLER__*/

#define PT_LEVEL_1 1
#define PT_LEVEL_2 2
#define PT_LEVEL_3 3

#define PT_LEVEL_1_BITS 36
#define PT_LEVEL_2_BITS 25
#define PT_LEVEL_3_BITS 14

#define LOONGARCH_L1PGSHIFT PT_LEVEL_1_BITS
#define LOONGARCH_L2PGSHIFT PT_LEVEL_2_BITS
#define LOONGARCH_L3PGSHIFT PT_LEVEL_3_BITS

#define PTE_L3_PA(PT_BASE) (word_t)(((PT_BASE) >> LOONGARCH_L3PGSHIFT) << LOONGARCH_L3PGSHIFT)
#define PTE_L2_PA(PT_BASE) (word_t)(((PT_BASE) >> LOONGARCH_L2PGSHIFT) << LOONGARCH_L2PGSHIFT)
#define PTE_L1_PA(PT_BASE) (word_t)(((PT_BASE) >> LOONGARCH_L1PGSHIFT) << LOONGARCH_L1PGSHIFT)
#define PTE_GSRWXV 0x3D3
#define PTE_H_GSRWXV 0x13D3
#define PTE_H_GSRWXV_UNCACHE_PLV0 0x13C3
// #define PTE_H_GSRWXV_UNCACHE_PLV3 0x13CF
#define PTE_USER_GSRWXV 0x39F
#define PTE_H_USER_GSRWXV 0x139F
// #define PTE_H_USER_GSRWXV 0x138F //maybe let all uncache?

#define PTE_CREATE_NEXT(PT_BASE) (word_t)PT_BASE
#define PTE_CREATE_L3_LEAF(PT_BASE) (word_t)(PTE_L3_PA(PT_BASE) | PTE_GSRWXV)
#define PTE_CREATE_L2_LEAF(PT_BASE) (word_t)(PTE_L2_PA(PT_BASE) | PTE_H_GSRWXV)
#define PTE_CREATE_L1_LEAF(PT_BASE) (word_t)(PTE_L1_PA(PT_BASE) | PTE_H_GSRWXV)
#define USER_PTE_CREATE_L3_LEAF(PT_BASE) (word_t)(PTE_L3_PA(PT_BASE) | PTE_USER_GSRWXV)
#define USER_PTE_CREATE_L2_LEAF(PT_BASE) (word_t)(PTE_L2_PA(PT_BASE) | PTE_H_USER_GSRWXV)
#define USER_PTE_CREATE_L1_LEAF(PT_BASE) (word_t)(PTE_L1_PA(PT_BASE) | PTE_H_USER_GSRWXV)

/* Extract the n-level PT index from a virtual address. This works for any
 * configured Loongarch system with CONFIG_PT_LEVEL
 */

#define LA_GET_PT_INDEX(addr, n)  (((addr) >> (((PT_INDEX_BITS) * ((CONFIG_PT_LEVELS) - (n))) + seL4_PageBits)) & MASK(PT_INDEX_BITS))
#define LA_GET_LVL_PGSIZE_BITS(n) (((PT_INDEX_BITS) * ((CONFIG_PT_LEVELS) - (n))) + seL4_PageBits)
#define LA_GET_LVL_PGSIZE(n)      BIT(LA_GET_LVL_PGSIZE_BITS((n)))

/* Page table bits */

#define	_PAGE_VALID_SHIFT	0
#define	_PAGE_ACCESSED_SHIFT	0  /* Reuse Valid for Accessed */
#define	_PAGE_DIRTY_SHIFT	1
#define	_PAGE_PLV_SHIFT		2  /* 2~3, two bits */
#define	_CACHE_SHIFT		4  /* 4~5, two bits */
#define	_PAGE_GLOBAL_SHIFT	6
#define	_PAGE_HUGE_SHIFT	6  /* HUGE is a PMD bit */
#define	_PAGE_PRESENT_SHIFT	7
#define	_PAGE_WRITE_SHIFT	8
#define	_PAGE_MODIFIED_SHIFT	9
#define	_PAGE_PROTNONE_SHIFT	10
#define	_PAGE_SPECIAL_SHIFT	11
#define	_PAGE_HGLOBAL_SHIFT	12 /* HGlobal is a PMD bit */
#define	_PAGE_PFN_SHIFT		12
#define	_PAGE_PFN_END_SHIFT	48
#define	_PAGE_NO_READ_SHIFT	61
#define	_PAGE_NO_EXEC_SHIFT	62
#define	_PAGE_RPLV_SHIFT	63

/* Used only by software */
#define _PAGE_PRESENT		(UL_CONST(1) << _PAGE_PRESENT_SHIFT)
#define _PAGE_WRITE		(UL_CONST(1) << _PAGE_WRITE_SHIFT)
#define _PAGE_ACCESSED		(UL_CONST(1) << _PAGE_ACCESSED_SHIFT)
#define _PAGE_MODIFIED		(UL_CONST(1) << _PAGE_MODIFIED_SHIFT)
#define _PAGE_PROTNONE		(UL_CONST(1) << _PAGE_PROTNONE_SHIFT)
#define _PAGE_SPECIAL		(UL_CONST(1) << _PAGE_SPECIAL_SHIFT)

/* Used by TLB hardware (placed in EntryLo*) */
#define _PAGE_VALID		(UL_CONST(1) << _PAGE_VALID_SHIFT)
#define _PAGE_DIRTY		(UL_CONST(1) << _PAGE_DIRTY_SHIFT)
#define _PAGE_PLV		(UL_CONST(3) << _PAGE_PLV_SHIFT)
#define _PAGE_GLOBAL		(UL_CONST(1) << _PAGE_GLOBAL_SHIFT)
#define _PAGE_HUGE		(UL_CONST(1) << _PAGE_HUGE_SHIFT)
#define _PAGE_HGLOBAL		(UL_CONST(1) << _PAGE_HGLOBAL_SHIFT)
#define _PAGE_NO_READ		(UL_CONST(1) << _PAGE_NO_READ_SHIFT)
#define _PAGE_NO_EXEC		(UL_CONST(1) << _PAGE_NO_EXEC_SHIFT)
#define _PAGE_RPLV		(UL_CONST(1) << _PAGE_RPLV_SHIFT)
#define _CACHE_MASK		(UL_CONST(3) << _CACHE_SHIFT)
#define _PFN_SHIFT		(PAGE_SHIFT - 12 + _PAGE_PFN_SHIFT)

#define _PAGE_USER	(PLV_USER << _PAGE_PLV_SHIFT)
#define _PAGE_KERN	(PLV_KERN << _PAGE_PLV_SHIFT)

#define _PFN_MASK (~((UL_CONST(1) << (_PFN_SHIFT)) - 1) & \
		  ((UL_CONST(1) << (_PAGE_PFN_END_SHIFT)) - 1))

#ifndef __ASSEMBLER__
#include <config.h>
#include <linker.h>

#include <arch/types.h>
#include <sel4/sel4_arch/constants.h>
          
/*
 * These values are defined in LoongArch Reference Manual Volume 1: Basic Architecture version 1.00,
 * they represent the exception codes saved in CSR.ESTAT register (by the hardware) on traps. For some
 * Ecode, you need to check EsubCode for details.
 */
enum vm_fault_type {
    LALoadPageInvalid=1, //PIL
    LAStorePageInvalid=2,//PIS
    LAFetchPageInvalid=3,//PIF
    LAPageModException=4,//PME
    LAPageNoReadable=5,     //PNR
    LAPageNoExecutable=6,   //PNX
    LAPagePrivilegeIllegal=7,   //PPI
    LAAddrError=8,//ADEF or ADEM         /*Check EsubCode for details*/
    LAAddrAlignFault=9,//ALE
    LABoundCheck=10    //BCE

    //other exceptions are not related to vm_fault, which will be recorded in badv
    /* >=25 reserved*/
};
typedef word_t vm_fault_type_t;

enum frameSizeConstants {
    LOONGARCHPageBits        = seL4_PageBits,
    LOONGARCHMegaPageBits    = seL4_LargePageBits,
#if CONFIG_PT_LEVELS > 2
    LOONGARCHGigaPageBits    = seL4_HugePageBits,
#endif
#if CONFIG_PT_LEVELS > 3
    LOONGARCHTeraPageBits    = seL4_TeraPageBits
#endif
};

enum vm_page_size {
    LOONGARCH_16K_Page,
    LOONGARCH_Mega_Page,
    LOONGARCH_Giga_Page,
    LOONGARCH_Tera_Page
};
typedef word_t vm_page_size_t;

static inline word_t CONST pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case LOONGARCH_16K_Page:
        return LOONGARCHPageBits;

    case LOONGARCH_Mega_Page:
        return LOONGARCHMegaPageBits;

#if CONFIG_PT_LEVELS > 2
    case LOONGARCH_Giga_Page:
        return LOONGARCHGigaPageBits;
#endif

#if CONFIG_PT_LEVELS > 3
    case LOONGARCH_Tera_Page:
        return LOONGARCHTeraPageBits;
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

