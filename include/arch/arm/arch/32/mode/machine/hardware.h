/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <sel4/sel4_arch/constants.h>

#define PAGE_BITS seL4_PageBits

#define PPTR_VECTOR_TABLE 0xffff0000

/* Control register fields */
#define CONTROL_M   0  /* MMU enable */
#define CONTROL_A   1  /* Alignment fault enable */
#define CONTROL_C   2  /* L1 data cache enable */
#define CONTROL_W   3  /* Write buffer enable */
#define CONTROL_B   7  /* Big endian mode */
#define CONTROL_S   8  /* System protection (deprecated) */
#define CONTROL_R   9  /* ROM protection (deprecated) */
#define CONTROL_Z   11 /* Flow prediction enable */
#define CONTROL_I   12 /* L1 instruction cache enable */
#define CONTROL_V   13 /* Exception vector remap */
#define CONTROL_RR  14 /* Cache replacement strategy */
#define CONTROL_FI  21 /* Fast Interrupt enable */
#define CONTROL_U   22 /* Unaligned access enable */
#define CONTROL_XP  23 /* Subpage AP bits disable */
#define CONTROL_VE  24 /* Vectored interrupt enable */
#define CONTROL_EE  25 /* Exception E bit */
#define CONTROL_TRE 28 /* TEX remap enable */
#define CONTROL_AP  29 /* Access Flag Enable */

#ifdef CONFIG_PLAT_HIKEY
/* Prefetcher register fields */
#ifdef CONFIG_DEBUG_DISABLE_PREFETCHERS

#define PREFETCHER         0x0
#define PREFETCHER_MASK    0xE000

#else /* CONFIG_DEBUG_DISABLE_PREFETCHERS */

#define L1PCTL          (CONFIG_ARM_HIKEY_OUTSTANDING_PREFETCHERS << 13)      /* Number of outstanding prefetch streams */
#define STRIDE          ((CONFIG_ARM_HIKEY_PREFETCHER_STRIDE-2) << 17)  /* Consecutive strides to trigger prefetch */
#define NPFSTRM         ((CONFIG_ARM_HIKEY_PREFETCHER_NPFSTRM-1) << 19) /* Number of independent prefetch streams*/

#ifndef CONFIG_ARM_HIKEY_PREFETCHER_STBPFDIS    /* Disable prefetch streams from STB access */
#define STBPFDIS (1 << 22)
#else
#define STBPFDIS (0 << 22)
#endif

#ifdef CONFIG_ARM_HIKEY_PREFETCHER_STBPFRS      /* ReadUnique or ReadShared to initiate prefetch from STB access*/
#define STBPFRS (1 << 23)
#else
#define STBPFRS (0 << 23)
#endif

#define PREFETCHER      (L1PCTL | \
                        STRIDE | \
                        NPFSTRM | \
                        STBPFDIS| \
                        STBPFRS)
#define PREFETCHER_MASK 0xDAE000       /* Mask bits */
#endif /* CONFIG_DEBUG_DISABLE_PREFETCHERS */
#endif /* CONFIG_PLAT_HIKEY */

/* Processor mode encodings (for CPS etc.) */
#define PMODE_USER       0x10
#define PMODE_FIQ        0x11
#define PMODE_IRQ        0x12
#define PMODE_SUPERVISOR 0x13
#define PMODE_ABORT      0x17
#define PMODE_HYPERVISOR 0x1a
#define PMODE_UNDEFINED  0x1b
#define PMODE_SYSTEM     0x1f
/* Processor exception mask bits */
#define PMASK_ASYNC_ABORT (1 << 8)
#define PMASK_IRQ         (1 << 7)
#define PMASK_FIRQ        (1 << 6)

/* Kernel operating mode */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PMODE_KERNEL     PMODE_HYPERVISOR
#define PMODE_IDLE       PMODE_HYPERVISOR
#else
#define PMODE_KERNEL     PMODE_SUPERVISOR
#define PMODE_IDLE       PMODE_SYSTEM
#endif

#ifndef __ASSEMBLER__

#include <arch/types.h>

enum vm_page_size {
    ARMSmallPage,
    ARMLargePage,
    ARMSection,
    ARMSuperSection
};
typedef word_t vm_page_size_t;

enum frameSizeConstants {
    ARMSmallPageBits    = seL4_PageBits,
    ARMLargePageBits    = seL4_LargePageBits,
    ARMSectionBits      = seL4_SectionBits,
    ARMSuperSectionBits = seL4_SuperSectionBits
};

static inline word_t CONST pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case ARMSmallPage:
        return ARMSmallPageBits;

    case ARMLargePage:
        return ARMLargePageBits;

    case ARMSection:
        return ARMSectionBits;

    case ARMSuperSection:
        return ARMSuperSectionBits;

    default:
        fail("Invalid page size");
    }
}

#endif /* __ASSEMBLER__ */

