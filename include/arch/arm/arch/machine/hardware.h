/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_HARDWARE_H
#define __ARCH_MACHINE_HARDWARE_H

#define PAGE_BITS 12

#define PPTR_VECTOR_TABLE 0xffff0000
#define PPTR_GLOBALS_PAGE 0xffffc000

/* The stack is the very last page of virtual memory. */
#define PPTR_KERNEL_STACK 0xfffff000
/*
 * We reserve 16 bytes at the end of the stack - perhaps we could store some
 * useful data here? Also, if we didn't subtract 16 off, the default initial sp
 * is 0x0, which is perfectly valid and would wrap correctly, but let's not
 * test the boundary cases here.
 */
#define PPTR_KERNEL_STACK_TOP (PPTR_KERNEL_STACK + 0x1000 - 16)

#if defined(CONFIG_ARM1136JF_S)
#define L1_CACHE_LINE_SIZE_BITS  5 /* 32 bytes */

#elif defined(ARM_CORTEX_A8)
#define L1_CACHE_LINE_SIZE_BITS  6 /* 64 bytes */

#elif defined(ARM_CORTEX_A9)
#define L1_CACHE_LINE_SIZE_BITS  5 /* 32 bytes */

#elif defined(ARM_CORTEX_A15)
#define L1_CACHE_LINE_SIZE_BITS  6 /* 64 bytes */
#endif

#ifndef L1_CACHE_LINE_SIZE_BITS
#error Unable to determine L1 cache line size
#endif


/* Processor ID used to check if errata work arounds need to be performed */
#define ARM1136_R0PX 0x4107b360

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

/* Processor mode encodings (for CPS etc.) */
#define PMODE_USER       0x10
#define PMODE_FIQ        0x11
#define PMODE_IRQ        0x12
#define PMODE_SUPERVISOR 0x13
#define PMODE_ABORT      0x17
#define PMODE_UNDEFINED  0x1b
#define PMODE_SYSTEM     0x1f
/* Processor exception mask bits */
#define PMASK_ASYNC_ABORT (1 << 8)
#define PMASK_IRQ         (1 << 7)
#define PMASK_FIRQ        (1 << 6)

/* VM event types, this should match the encoding of vm_fault_type below */
#define VM_EVENT_DATA_ABORT 0
#define VM_EVENT_PREFETCH_ABORT 1

#ifndef __ASSEMBLER__

#include <arch/types.h>

enum vm_fault_type {
    ARMDataAbort = 0,
    ARMPrefetchAbort = 1
};
typedef uint32_t vm_fault_type_t;

enum vm_page_size {
    ARMSmallPage,
    ARMLargePage,
    ARMSection,
    ARMSuperSection
};
typedef uint32_t vm_page_size_t;

enum frameSizeConstants {
    ARMSmallPageBits    = 12,
    ARMLargePageBits    = 16,
    ARMSectionBits      = 20,
    ARMSuperSectionBits = 24
};

static inline unsigned int CONST
pageBitsForSize(vm_page_size_t pagesize)
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

#endif /* !__ARCH_MACHINE_HARDWARE_H */
