/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_SEL4_ARCH_CONSTANTS_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#ifndef __ASSEMBLER__
enum {
    seL4_GlobalsFrame = 0xffffc000,
};
#endif

#define seL4_DataFault 0
#define seL4_InstructionFault 1

/* object sizes - 2^n */
#define seL4_PageBits 12
#define seL4_LargePageBits 16
#define seL4_SlotBits 4
#define seL4_TCBBits 9
#define seL4_EndpointBits 4
#define seL4_NotificationBits 4

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define seL4_PageTableBits      12
#define seL4_SectionBits 21
#define seL4_SuperSectionBits 25
#else
#define seL4_PageTableBits 10
#define seL4_SectionBits 20
#define seL4_SuperSectionBits 24
#endif

#define seL4_PageDirBits 14
#define seL4_ASIDPoolBits 12
#define seL4_ARM_VCPUBits       12
#define seL4_IOPageTableBits    12
/* word size */
#define seL4_WordBits (sizeof(seL4_Word) * 8)

/* Untyped size limits */
#define seL4_MinUntypedBits 4
#define seL4_MaxUntypedBits 29

#ifdef CONFIG_ENABLE_BENCHMARKS
/* size of kernel log buffer in bytes */
#define seL4_LogBufferSize (LIBSEL4_BIT(20))
#endif /* CONFIG_ENABLE_BENCHMARKS */

#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstBreakpoint (0)
#define seL4_FirstDualFunctionMonitor (-1)
#define seL4_NumDualFunctionMonitors (0)
#endif

#endif
