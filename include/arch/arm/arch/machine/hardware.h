/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <mode/machine/hardware.h>

#ifndef __ASSEMBLER__
enum vm_fault_type {
    ARMDataAbort = seL4_DataFault,
    ARMPrefetchAbort = seL4_InstructionFault
};
typedef word_t vm_fault_type_t;

#define PAGE_BASE(_p, _s)        ((_p) & ~MASK(pageBitsForSize((_s))))
#define PAGE_OFFSET(_p, _s)      ((_p) & MASK(pageBitsForSize((_s))))

#define IPI_MEM_BARRIER \
  do { \
     /* This can be relaxed for GICv2 but for GICv3 dmb() no longer works */ \
     /* since the way IPI is triggered is different (memory-mapped or MSR inst.) */ \
     /* and dmb() is not able to avoid re-ordering between memory accesses and */ \
     /* instructions. In order to support both GICv2 and v3 dsb() is required. */ \
     dsb_ishst(); \
  } while (0)

#endif /* __ASSEMBLER__ */

#define L1_CACHE_LINE_SIZE_BITS CONFIG_L1_CACHE_LINE_SIZE_BITS
#define L1_CACHE_LINE_SIZE BIT(L1_CACHE_LINE_SIZE_BITS)

