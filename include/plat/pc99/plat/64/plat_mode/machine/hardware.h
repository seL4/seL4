/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>

/* WARNING: some of these constants are also defined in linker.lds
 * These constants are written out in full instead of using bit arithmetic
 * because they need to defined like this in linker.lds
 */
#define PADDR_BASE  UL_CONST(0x00000000)
#define PADDR_LOAD  UL_CONST(0x00100000)
/* our kernel window is 2^39 bits (2^9 * 1gb) and the virtual address
 * range is 48 bits. Therefore our base is 2^48 - 2^39
 */
#define PPTR_BASE   UL_CONST(0xffffff8000000000)

/* The kernel binary itself is placed in the bottom 1gb of the top
 * 2gb of virtual address space. This is so we can use the 'kernel'
 * memory model of GCC, which requires all symbols to be linked
 * within the top 2GiB of memory. This is (2^48 - 2 ^ 31) */
#define KERNEL_BASE UL_CONST(0xffffffff80000000)

/* Put the kernel devices at the very beginning of the top
 * 1GB. This means they are precisely after the kernel binary
 * region. This is 2^48 - 2^30
 */
#define KDEV_BASE UL_CONST(0xffffffffc0000000)

/* PADDR_TOP is the end of our larger kernel window, just before the
 * kernel image itself */
#define PADDR_TOP (KERNEL_BASE - PPTR_BASE)

/* Define the top of our static 'kernel window', which is the top 1GiB of memory */
#define PADDR_HIGH_TOP (KDEV_BASE - KERNEL_BASE)

/* Below the main kernel window we have any slots for the TLB bitmap */
#define TLBBITMAP_PML4_RESERVED (TLBBITMAP_ROOT_ENTRIES * BIT(PML4_INDEX_OFFSET))
#define TLBBITMAP_PPTR (PPTR_BASE - TLBBITMAP_PML4_RESERVED)
#define PPTR_TOP TLBBITMAP_PPTR

/* Define PPTR_USER_TOP to be 1 before the last address before sign extension occurs.
 * This ensures that
 *  1. user addresses never needed to be sign extended to be valid canonical addresses
 *  2. the user cannot map the last page before addresses need sign extension. This prevents
 *     the user doing a syscall as the very last instruction and the CPU calculated PC + 2
 *     from being an invalid (non sign extended) address
 */
#define PPTR_USER_TOP 0x7FFFFFFFFFFF

#define KERNEL_BASE_OFFSET (KERNEL_BASE - PADDR_BASE)
#define kernelBase KERNEL_BASE

#define BASE_OFFSET PPTR_BASE

#ifndef __ASSEMBLER__

#include <config.h>
#include <basic_types.h>
#include <plat/machine.h>
#include <plat_mode/machine/hardware_gen.h>
#include <arch/kernel/tlb_bitmap_defs.h>

/* ensure the user top and tlb bitmap do not overlap if multicore */
#ifdef ENABLE_SMP_SUPPORT
compile_assert(user_top_tlbbitmap_no_overlap, GET_PML4_INDEX(PPTR_USER_TOP) != GET_PML4_INDEX(TLBBITMAP_PPTR))
#endif

/* since we have two kernel VM windows, we have two pptr to paddr
 * conversion functions.
 * paddr_to_kpptr converts physical address to the second small kernel
 * window which locates at the top 2GiB.
 */
static inline void *CONST
paddr_to_kpptr(paddr_t paddr)
{
    assert(paddr < PADDR_HIGH_TOP);
    return (void *)(paddr + KERNEL_BASE_OFFSET);
}

static inline paddr_t CONST kpptr_to_paddr(void *pptr)
{
    assert((word_t)pptr >= KERNEL_BASE);
    return (paddr_t)pptr - KERNEL_BASE_OFFSET;
}

#endif /* __ASSEMBLER__ */

