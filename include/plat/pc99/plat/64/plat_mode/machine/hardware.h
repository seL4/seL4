/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>

/*
 *          2^64 +-------------------+
 *               | Kernel Page PDPT  | --+
 *   2^64 - 2^39 +-------------------+ PPTR_BASE
 *               |    TLB Bitmaps    |   |
 *               +-------------------+   |
 *               |                   |   |
 *               |     Unmapped      |   |
 *               |                   |   |
 *   2^64 - 2^47 +-------------------+   |
 *               |                   |   |
 *               |   Unaddressable   |   |
 *               |                   |   |
 *          2^47 +-------------------+ USER_TOP
 *               |                   |   |
 *               |       User        |   |
 *               |                   |   |
 *           0x0 +-------------------+   |
 *                                       |
 *                         +-------------+
 *                         |
 *                         v
 *          2^64 +-------------------+
 *               |                   |
 *               |                   |     +------+      +------+
 *               |                   | --> |  PD  | -+-> |  PT  |
 *               |  Kernel Devices   |     +------+  |   +------+
 *               |                   |               |
 *               |                   |               +-> Log Buffer
 *               |                   |
 *   2^64 - 2^30 +-------------------+ KDEV_BASE
 *               |                   |
 *               |                   |     +------+
 *               |    Kernel ELF     | --> |  PD  |
 *               |                   |     +------+
 *               |                   |
 *   2^64 - 2^29 +-------------------+ PPTR_TOP / KERNEL_ELF_BASE
 *               |                   |
 *               |  Physical Memory  |
 *               |       Window      |
 *               |                   |
 *   2^64 - 2^39 +-------------------+ PPTR_BASE
 */

/* WARNING: some of these constants are also defined in linker.lds
 * These constants are written out in full instead of using bit arithmetic
 * because they need to defined like this in linker.lds
 */

/* Define USER_TOP to be 1 before the last address before sign extension occurs.
 * This ensures that
 *  1. user addresses never needed to be sign extended to be valid canonical addresses
 *  2. the user cannot map the last page before addresses need sign extension. This prevents
 *     the user doing a syscall as the very last instruction and the CPU calculated PC + 2
 *     from being an invalid (non sign extended) address
 */
#define USER_TOP UL_CONST(0x7FFFFFFFFFFF)

/* The first physical address to map into the kernel's physical memory
 * window */
#define PADDR_BASE UL_CONST(0x00000000)

/* The base address in virtual memory to use for the 1:1 physical memory
 * mapping. Our kernel window is 2^39 bits (2^9 * 1gb) and the virtual
 * address range is 48 bits. Therefore our base is 2^48 - 2^39 */
#define PPTR_BASE UL_CONST(0xffffff8000000000)

/* Below the main kernel window we have any slots for the TLB bitmap */
#define TLBBITMAP_PML4_RESERVED (TLBBITMAP_ROOT_ENTRIES * BIT(PML4_INDEX_OFFSET))
#define TLBBITMAP_PPTR (PPTR_BASE - TLBBITMAP_PML4_RESERVED)

/* The kernel binary itself is placed in the bottom 1gb of the top
 * 2gb of virtual address space. This is so we can use the 'kernel'
 * memory model of GCC, which requires all symbols to be linked
 * within the top 2GiB of memory. This is (2^48 - 2 ^ 31) */
#define PPTR_TOP UL_CONST(0xffffffff80000000)

/* The physical memory address to use for mapping the kernel ELF */
#define KERNEL_ELF_PADDR_BASE UL_CONST(0x00100000)

/* Kernel mapping starts directly after the physical memory window */
#define KERNEL_ELF_BASE (PPTR_TOP + KERNEL_ELF_PADDR_BASE)

/* Put the kernel devices at the very beginning of the top
 * 1GB. This means they are precisely after the kernel binary
 * region. This is 2^48 - 2^30 */
#define KDEV_BASE UL_CONST(0xffffffffc0000000)

/* The kernel log buffer is a large page mapped into the second index
 * of the page directory that is only otherwise used for the kernel
 * device page table. */
#ifdef CONFIG_KERNEL_LOG_BUFFER
#define KS_LOG_PPTR (KDEV_BASE + BIT(seL4_LargePageBits))
#endif

#ifndef __ASSEMBLER__

#include <basic_types.h>
#include <plat/machine.h>
#include <plat_mode/machine/hardware_gen.h>
#include <arch/kernel/tlb_bitmap_defs.h>

/* ensure the user top and tlb bitmap do not overlap if multicore */
#ifdef ENABLE_SMP_SUPPORT
compile_assert(user_top_tlbbitmap_no_overlap, GET_PML4_INDEX(USER_TOP) != GET_PML4_INDEX(TLBBITMAP_PPTR))
#endif

#endif /* __ASSEMBLER__ */

