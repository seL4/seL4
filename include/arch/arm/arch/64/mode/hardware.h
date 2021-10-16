/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <arch/machine/hardware.h>
#include <sel4/plat/api/constants.h>

/* EL2 kernel address map:
 *
 * The EL2 mode kernel uses TTBR0_EL2 which covers the range of
 * 0x0 - 0x0000ffffffffffff (48 bits of vaddrspace).
 *
 * The CPU on the TX1 only allows for 48-bits of addressable virtual memory.
 * Within the seL4 EL2 kernel's separate vaddrspace, it uses
 * the 512 GiB at the top of that 48 bits of addressable
 * virtual memory.
 *
 * In EL2 there is no canonical-high portion of the address space since
 * address tagging is not supported in EL2. Therefore the kernel is linked
 * to the canonical lower portion of the address space (all the unused top bits
 * are set to 0, not 1).
 *
 * The memory map used by the EL2 kernel's separate address space
 * ends up looking something like this:
 *
 * +-----------------------------------+ <- 0xFFFFFFFF_FFFFFFFF
 * | Canonical high portion - unusable |
 * | virtual addrs                     |
 * +-----------------------------------+ <- PPTR_TOP: 256TiB mark (top of 48 bits)
 * | seL4 EL2 kernel                   |    ^
 * |                                   |    |
 * |                                   |    512GiB
 * |                                   |    of EL2 kernel windowing
 * |                                   |    into memory.
 * |                                   |    |
 * |                                   |    v
 * +-----------------------------------+ <- PPTR_BASE: 256TiB minus 512GiB.
 * | Unused virtual addresses within   |    ^
 * | the EL2 kernel's                  |    |
 * | separate vaddrspace.              |    Rest of the
 * |                                   |    EL2 kernel
 * |                                   |    vaddrspace, unused,
 * |                                   |    which is the rest of
 * |                                   |    the lower 256 TiB.
 * |                                   |    |
 * |                                   |    v
 * +-----------------------------------+ <- 0x0
 *
 * !defined(CONFIG_ARM_HYPERVISOR_SUPPORT)
 *
 *          2^64 +-------------------+
 *               | Kernel Page PDPT  | --+
 *   2^64 - 2^39 +-------------------+ PPTR_BASE
 *               |    TLB Bitmaps    |   |
 *               +-------------------+   |
 *               |                   |   |
 *               |     Unmapped      |   |
 *               |                   |   |
 *   2^64 - 2^48 +-------------------+   |
 *               |                   |   |
 *               |   Unaddressable   |   |
 *               |                   |   |
 *          2^48 +-------------------+ USER_TOP
 *               |                   |   |
 *               |       User        |   |
 *               |                   |   |
 *           0x0 +-------------------+   |
 *                                       |
 *                         +-------------+
 *                         |
 *                         v
 *          2^64 +-------------------+ PPTR_BASE
 *               |                   |
 *               |                   |     +------+
 *               | Kernel Page Table | --> |  PD  | ----+
 *               |                   |     +------+     |
 *               |                   |                  |
 *   2^64 - 2^30 +-------------------+ PPTR_TOP         |
 *               |                   |                  |
 *               |  Physical Memory  |                  |
 *               |       Window      |                  |
 *               |                   |                  |
 *               +-------------------+                  |
 *               |                   |                  |
 *               |                   |     +------+     |
 *               |    Kernel ELF     | --> |  PD  |     |
 *               |                   |     +------+     |
 *               |                   |                  |
 *               +-------------------+ KERNEL_ELF_BASE  |
 *               |                   |                  |
 *               |  Physical Memory  |                  |
 *               |       Window      |                  |
 *               |                   |                  |
 *   2^64 - 2^39 +-------------------+                  |
 *                                                      |
 *                                +---------------------+
 *                                |
 *                                v
 *   2^64 - 2^21 + 2^12 +-------------------+
 *                      |                   |
 *                      |  Kernel Devices   |
 *                      |                   |
 *          2^64 - 2^21 +-------------------+ KDEV_BASE
 *
 *
 * defined(CONFIG_ARM_HYPERVISOR_SUPPORT)
 *
 *          2^64 +-------------------+
 *               |                   |
 *               |   Unaddressable   |
 *               |                   |
 *          2^48 +-------------------+
 *               | Kernel Page PDPT  | --+
 *   2^48 - 2^39 +-------------------+ PPTR_BASE
 *               |    TLB Bitmaps    |   |
 *               +-------------------+   |
 *               |                   |   |
 *               |     Unmapped      |   |
 *               |                   |   |
 *           0x0 +-------------------+   |
 *                                       |
 *                         +-------------+
 *                         |
 *                         v
 *          2^48 +-------------------+
 *               |                   |
 *               |                   |     +------+
 *               | Kernel Page Table | --> |  PD  | ----+
 *               |                   |     +------+     |
 *               |                   |                  |
 *   2^48 - 2^30 +-------------------+ PPTR_TOP         |
 *               |                   |                  |
 *               |  Physical Memory  |                  |
 *               |       Window      |                  |
 *               |                   |                  |
 *               +-------------------+                  |
 *               |                   |                  |
 *               |                   |     +------+     |
 *               |    Kernel ELF     | --> |  PD  |     |
 *               |                   |     +------+     |
 *               |                   |                  |
 *               +-------------------+ KERNEL_ELF_BASE  |
 *               |                   |                  |
 *               |  Physical Memory  |                  |
 *               |       Window      |                  |
 *               |                   |                  |
 *   2^48 - 2^39 +-------------------+ PPTR_BASE        |
 *                                                      |
 *                                +---------------------+
 *                                |
 *                                v
 *   2^48 - 2^21 + 2^12 +-------------------+
 *                      |                   |
 *                      |  Kernel Devices   |
 *                      |                   |
 *          2^48 - 2^21 +-------------------+ KDEV_BASE
 *
 */

/* last accessible virtual address in user space */
#define USER_TOP seL4_UserTop

/* The first physical address to map into the kernel's physical memory
 * window */
#define PADDR_BASE UL_CONST(0x0)

/* The base address in virtual memory to use for the 1:1 physical memory
 * mapping */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PPTR_BASE UL_CONST(0x0000008000000000)
#else
#define PPTR_BASE UL_CONST(0xffffff8000000000)
#endif

/* Top of the physical memory window */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PPTR_TOP UL_CONST(0x000000ffc0000000)
#else
#define PPTR_TOP UL_CONST(0xffffffffc0000000)
#endif

/* The physical memory address to use for mapping the kernel ELF */
#define KERNEL_ELF_PADDR_BASE physBase

/* The base address in virtual memory to use for the kernel ELF mapping */
#define KERNEL_ELF_BASE (PPTR_BASE_OFFSET + KERNEL_ELF_PADDR_BASE)

/* This is a page table mapping at the end of the virtual address space
 * to map objects with 4KiB pages rather than 4MiB large pages. */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define KERNEL_PT_BASE UL_CONST(0x000000ffffe00000)
#else
#define KERNEL_PT_BASE UL_CONST(0xffffffffffe00000)
#endif

/* The base address in virtual memory to use for the kernel device
 * mapping region. These are mapped in the kernel page table. */
#define KDEV_BASE KERNEL_PT_BASE

/* The log buffer is placed before the device region */
#define KS_LOG_PPTR (KDEV_BASE - UL_CONST(0x200000))

/* All PPTR addresses must be canonical to be able to be stored in caps or objects.
   Check that all UTs that are created will have valid address in the PPTR space.
   For non-hyp, PPTR_BASE is in the top part of the address space and device untyped
   addresses are allowed to be large enough to overflow and be in the bottom half of
   the address space.  However, when the kernel is in EL2 it is not possible to safely
   overflow without going into address ranges that are non-canonical.  These static
   asserts check that the kernel config won't lead to UTs being created that aren't
   representable. */
#ifndef __ASSEMBLER__
compile_assert(ut_max_less_than_cannonical, CONFIG_PADDR_USER_DEVICE_TOP <= BIT(47));
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
compile_assert(ut_max_is_cannonical, (PPTR_BASE + CONFIG_PADDR_USER_DEVICE_TOP) <= BIT(48));
#endif
#endif
