/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/machine/hardware.h>
#include <sel4/plat/api/constants.h>


#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
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
 * +-----------------------------------+ <- kernelBase: 256TiB minus 512GiB.
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
 */
#define kernelBase          0x0000ff8000000000
#else
#define kernelBase          0xffffff8000000000
#endif

/* last accessible virtual address in user space */
#define USER_TOP seL4_UserTop
/* the base physical address that the kernel can address */
#define PADDR_BASE 0x0
/* the physical address that the kernel image is linked to */
#define PADDR_LOAD physBase

/* offset between physical addresses and kernel virtual addresses */
#define BASE_OFFSET (kernelBase - PADDR_BASE)
#define KERNEL_ELF_BASE (PADDR_LOAD + BASE_OFFSET)

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PPTR_TOP 0xffffc0000000lu
#define KDEV_BASE 0xffffffe00000lu
#else
#define PPTR_TOP 0xffffffffc0000000
#define KDEV_BASE 0xffffffffffe00000lu
#endif

#define PADDR_TOP (PPTR_TOP - BASE_OFFSET)


