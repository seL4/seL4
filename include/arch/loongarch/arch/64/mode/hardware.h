/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 *
 * Derived from:
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>

#if CONFIG_PT_LEVELS == 3

/* last accessible virtual address in user space */
#define USER_TOP seL4_UserTop

/* The first physical address to map into the kernel's physical memory
 * window */
#define PADDR_BASE UL_CONST(0x0)

/* The base address in virtual memory to use for the 1:1 physical memory
 * mapping */
#define PPTR_BASE UL_CONST(0xFFFF800000000000)

/* Top of the physical memory window */
#define PPTR_TOP UL_CONST(0xFFFF810000000000)

/* The physical memory address to use for mapping the kernel ELF */
/* This represents the physical address that the kernel image will be linked to. This needs to
 * be on a 1gb boundary as we currently require being able to creating a mapping to this address
 * as the largest frame size */

#define KERNEL_ELF_PADDR_BASE (physBase + UL_CONST(0x0))

/* The base address in virtual memory to use for the kernel ELF mapping, MASK(25) for 32MB page, MASK(36) for 64GB page*/
#define KERNEL_ELF_BASE (PPTR_TOP + (KERNEL_ELF_PADDR_BASE & MASK(25)))

/* The base address in virtual memory to use for the kernel device
 * mapping region. These are mapped in the kernel page table. */
#define KDEV_BASE UL_CONST(0xFFFF81101FE00000)
/*#define KDEV_BASE UL_CONST(0xFFFF81101E000000)*/

/* Place the kernel log buffer at the end of the kernel device page table */
#define KS_LOG_PPTR UL_CONST(0xFFFF80000F000000)

#else
#error //Only PT_LEVELS == 3 is supported
#endif

/*rv
#define LOAD  ld
#define STORE sd
*/
