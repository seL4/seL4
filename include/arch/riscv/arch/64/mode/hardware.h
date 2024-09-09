/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>

#if CONFIG_PT_LEVELS == 3

/*
 * The top half of the address space is reserved for the kernel. This means that 256 top level
 * entries are for the user, and 256 are for the kernel. This will be further split into the
 * 'regular' kernel window, which contains mappings to physical memory, a small (1GiB) higher
 * kernel image window that we use for running the actual kernel from and a top 1GiB window for
 * kernel device mappings. This means that between PPTR_BASE and
 * KERNEL_ELF_BASE there are 254 entries remaining, which represents how much physical memory
 * can be used.
 *
 * Almost all of the top 256 kernel entries will contain 1GiB page mappings. The only 2 entries
 * that contain a 2nd level PageTable consisting of 2MiB page entries is the entry
 * for the 1GiB Kernel ELF region and the 1GiB region corresponding to the physical memory
 * of the kernel ELF in the kernel window.  The same 2nd level PageTable is used and so both
 * entries refer to the same 1GiB of physical memory.
 * This means that the 1GiB kernel ELF mapping will correspond to physical memory with a 1GiB
 * alignment.
 *
 *                   +-----------------------------+ 2^64
 *                   |        Kernel Devices       |
 *                -> +-------------------KDEV_BASE-+ 2^64 - 1GiB
 *                |  |         Kernel ELF          |
 *            ----|  +-------------KERNEL_ELF_BASE-+ --+ 2^64 - 2GiB + (KERNEL_ELF_PADDR_BASE % 1GiB)
 *            |   |  |                             |
 *            |   -> +-----------------------------+ --+ 2^64 - 2GiB = (KERNEL_ELF_BASE % 1GiB)
 * Shared 1GiB|      |                             |   |
 * table entry|      |           PSpace            |   |
 *            |      |  (direct kernel mappings)   |   +----+
 *            ------>|                             |   |    |
 *                   |                             |   |    |
 *                   +-------------------PPTR_BASE-+ --+ 2^64 - 2^b
 *                   |                             |        |         +-------------------------+
 *                   |                             |        |         |                         |
 *                   |                             |        |         |                         |
 *                   |          Invalid            |        |         |                         |
 *                   |                             |        |         |           not           |
 *                   |                             |        |         |         kernel          |
 *                   |                             |        |         |       addressable       |
 *                   +--------------------USER_TOP-+  2^c   |         |                         |
 *                   |                             |        |         |                         |
 *                   |                             |        |         |                         |
 *                   |                             |        |      +- --------------------------+  PADDR_TOP =
 *                   |                             |        |      |  |                         |    PPTR_TOP - PPTR_BASE
 *                   |                             |        |      |  |                         |
 *                   |                             |        |      |  |                         |
 *                   |            User             |        |      |  |                         |
 *                   |                             |        |      |  |                         |
 *                   |                             |        +------+  +-------------------------+  KDEV_BASE - KERNEL_ELF_BASE + PADDR_LOAD
 *                   |                             |     kernel    |  |        Kernel ELF       |
 *                   |                             |   addressable |  +-------------------------+  KERNEL_ELF_PADDR_BASE
 *                   |                             |               |  |                         |
 *                   |                             |               |  |                         |
 *                   +-----------------------------+  0            +- +-------------------------+  0 PADDR_BASE
 *
 *                      virtual address space                          physical address space
 *
 *
 *  c = one less than number of bits the page tables can translate
 *    = sign extension bit for canonical addresses
 *    (= 47 on x64, 38 on RISCV64 sv39, 47 on RISCV64 sv48)
 *  b = The number of bits used by kernel mapping.
 *    = 38 (half of the 1 level page table) on RISCV64 sv39
 *    = 39 (entire second level page table) on aarch64 / X64 / sv48
 */

/* last accessible virtual address in user space */
#define USER_TOP seL4_UserTop

/* The first physical address to map into the kernel's physical memory
 * window */
#define PADDR_BASE UL_CONST(0x0)

/* The base address in virtual memory to use for the 1:1 physical memory
 * mapping */
#define PPTR_BASE UL_CONST(0xFFFFFFC000000000)

/* Top of the physical memory window */
#define PPTR_TOP UL_CONST(0xFFFFFFFF80000000)

/* The physical memory address to use for mapping the kernel ELF */
/* This represents the physical address that the kernel image will be linked to. This needs to
 * be on a 1gb boundary as we currently require being able to creating a mapping to this address
 * as the largest frame size */
#define KERNEL_ELF_PADDR_BASE (physBase() + UL_CONST(0x4000000))
/* For use by the linker (only integer constants allowed) */
#define KERNEL_ELF_PADDR_BASE_RAW (PHYS_BASE_RAW + UL_CONST(0x4000000))

/* The base address in virtual memory to use for the kernel ELF mapping */
#define KERNEL_ELF_BASE (PPTR_TOP + (KERNEL_ELF_PADDR_BASE & MASK(30)))
/* For use by the linker (only integer constants allowed) */
#define KERNEL_ELF_BASE_RAW (PPTR_TOP + (KERNEL_ELF_PADDR_BASE_RAW & MASK(30)))

/* The base address in virtual memory to use for the kernel device
 * mapping region. These are mapped in the kernel page table. */
#define KDEV_BASE UL_CONST(0xFFFFFFFFC0000000)

/* Place the kernel log buffer at the end of the kernel device page table */
#define KS_LOG_PPTR UL_CONST(0XFFFFFFFFFFE00000)

#else
#error Only PT_LEVELS == 3 is supported
#endif

#if defined(CONFIG_HAVE_CHERI)
#define LOAD  clc
#define STORE csc
#define ILOAD  cld
#define ISTORE csd
#define REGBYTES 16
#else
#define LOAD  ld
#define STORE sd
#define ILOAD  LOAD
#define ISTORE STORE
#define REGBYTES 8
#endif
