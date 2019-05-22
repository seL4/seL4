/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#ifndef __PLAT_MODE_MACHINE_HARDWARE_H
#define __PLAT_MODE_MACHINE_HARDWARE_H

#include <config.h>

#if CONFIG_PT_LEVELS == 3

/*
 * The top half of the address space is reserved for the kernel. This means that 256 top level
 * entries are for the user, and 256 are for the kernel. This will be further split into the
 * 'regular' kernel window, which contains mappings to physical memory, a small (1GiB) higher
 * kernel image window that we use for running the actual kernel from and a top 1GiB window for
 * kernel device mappings. This means that between PPTR_BASE and
 * KERNEL_BASE there are 254 entries remaining, which represents how much physical memory
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
 *                -> +-------------------PPTR_KDEV-+ 2^64 - 1GiB
 *                |  |         Kernel ELF          |
 *            ----|  +-------------KERNEL_ELF_BASE-+ --+ 2^64 - 2GiB + (PADDR_LOAD % 1GiB)
 *            |   |  |                             |
 *            |   -> +-----------------KERNEL_BASE-+ --+ 2^64 - 2GiB
 * Shared 1GiB|      |                             |   |
 * table entry|      |           PSpace            |   |
 *            |      |  (direct kernel mappings)   |   +----+
 *            ------>|                             |   |    |
 *                   |                             |   |    |
 *                   +-------------------PPTR_BASE-+ --+ 2^64 - 2^c
 *                   |                             |        |         +-------------------------+
 *                   |                             |        |         |                         |
 *                   |                             |        |         |                         |
 *                   |          Invalid            |        |         |                         |
 *                   |                             |        |         |           not           |
 *                   |                             |        |         |         kernel          |
 *                   |                             |        |         |       addressable       |
 *                   +-----------------------------+  2^c   |         |                         |
 *                   |                             |        |         |                         |
 *                   |                             |        |         |                         |
 *                   |                             |        |      +- --------------------------+  PADDR_TOP =
 *                   |                             |        |      |  |                         |    KERNEL_BASE - PPTR_BASE
 *                   |                             |        |      |  |                         |
 *                   |                             |        |      |  |                         |
 *                   |            User             |        |      |  |                         |
 *                   |                             |        |      |  |                         |
 *                   |                             |        +------+  +-------------------------+  PADDR_HIGH_TOP =
 *                   |                             |     kernel    |  |        Kernel ELF       |    (PPTR_KDEV - KERNEL_ELF_BASE + PADDR_LOAD)
 *                   |                             |   addressable |  +-------------------------+  PADDR_LOAD
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
 *
 */

/* This is the base of the kernel window, which is directly mapped to PADDR_BASE */
#define PPTR_BASE        0xFFFFFFC000000000lu
/* This is the mapping of the kernel (mapped above the kernel window currently) */
#define KERNEL_BASE      0xFFFFFFFF80000000lu
#define KERNEL_ELF_BASE  0xFFFFFFFF84000000lu
/* Start of kernel device mapping region in highest 1GiB of memory. */
#define PPTR_KDEV        0xFFFFFFFFC0000000lu
#else
#error Only PT_LEVELS == 3 is supported
#endif

#endif
