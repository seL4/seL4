/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/* This header file is shared by assembler and C code, thus it should contain
 * only definitions, but no actual C code.
 *
 * Each architecture defines a set of constants in #defines. These
 * constants describe the memory regions of the kernel's portion of the
 * address space including the physical memory window, the kernel ELF
 * region, and the device region.
 *
 *  - USER_TOP: The first address after the end of user memory
 *
 *  - PADDR_BASE: The first physical address mapped in the kernel's
 *    physical memory window.
 *  - PPTR_BASE: The first virtual address of the kernel's physical
 *    memory window.
 *  - PPTR_TOP: The first virtual address after the end of the kernel's
 *    physical memory window.
 *
 *  - KERNEL_ELF_PADDR_BASE: The first physical address used to map the
 *    initial kernel image. The kernel ELF is mapped contiguously
 *    starting at this address.
 *  - KERNEL_ELF_BASE: The first virtual address used to map the initial
 *    kernel image.
 *
 *  - KDEV_BASE: The first virtual address used to map devices.
 */

/* The offset from a physical address to a virtual address in the
 * physical memory window. */
#define PPTR_BASE_OFFSET (PPTR_BASE - PADDR_BASE)

/* The last address in the physical memory region mapped into the
 * physical memory window */
#define PADDR_TOP (PPTR_TOP - PPTR_BASE_OFFSET)

/* The kernel base offset is a way to translate the kernel image segment
 * from virtual to physical. This translation must be a single offset
 * for for the entire segment (i.e. the kernel image must be contiguous
 * both virtually and physically) */
#define KERNEL_ELF_BASE_OFFSET (KERNEL_ELF_BASE - KERNEL_ELF_PADDR_BASE)

#include <mode/hardware.h>
