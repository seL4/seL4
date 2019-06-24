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

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 */

#ifndef __ARCH_MODE_HARDWARE_H
#define __ARCH_MODE_HARDWARE_H

#define LOAD  lw
#define STORE sw

/* Contain the typical location of memory */
#define PADDR_BASE physBase
/* This represents the physical address that the kernel image will be linked to. This needs to
 * be on a 1gb boundary as we currently require being able to creating a mapping to this address
 * as the largest frame size */
#define PADDR_LOAD UL_CONST(0x84000000)
/* This is the base of the kernel window, which is directly mapped to PADDR_BASE */
#define PPTR_BASE  seL4_UserTop
/* This is the mapping of the kernel (mapped above the kernel window currently) */
#define KERNEL_BASE UL_CONST(0xFF800000)
#define KERNEL_ELF_BASE KERNEL_BASE
/* Start of kernel device mapping region in highest 4MiB of memory. */
#define KDEV_PPTR UL_CONST(0xFFC00000)

#endif /* __ARCH_MODE_HARDWARE_H */
