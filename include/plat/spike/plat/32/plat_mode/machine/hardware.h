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
#include <sel4/plat/api/constants.h>

/* This is the base of the kernel window, which is directly mapped to PADDR_BASE */
#define PPTR_BASE  seL4_UserTop
/* This is the mapping of the kernel (mapped above the kernel window currently) */
#define KERNEL_BASE 0xFF800000lu

#endif
