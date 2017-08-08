/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __ARCH_LINKER_H
#define __ARCH_LINKER_H

#include <util.h>

/* code that is linked to physical addresses */
#define PHYS_CODE SECTION(".phys.text")

/* data that is linked to physical addresses */
#define PHYS_DATA SECTION(".phys.data")

/* bss data that is linked to physical addresses */
#define PHYS_BSS SECTION(".phys.bss")

#endif /* __ARCH_LINKER_H */
