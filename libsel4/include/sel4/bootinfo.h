/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_BOOTINFO_H
#define __LIBSEL4_BOOTINFO_H

#include <sel4/types.h>
#include <sel4/bootinfo_types.h>
#include <sel4/macros.h>

void seL4_InitBootInfo(seL4_BootInfo* bi) SEL4_DEPRECATED("libsel4 management of bootinfo is deprecated, see the BootInfo Frame section of the manual");
seL4_BootInfo* seL4_GetBootInfo(void) SEL4_DEPRECATED("libsel4 management of bootinfo is deprecated, see the BootInfo Frame section of the manual");

#endif // __LIBSEL4_BOOTINFO_H
