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

void seL4_InitBootInfo(seL4_BootInfo* bi);
seL4_BootInfo* seL4_GetBootInfo(void);

#endif // __LIBSEL4_BOOTINFO_H
