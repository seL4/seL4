/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_LINKER_H
#define __ARCH_LINKER_H

#include <util.h>

/* code that is only used during kernel bootstrapping */
#define BOOT_CODE SECTION(".boot.text")

/* read-only data only used during kernel bootstrapping */
#define BOOT_RODATA SECTION(".boot.rodata")

/* read/write data only used during kernel bootstrapping */
#define BOOT_DATA SECTION(".boot.data")

/* data will be aligned to n bytes in a special BSS section */
#define ALIGN_BSS(n) ALIGN(n) SECTION(".bss.aligned")

#endif
