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

/* code that is linked to physical addresses */
#define PHYS_CODE SECTION(".phys.text")

/* code that is only used during kernel bootstrapping */
#define BOOT_CODE SECTION(".boot.text")

/* node-local data that is only used during kernel bootstrapping */
#define BOOT_DATA SECTION(".boot.node")

/* global data (shared by all nodes) but only used during kernel bootstrapping */
#define BOOT_DATA_GLOB SECTION(".boot.glob")

/* global data (shared by all nodes) */
#define DATA_GLOB SECTION(".glob")

#endif
