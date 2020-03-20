/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <arch/linker.h>

/* code that is only used during kernel bootstrapping */
#define BOOT_CODE SECTION(".boot.text")

/* read-only data only used during kernel bootstrapping */
#define BOOT_RODATA SECTION(".boot.rodata")

/* read/write data only used during kernel bootstrapping */
#define BOOT_DATA SECTION(".boot.data")

/* node-local bss data that is only used during kernel bootstrapping */
#define BOOT_BSS SECTION(".boot.bss")

/* data will be aligned to n bytes in a special BSS section */
#define ALIGN_BSS(n) ALIGN(n) SECTION(".bss.aligned")

/* data that will be mapped into and permitted to be used in the restricted SKIM
 * address space */
#define SKIM_DATA SECTION(".skim.data")

/* bss data that is permitted to be used in the restricted SKIM address space */
#define SKIM_BSS SECTION(".skim.bss")


