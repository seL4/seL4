/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <mode/kernel/elf.h>

/* minimal ELF functionality for loading GRUB boot module */
bool_t elf_checkFile(Elf_Header_t *elfFile);
v_region_t elf_getMemoryBounds(Elf_Header_t *elfFile);
void elf_load(Elf_Header_t *elfFile, seL4_Word offset);

