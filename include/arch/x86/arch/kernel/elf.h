/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>

#define ELFCLASS32 1
#define ELFCLASS64 2
#define PT_LOAD    1 /* loadable program segment */
#define ET_EXEC    2 /* executable file */
#define ET_DYN     3 /* shared object / position-independent executable */
#define EM_386     3 /* Intel 80386 (32-bit x86) */
#define EM_X86_64  62 /* AMD x86-64 */

#include <mode/kernel/elf.h>

/* minimal ELF functionality for loading GRUB boot module */
bool_t elf_checkFile(Elf_Header_t *elfFile, word_t max_length);
v_region_t elf_getMemoryBounds(Elf_Header_t *elfFile);
void elf_load(Elf_Header_t *elfFile, seL4_Word offset);
