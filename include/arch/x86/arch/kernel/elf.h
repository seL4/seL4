/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <mode/kernel/elf.h>

/* standard ELF constants needed to validate the boot module (see
 * elf(5); values are the same for 32-bit and 64-bit ELF) */
#define PT_LOAD   1 /* loadable program segment */
#define ET_EXEC   2 /* executable file */
#define ET_DYN    3 /* shared object / position-independent executable */
#define EM_386    3  /* Intel 80386 (32-bit x86) */
#define EM_X86_64 62 /* AMD x86-64 */

/* minimal ELF functionality for loading GRUB boot module */

/* Checks that 'elfFile' points to a well-formed ELF file for this
 * architecture, entirely contained within the first 'max_length'
 * bytes starting at 'elfFile' (i.e. within the boot module). This
 * only validates the ELF file itself; it does not check whether
 * seL4 can actually load it (see elf_getMemoryBounds()/elf_load()
 * and their callers for that). */
bool_t elf_checkFile(Elf_Header_t *elfFile, word_t max_length);

v_region_t elf_getMemoryBounds(Elf_Header_t *elfFile);

/* Loads the PT_LOAD segments of the ELF image at the given offset.
 * 'image_region' must be the v_region_t previously returned by
 * elf_getMemoryBounds() for this same ELF file; it is used to sanity
 * check that every segment being loaded lies where it is expected
 * to, rather than trusting the program headers a second time without
 * verification. Returns false (and loads nothing further) if a
 * segment is malformed. */
bool_t elf_load(Elf_Header_t *elfFile, seL4_Word offset, v_region_t image_region);

