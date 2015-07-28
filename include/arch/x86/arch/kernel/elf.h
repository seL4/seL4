/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_ELF_H
#define __ARCH_KERNEL_ELF_H

#include <types.h>

/* minimal ELF structures needed for loading GRUB boot module */

typedef struct Elf32_Header {
    unsigned char e_ident[16];
    uint16_t      e_type;      /* Relocatable=1, Executable=2 (+ some more ..) */
    uint16_t      e_machine;   /* Target architecture: MIPS=8 */
    uint32_t      e_version;   /* Elf version (should be 1) */
    uint32_t      e_entry;     /* Code entry point */
    uint32_t      e_phoff;     /* Program header table */
    uint32_t      e_shoff;     /* Section header table */
    uint32_t      e_flags;     /* Flags */
    uint16_t      e_ehsize;    /* ELF header size */
    uint16_t      e_phentsize; /* Size of one program segment header */
    uint16_t      e_phnum;     /* Number of program segment headers */
    uint16_t      e_shentsize; /* Size of one section header */
    uint16_t      e_shnum;     /* Number of section headers */
    uint16_t      e_shstrndx;  /* Section header index of the string table for section header names */
} Elf32_Header_t;

typedef struct Elf32_Phdr {
    uint32_t      p_type;      /* Segment type: Loadable segment = 1 */
    uint32_t      p_offset;    /* Offset of segment in file */
    uint32_t      p_vaddr;     /* Reqd virtual address of segment when loading */
    uint32_t      p_paddr;     /* Reqd physical address of segment (ignore) */
    uint32_t      p_filesz;    /* How many bytes this segment occupies in file */
    uint32_t      p_memsz;     /* How many bytes this segment should occupy in memory */
    uint32_t      p_flags;     /* Flags: logical "or" of PF_ constants below */
    uint32_t      p_align;     /* Reqd alignment of segment in memory */
} Elf32_Phdr_t;

/* minimal ELF functionality for loading GRUB boot module */

bool_t elf32_checkFile(Elf32_Header_t* elfFile);
v_region_t elf32_getMemoryBounds(Elf32_Header_t* elfFile);
void elf32_load(Elf32_Header_t* elfFile, int32_t offset);

#endif
