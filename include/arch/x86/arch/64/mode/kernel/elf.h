/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODE_KERNEL_ELF_H_
#define __ARCH_MODE_KERNEL_ELF_H_

#include <types.h>

/* minimal ELF structures needed for loading GRUB boot module */
typedef struct Elf64_Header {
    unsigned char       e_ident[16];
    uint16_t            e_type;
    uint16_t            e_machine;
    uint32_t            e_version;
    uint64_t            e_entry;
    uint64_t            e_phoff;
    uint64_t            e_shoff;
    uint32_t            e_flags;
    uint16_t            e_ehsize;
    uint16_t            e_phentsize;
    uint16_t            e_phnum;
    uint16_t            e_shentsize;
    uint16_t            e_shnum;
    uint16_t            e_shstrndx;
} Elf64_Header_t, Elf_Header_t;

typedef struct Elf64_Phdr {
    uint32_t            p_type;
    uint32_t            p_flags;
    uint64_t            p_offset;
    uint64_t            p_vaddr;
    uint64_t            p_paddr;
    uint64_t            p_filesz;
    uint64_t            p_memsz;
    uint64_t            p_align;
} Elf64_Phdr_t, Elf_Phdr_t;

#endif /* __ARCH_MODE_KERNEL_ELF_H_ */
