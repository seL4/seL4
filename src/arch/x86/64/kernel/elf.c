/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/kernel/elf.h>
#include <linker.h>

BOOT_CODE bool_t elf_checkFile(Elf64_Header_t *elf)
{
    return (
               elf->e_ident[0] == '\177' &&
               elf->e_ident[1] == 'E'    &&
               elf->e_ident[2] == 'L'    &&
               elf->e_ident[3] == 'F'    &&
               elf->e_ident[4] == 2
           );
}


BOOT_CODE v_region_t elf_getMemoryBounds(Elf64_Header_t *elf)
{
    v_region_t  elf_reg;
    vptr_t      sect_start;
    vptr_t      sect_end;
    uint32_t    i;
    Elf64_Phdr_t *phdr = (Elf64_Phdr_t *)((paddr_t)elf + elf->e_phoff);

    elf_reg.start = 0x7fffffffffffffffUL;
    elf_reg.end = 0;

    for (i = 0; i < elf->e_phnum; i++) {
        if (phdr[i].p_memsz > 0) {
            sect_start = phdr[i].p_vaddr;
            sect_end = sect_start + phdr[i].p_memsz;
            if (sect_start < elf_reg.start) {
                elf_reg.start = sect_start;
            }
            if (sect_end > elf_reg.end) {
                elf_reg.end = sect_end;
            }
        }
    }

    return elf_reg;
}

BOOT_CODE void elf_load(Elf64_Header_t *elf, seL4_Word offset)
{
    paddr_t     src;
    paddr_t     dst;
    uint64_t    len;
    uint32_t    i;
    Elf64_Phdr_t *phdr = (Elf64_Phdr_t *)((paddr_t)elf + elf->e_phoff);

    for (i = 0; i < elf->e_phnum; i++) {
        src = (paddr_t)elf + phdr[i].p_offset;
        dst = phdr[i].p_vaddr + offset;
        len = phdr[i].p_filesz;
        memcpy((void *)dst, (char *)src, len);
        dst += len;
        memset((void *)dst, 0, phdr[i].p_memsz - len);
    }
}
