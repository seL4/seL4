/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/kernel/elf.h>
#include <arch/linker.h>

/* minimal ELF functionality for loading GRUB boot module */

BOOT_CODE bool_t
elf32_checkFile(Elf32_Header_t* elfFile)
{
    return (
               elfFile->e_ident[0] == '\177' &&
               elfFile->e_ident[1] == 'E'    &&
               elfFile->e_ident[2] == 'L'    &&
               elfFile->e_ident[3] == 'F'    &&
               elfFile->e_ident[4] == 1
           );
}

BOOT_CODE v_region_t
elf32_getMemoryBounds(Elf32_Header_t* elfFile)
{
    Elf32_Phdr_t* phdr = (Elf32_Phdr_t*)((paddr_t)elfFile + elfFile->e_phoff);
    v_region_t elf_reg;
    vptr_t     sect_start;
    vptr_t     sect_end;
    uint32_t   i;

    elf_reg.start = 0xffffffff;
    elf_reg.end = 0;

    /* loop through all program headers (segments) and record start/end address */
    for (i = 0; i < elfFile->e_phnum; i++) {
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

BOOT_CODE void
elf32_load(Elf32_Header_t* elfFile, int32_t offset)
{
    Elf32_Phdr_t* phdr = (Elf32_Phdr_t*)((paddr_t)elfFile + elfFile->e_phoff);
    paddr_t       src;
    paddr_t       dst;
    uint32_t      len;
    uint32_t      i;

    /* loop through all program headers (segments) and load them */
    for (i = 0; i < elfFile->e_phnum; i++) {
        src = (paddr_t)elfFile + phdr[i].p_offset;
        dst = phdr[i].p_vaddr + offset;
        len = phdr[i].p_filesz;
        memcpy((void*)dst, (char*)src, len);
        dst += len;
        memset((void*)dst, 0, phdr[i].p_memsz - len);
    }
}
