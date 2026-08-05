/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/kernel/elf.h>
#include <linker.h>
#include <machine/io.h>

/* minimal ELF functionality for loading GRUB boot module */

BOOT_CODE bool_t elf_checkFile(Elf_Header_t *elfFile, word_t max_length)
{
    Elf_Phdr_t *phdr;
    uint32_t    i;
    word_t      phdr_table_size;

    /* We can't safely read any field until we know the header itself
     * fits inside the boot module. */
    if (max_length < sizeof(Elf_Header_t)) {
        return false;
    }

    if (elfFile->e_ident[0] != '\177' || elfFile->e_ident[1] != 'E' ||
        elfFile->e_ident[2] != 'L'    || elfFile->e_ident[3] != 'F' ||
        elfFile->e_ident[4] != (sizeof(word_t) == 8 ? 2 : 1)) {
        return false;
    }

    if (elfFile->e_machine != (sizeof(word_t) == 8 ? EM_X86_64 : EM_386)) {
        return false;
    }

    if (elfFile->e_type != ET_EXEC && elfFile->e_type != ET_DYN) {
        return false;
    }

    if (elfFile->e_phentsize != sizeof(Elf_Phdr_t) || elfFile->e_phnum == 0) {
        return false;
    }

    /* The program header table must be entirely within the boot
     * module. e_phnum is 16 bits and sizeof(Elf_Phdr_t) is small, so
     * this multiplication cannot overflow word_t; the bounds check
     * itself is written with subtraction so that it cannot overflow
     * either. */
    phdr_table_size = (word_t)elfFile->e_phnum * sizeof(Elf_Phdr_t);
    if (elfFile->e_phoff > max_length || phdr_table_size > max_length - elfFile->e_phoff) {
        return false;
    }

    /* Each segment's file contents must also be entirely within the
     * boot module. We don't trust p_filesz/p_memsz later on unless we
     * also don't trust what leads to them here. */
    phdr = (Elf_Phdr_t *)((paddr_t)elfFile + elfFile->e_phoff);
    for (i = 0; i < elfFile->e_phnum; i++) {
        if (phdr[i].p_type != PT_LOAD) {
            continue;
        }
        if (phdr[i].p_offset > max_length || phdr[i].p_filesz > max_length - phdr[i].p_offset) {
            return false;
        }
    }

    return true;
}

BOOT_CODE v_region_t elf_getMemoryBounds(Elf_Header_t *elfFile)
{
    Elf_Phdr_t *phdr = (Elf_Phdr_t *)((paddr_t)elfFile + elfFile->e_phoff);
    v_region_t elf_reg;
    vptr_t     sect_start;
    vptr_t     sect_end;
    uint32_t   i;

    elf_reg.start = (vptr_t) -1;
    elf_reg.end = 0;

    /* loop through all loadable program headers and record start/end address */
    for (i = 0; i < elfFile->e_phnum; i++) {
        if (phdr[i].p_type == PT_LOAD && phdr[i].p_memsz > 0) {
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

BOOT_CODE bool_t elf_load(Elf_Header_t *elfFile, seL4_Word offset, v_region_t image_region)
{
    Elf_Phdr_t *phdr = (Elf_Phdr_t *)((paddr_t)elfFile + elfFile->e_phoff);
    paddr_t    src;
    paddr_t    dst;
    word_t     len;
    uint32_t   i;

    /* loop through all program headers and load the loadable segments */
    for (i = 0; i < elfFile->e_phnum; i++) {
        if (phdr[i].p_type != PT_LOAD || phdr[i].p_memsz == 0) {
            continue;
        }

        /* A malformed segment could make the BSS-clearing step below
         * (which computes p_memsz - p_filesz) underflow, or describe a
         * destination outside of image_region (the region
         * elf_getMemoryBounds() computed and that was actually
         * allocated for this image). Both are checked up front using
         * subtraction rather than computing p_vaddr + p_memsz, so the
         * checks themselves cannot overflow. elf_checkFile() already
         * validated the file-offset side of things. */
        if (phdr[i].p_filesz > phdr[i].p_memsz ||
            phdr[i].p_vaddr < image_region.start ||
            phdr[i].p_vaddr > image_region.end ||
            phdr[i].p_memsz > image_region.end - phdr[i].p_vaddr) {
            printf("ELF load failed: segment %d is malformed\n", i);
            return false;
        }

        src = (paddr_t)elfFile + phdr[i].p_offset;
        dst = phdr[i].p_vaddr + offset;
        len = phdr[i].p_filesz;
        memcpy((void *)dst, (char *)src, len);
        dst += len;
        memset((void *)dst, 0, phdr[i].p_memsz - len);
    }

    return true;
}
