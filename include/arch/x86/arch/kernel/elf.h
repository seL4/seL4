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

#ifndef __ARCH_KERNEL_ELF_H_
#define __ARCH_KERNEL_ELF_H_

#include <types.h>
#include <mode/kernel/elf.h>

/* minimal ELF functionality for loading GRUB boot module */
bool_t elf_checkFile(Elf_Header_t* elfFile);
v_region_t elf_getMemoryBounds(Elf_Header_t* elfFile);
void elf_load(Elf_Header_t* elfFile, seL4_Word offset);

#endif /* __ARCH_KERNEL_ELF_H_ */
