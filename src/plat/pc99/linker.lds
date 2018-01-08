/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

ENTRY(_start)

/* WARNING: constants also defined in plat/machine/hardware.h */
#if defined(CONFIG_ARCH_IA32)
#undef i386
PADDR_BASE = 0x00000000;
PADDR_LOAD = 0x00100000;
KERNEL_BASE  = 0xe0000000;
OUTPUT_ARCH(i386)
OUTPUT_FORMAT(elf32-i386)
#elif defined(CONFIG_ARCH_X86_64)
PADDR_BASE = 0x00000000;
PADDR_LOAD = 0x00100000;
KERNEL_BASE = 0xffffffff80000000;
OUTPUT_FORMAT(elf64-x86-64)
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
nodeSkimScratchOffset = nodeSkimScratch - node_info;
#endif
#endif

KERNEL_OFFSET = KERNEL_BASE - PADDR_BASE;

SECTIONS
{
    /* load kernel to 1M to avoid the famous IA-32 memory holes below */
    . = PADDR_LOAD;

    /* code/data only needed during bootstrapping, linked to physical addresses */

    .phys . :
    {
        *(.mbh)
        *(.phys.text)
        *(.phys.data)
        . = ALIGN(4K);
    }

    .phys.bss ADDR(.phys) + SIZEOF(.phys) (NOLOAD) :
    {
        boot_stack_bottom = .;
        . = . + 2K;
        . = ALIGN(4K);
        boot_stack_top = .;
        *(.phys.bss)
    }

    . = . + KERNEL_OFFSET;

    .boot . : AT(ADDR(.boot) - KERNEL_OFFSET)
    {
        *(.boot.text)
        *(.boot.data)
    }

    .boot.bss . (NOLOAD) : AT(ADDR(.boot.bss) - KERNEL_OFFSET)
    {
        *(.boot.bss)
        . = ALIGN(4K);
    }
#ifdef CONFIG_KERNEL_SKIM_WINDOW
    /* Align up so that the SKIM portion of the kernel is by itself
       on large pages */
#ifdef CONFIG_ARCH_IA32
    . = ALIGN(4M);
#else
    . = ALIGN(2M);
#endif
#endif

    ki_boot_end = .;
    ki_skim_start = .;

    .text . : AT(ADDR(.text) - KERNEL_OFFSET)
    {
        *(.text)
    }

    .rodata . : AT(ADDR(.rodata) - KERNEL_OFFSET)
    {
        *(.rodata)
        *(.rodata.*)
    }

    .skim_data . : AT(ADDR(.skim_data) - KERNEL_OFFSET)
    {
        *(.skim.data)
        *(.skim.data.*)
    }

    .skim_bss . : AT(ADDR(.skim_bss) - KERNEL_OFFSET)
    {
        *(.skim.bss)
        *(.skim.bss.*)
    }

#ifdef CONFIG_KERNEL_SKIM_WINDOW
    /* Align up so that the SKIM portion of the kernel is by itself
       on large pages */
#ifdef CONFIG_ARCH_IA32
    . = ALIGN(4M);
#else
    . = ALIGN(2M);
#endif
#endif
    ki_skim_end = .;

    .data . : AT(ADDR(.data) - KERNEL_OFFSET)
    {
        *(.data)
    }

    .bss . : AT(ADDR(.bss) - KERNEL_OFFSET)
    {
        *(.bss)
        *(COMMON)
    }

    .ehframe : AT(ADDR(.ehframe) - KERNEL_OFFSET)
    {
        _ehframe = .;
        *(.eh_frame)
        . = ALIGN(4K);
    }

    . = ALIGN(4K);
    ki_end = .;

    /DISCARD/ :
    {
        *(.note.gnu.build-id)
        *(.comment)
    }
}
