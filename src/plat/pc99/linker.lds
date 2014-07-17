/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

ENTRY(_start)

/* WARNING: constants also defined in plat/machine/hardware.h */
PADDR_BASE = 0x00000000;
PADDR_LOAD = 0x00100000;
PPTR_BASE  = 0xe0000000;
BASE_OFFSET = PPTR_BASE - PADDR_BASE;

SECTIONS
{
    /* load kernel to 1M to avoid the famous IA-32 memory holes below */
    . = PADDR_LOAD;

    /* code/data only needed during bootstrapping, linked to physical addresses */

    .phys . :
    {
        *(.mbh)
        *(.phys.text)
        _boot_stack_bottom = .;
        . = . + 2K;
        . = ALIGN(4K);
        _boot_stack_top = .;
        _boot_pd = .;
        . = . + 4K;
    }

    . = . + BASE_OFFSET;

    .boot . : AT(ADDR(.boot) - BASE_OFFSET)
    {
        *(.boot.text)
        *(.boot.glob)
        . = ALIGN(4K);
    }

    ki_boot_end = .;

    .text . : AT(ADDR(.text) - BASE_OFFSET)
    {
        *(.text)
    }

    .rodata . : AT(ADDR(.rodata) - BASE_OFFSET)
    {
        *(.rodata)
        *(.rodata.*)
    }

    .glob . : AT(ADDR(.glob) - BASE_OFFSET)
    {
        *(.glob)
    }
    
    .data . : AT(ADDR(.data) - BASE_OFFSET)
    {
        *(.data)
    }

    . = ALIGN(4K);
    ki_end = .;

    /* The NDKS section is allocated and mapped per node in boot_sys.c */

    .ndks 0xffc01000 (DSECT) :
    {
        _ndks_start = .;

        /*
         * Due to stupid ld behaviour, we can't simply allocate the stack here
         * using ". = . + ALIGN(4K)" because that makes .ndks appear in the
         * program headers of the ELF file (which is wrong).
         *
         * Instead, we define the stack in C in the .ndks.stack section, which
         * makes it appear here.
         */
        _kernel_stack_bottom = .;
        *(.ndks.stack)
        _kernel_stack_top = .;

        *(.boot.node)
        *(.bss)
        *(COMMON)
        _ndks_end = .;
    }

    /DISCARD/ :
    {
        *(.note.gnu.build-id)
        *(.comment)
    }
}
