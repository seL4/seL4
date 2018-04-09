/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

OUTPUT_ARCH(riscv)
ENTRY(_start)

#include <config.h>

#if CONFIG_PT_LEVELS == 2
KERNEL_BASE  = 0xFFC00000;
#elif CONFIG_PT_LEVELS == 3
KERNEL_BASE  = 0xFFFFFFFF80000000;
#elif CONFIG_PT_LEVELS == 4
#error PT_LEVELS == 4 is not supported yet
#endif

/* WARNING: constants also defined in plat/machine/hardware.h */
PADDR_LOAD     = 0x00000000C0000000;
KERNEL_OFFSET = KERNEL_BASE - PADDR_LOAD;

SECTIONS
{
    . = KERNEL_BASE;

    .boot . : AT(ADDR(.boot) - KERNEL_OFFSET)
    {
        *(.boot.text)
        *(.boot.rodata)
        *(.boot.data)
        . = ALIGN(64K);
    }


    ki_boot_end = .;

    .text . : AT(ADDR(.text) - KERNEL_OFFSET)
    {
        /* Sit inside a large frame */
        . = ALIGN(4K);


        /* Standard kernel */
        *(.text)
    }

    /* Start of data section */
    _sdata = .;
    .sdata : {
        __global_pointer$ = . + 0x800;
        *(.sdata*)
    }
    .srodata : {
        *(.srodata*)
    }

    .rodata . : AT(ADDR(.rodata) - KERNEL_OFFSET)
    {
        *(.rodata)
        *(.rodata.*)
    }

    .data . : AT(ADDR(.data) - KERNEL_OFFSET)
    {
        *(.data)
    }

    .bss . : AT(ADDR(.bss) - KERNEL_OFFSET)
    {
        *(.bss)
        *(.sbss)

        /* 4k breakpoint stack */
        _breakpoint_stack_bottom = .;
        . = . + 4K;
        _breakpoint_stack_top = .;

        /* large data such as the globals frame and global PD */
        *(.bss.aligned)
        . = . + 8;
    }

    . = ALIGN(4K);
    ki_end = .;
}
