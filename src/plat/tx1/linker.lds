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

#include <config.h>

ENTRY(_start)

/* WARNING: constants also defined in plat/machine/hardware.h */
PHYS_BASE = 0x80000000;
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
KERNEL_BASE = 0x0000ff8080000000;
#else
KERNEL_BASE = 0xffffff8000000000;
#endif
KERNEL_OFFSET = KERNEL_BASE - PHYS_BASE;

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
        . = ALIGN(64K);
        *(.vectors)

        /* Fastpath code */
        *(.vectors.fastpath_call)
        *(.vectors.fastpath_reply_recv)
        *(.vectors.text)

        /* Anything else that should be in the vectors page. */
        *(.vectors.*)

        /* Hopefully all that fits into 4K! */

        /* Standard kernel */
        *(.text)
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

        /* 4k breakpoint stack */
        _breakpoint_stack_bottom = .;
        . = . + 4K;
        _breakpoint_stack_top = .;
        
        /* large data such as the globals frame and global PD */
        *(.bss.aligned)
    }

    . = ALIGN(4K);
    ki_end = .;

    /DISCARD/ :
    {
        *(.note.gnu.build-id)
        *(.comment)
    }
}

