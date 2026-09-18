/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#define __ASSEMBLER__
#include <hardware.h>
#include <sel4/plat/api/constants.h>
#include <plat/machine/devices_gen.h>

ENTRY(_start)

SECTIONS
{
    . = KERNEL_ELF_BASE;

    .boot . :
    {
        *(.boot.text)
        *(.boot.rodata)
        *(.boot.data)
        . = ALIGN(64K);
    }

    ki_boot_end = .;

    .text . :
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

    .rodata . :
    {
        *(.rodata)
        *(.rodata.*)
    }

    .data . :
    {
        *(.data)
    }

    .bss . (NOLOAD) :
    {
        *(.bss)
        *(COMMON) /* fallback in case '-fno-common' is not used */

#if defined(CONFIG_ARCH_AARCH32) && !defined(CONFIG_ARM_HYPERVISOR_SUPPORT)
        /* 4k abort stack, needed for PMODE_ABORT on ARM32 */
        _abort_stack_bottom = .;
        . = . + 4K * CONFIG_MAX_NUM_NODES;
        _abort_stack_top = .;
#endif

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
