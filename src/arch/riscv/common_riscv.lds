/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 * Copyright 2023, DornerWorks
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

OUTPUT_ARCH(riscv)
ENTRY(_start)

#include <config.h>
#define __ASSEMBLER__
#include <hardware.h>
#include <sel4/plat/api/constants.h>
#include <plat/machine/devices_gen.h>

KERNEL_OFFSET = KERNEL_ELF_BASE_RAW - KERNEL_ELF_PADDR_BASE_RAW;

SECTIONS
{
    . = KERNEL_ELF_BASE_RAW;

    .boot.text . : AT(ADDR(.boot.text) - KERNEL_OFFSET)
    {
        *(.boot.text)
    }
    .boot.rodata . : AT(ADDR(.boot.rodata) - KERNEL_OFFSET)
    {
        *(.boot.rodata)
    }
    .boot.data . : AT(ADDR(.boot.data) - KERNEL_OFFSET)
    {
        *(.boot.data)
    }
    .boot.bss . : AT(ADDR(.boot.bss) - KERNEL_OFFSET)
    {
        *(.boot.bss)
    }
    . = ALIGN(4K);

    ki_boot_end = .;

    .text . : AT(ADDR(.text) - KERNEL_OFFSET)
    {
        . = ALIGN(4K);


        /* Standard kernel */
        *(.text)
    }

    /* Start of data section */
    _sdata = .;
    .small : {
        /* Small data that should be accessed relative to gp.  ld has trouble
           with the relaxation if they are not in a single section. */
        __global_pointer$ = . + 0x800;
        *(.srodata*)
        *(.sdata*)
        *(.sbss)
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

    /* The kernel's idle thread section contains no code or data. */
    ._idle_thread . (NOLOAD): AT(ADDR(._idle_thread) - KERNEL_OFFSET)
    {
        *(._idle_thread)
    }

    .bss . (NOLOAD): AT(ADDR(.bss) - KERNEL_OFFSET)
    {
        *(.bss)
        *(COMMON) /* fallback in case '-fno-common' is not used */

        /* 4k breakpoint stack */
        _breakpoint_stack_bottom = .;
        . = . + 4K;
        _breakpoint_stack_top = .;

        /* large data such as the globals frame and global PD */
        *(.bss.aligned)
#if defined(CONFIG_PLAT_ROCKETCHIP_ZCU102)
        /* softcore instantiation on ZCU102 needs these bytes to
         * avoid userspace issues
         */
        . = . + 8;
#endif
    }

    . = ALIGN(4K);
    ki_end = .;
}
