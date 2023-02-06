/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 * 
 * Derived from:
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

OUTPUT_ARCH(loongarch)
ENTRY(_start)

#include <config.h>
#define __ASSEMBLER__
#include <hardware.h>
#include <sel4/plat/api/constants.h>
#include <plat/machine/devices_gen.h>

KERNEL_OFFSET = KERNEL_ELF_BASE - KERNEL_ELF_PADDR_BASE;

SECTIONS
{
    . = KERNEL_ELF_BASE;

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
    . = ALIGN(16k);

    ki_boot_end = .;

    .text . : AT(ADDR(.text) - KERNEL_OFFSET)
    {
        . = ALIGN(16k);
        /* Standard kernel */
        *(.text)
        . = ALIGN(16k);
        kernel_tlbrefillentry = .;
        *(kernel_tlbrefillentry)
        . = ALIGN(16k);
        _k_eentry = .;
        *(k_eentry)
    }
    
    . = ALIGN(16k);
    /* Start of data section */
    _sdata = .;
    .sdata : {
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

    . = ALIGN(16K);
    __bss_start = .;
    .bss . : AT(ADDR(.bss) - KERNEL_OFFSET)
    {
        *(.bss)
        *(COMMON) /* fallback in case '-fno-common' is not used */
        *(.sbss)

        /* 16k breakpoint stack */
        _breakpoint_stack_bottom = .;
        . = . + 16K;
        _breakpoint_stack_top = .;

        /* large data such as the globals frame and global PD */
        *(.bss.aligned)
    }
    . = ALIGN(16K);
    __bss_stop = .;

    . = ALIGN(16K);
    ki_end = .;
    
}
