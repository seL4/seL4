/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define LOAD  lw
#define STORE sw

/* Contain the typical location of memory */
#define PADDR_BASE physBase
/* This represents the physical address that the kernel image will be linked to. This needs to
 * be on a 1gb boundary as we currently require being able to creating a mapping to this address
 * as the largest frame size */
#define PADDR_LOAD UL_CONST(0x84000000)
/* This is the base of the kernel window, which is directly mapped to PADDR_BASE */
#define PPTR_BASE  seL4_UserTop
/* This is the mapping of the kernel (mapped above the kernel window currently) */
#define KERNEL_BASE UL_CONST(0xFF800000)
#define KERNEL_ELF_BASE KERNEL_BASE
/* Start of kernel device mapping region in highest 4MiB of memory. */
#define KDEV_BASE UL_CONST(0xFFC00000)

#ifndef __ASSEMBLER__

#include <stdint.h>

static inline uint64_t riscv_read_time(void)
{
    uint32_t nH1, nL, nH2;
    asm volatile(
        "rdtimeh %0\n"
        "rdtime  %1\n"
        "rdtimeh %2\n"
        : "=r"(nH1), "=r"(nL), "=r"(nH2));
    if (nH1 < nH2) {
        /* Ensure that the time is correct if there is a rollover in the
         * high bits between reading the low and high bits. */
        asm volatile(
            "rdtime  %0\n"
            : "=r"(nL));
        nH1 = nH2;
    }
    return ((uint64_t)((uint64_t) nH1 << 32)) | (nL);
}

#endif /* __ASSEMBLER__ */

