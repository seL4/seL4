/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <arch/model/smp.h>
#include <stdint.h>
#include <plat/machine/devices_gen.h>

#ifdef CONFIG_RISCV_USE_CLINT_MTIME
/*
 * Currently all RISC-V 32-bit platforms supported have the mtime register
 * mapped at the same offset of the base address of the CLINT.
 */
#define CLINT_MTIME_OFFSET_LO 0xbff8
#define CLINT_MTIME_OFFSET_HI 0xbffc
#endif

static inline uint64_t riscv_read_time(void)
{
    word_t nH1, nL, nH2;

#ifdef CONFIG_RISCV_USE_CLINT_MTIME
    nH1 = *(volatile uint32_t *)(clint_pptr + CLINT_MTIME_OFFSET_HI);
    nL = *(volatile uint32_t *)(clint_pptr + CLINT_MTIME_OFFSET_LO);
    nH2 = *(volatile uint32_t *)(clint_pptr + CLINT_MTIME_OFFSET_HI);
    if (nH1 != nH2) {
        /* Ensure that the time is correct if there is a rollover in the
         * high bits between reading the low and high bits. */
        nL = *(volatile uint32_t *)(clint_pptr + CLINT_MTIME_OFFSET_LO);
    }
#else
    asm volatile(
        "rdtimeh %0\n"
        "rdtime  %1\n"
        "rdtimeh %2\n"
        : "=r"(nH1), "=r"(nL), "=r"(nH2));
    if (nH1 != nH2) {
        /* Ensure that the time is correct if there is a rollover in the
         * high bits between reading the low and high bits. */
        asm volatile("rdtime  %0\n" : "=r"(nL));
    }
#endif

    return (((uint64_t)nH2) << 32) | nL;
}


static inline uint64_t riscv_read_cycle(void)
{
    word_t nH1, nL, nH2;
    asm volatile(
        "rdcycleh %0\n"
        "rdcycle  %1\n"
        "rdcycleh %2\n"
        : "=r"(nH1), "=r"(nL), "=r"(nH2));
    if (nH1 != nH2) {
        /* Ensure that the cycles are correct if there is a rollover in the
         * high bits between reading the low and high bits. */
        asm volatile("rdcycle  %0\n" : "=r"(nL));
    }
    return (((uint64_t)nH2) << 32) | nL;
}
