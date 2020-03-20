/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <assert.h>

#ifdef CONFIG_HARDWARE_DEBUG_API

#define X86_DEBUG_BP_N_REGS                 (4)

static inline word_t readDr6Reg(void)
{
    word_t ret;

    asm volatile(
        "movq %%dr6, %0 \n\t"
        : "=r"(ret));
    return ret;
}

static inline void writeDr6Reg(word_t val)
{
    asm volatile(
        "movq %0, %%dr6 \n\t"
        :
        : "r"(val));
}

static inline word_t readDr7Reg(void)
{
    word_t ret;

    asm volatile(
        "movq %%dr7, %0 \n\t"
        : "=r"(ret));
    return ret;
}

static inline void writeDr7Reg(word_t val)
{
    asm volatile(
        "movq %0, %%dr7 \n\t"
        :
        : "r"(val));
}

static inline word_t readDrReg(uint8_t reg)
{
    word_t ret;

    assert(reg < X86_DEBUG_BP_N_REGS);
    switch (reg) {
    case 0:
        asm volatile("movq %%dr0, %0 \n\t" : "=r"(ret));
        break;
    case 1:
        asm volatile("movq %%dr1, %0 \n\t" : "=r"(ret));
        break;
    case 2:
        asm volatile("movq %%dr2, %0 \n\t" : "=r"(ret));
        break;
    default:
        asm volatile("movq %%dr3, %0 \n\t" : "=r"(ret));
        break;
    }
    return ret;
}

static inline void writeDrReg(uint8_t reg, word_t val)
{
    assert(reg < X86_DEBUG_BP_N_REGS);
    switch (reg) {
    case 0:
        asm volatile("movq %0, %%dr0 \n\t" :: "r"(val));
        break;
    case 1:
        asm volatile("movq %0, %%dr1 \n\t" :: "r"(val));
        break;
    case 2:
        asm volatile("movq %0, %%dr2 \n\t" :: "r"(val));
        break;
    default:
        asm volatile("movq %0, %%dr3 \n\t" :: "r"(val));
        break;
    }
}

/** Restore debug register context from a block of memory.
 *@param source The memory block from which to load the register values.
 */
static inline void loadBreakpointState(tcb_t *source)
{
    /* Order does matter when restoring the registers: we want to restore the
     * breakpoint control register (DR7) last since it is what "activates" the
     * effects of the configuration described by the other registers.
     */
    asm volatile(
        "movq %0, %%rdx \n\t"
        "movq (%%rdx), %%rcx \n\t"
        "movq %%rcx, %%dr0 \n\t"
        "addq $8, %%rdx \n\t"
        "movq (%%rdx), %%rcx \n\t"
        "movq %%rcx, %%dr1 \n\t"
        "addq $8, %%rdx \n\t"
        "movq (%%rdx), %%rcx \n\t"
        "movq %%rcx, %%dr2 \n\t"
        "addq $8, %%rdx \n\t"
        "movq (%%rdx), %%rcx \n\t"
        "movq %%rcx, %%dr3 \n\t"
        "addq $8, %%rdx \n\t"
        "movq (%%rdx), %%rcx \n\t"
        "movq %%rcx, %%dr6 \n\t"
        "addq $8, %%rdx \n\t"
        "movq (%%rdx), %%rcx \n\t"
        "movq %%rcx, %%dr7 \n\t"
        :
        : "r"(source->tcbArch.tcbContext.breakpointState.dr)
        : "rdx", "rcx");
}

#endif /* CONFIG_HARDWARE_DEBUG_API */
