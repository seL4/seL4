/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/types.h>
#include <util.h>
#include <assert.h>

#include <mode/machine/registerset.h>

/* Minimum hardware-enforced alignment needed for FPU state. */
#define MIN_FPU_ALIGNMENT 64

#ifdef CONFIG_HARDWARE_DEBUG_API
/* X86 Debug register context */
struct user_debug_state {
    /* DR0-3 = Breakpoint linear address.
     * DR4-5 = reserved or aliased, depending on value of CR4.DE.
     * DR6 = Debug status register.
     * DR7 = Debug control register.
     */
    word_t dr[6];
    /* For each breakpoint currently being used by a thread, a bit in this
     * bitfield is set, and for each breakpoint that is cleared, a bit is
     * cleared. This enables an optimization: when a thread is being context-
     * switched to, we can check to see if it's using breakpoints, and
     * if so, we pop the whole register context.
     *
     * If it's not using breakpoints, we just pop all 0s into the ENABLED
     * bits in DR7.
     */
    uint32_t used_breakpoints_bf;

    /* The API supports stepping N instructions forward, where N can 1..N.
     * That feature is provided using this counter. Everytime a debug exception
     * occurs, the kernel will decrement, then check the counter, and only when
     * the counter is 0 will we deliver the fault to the userspace thread.
     */
    word_t n_instructions;

    /* This is part of the state machine that allows a thread to make
     * syscalls while being single-stepped. Basically helps the kernel to
     * disable single-stepping while executing the syscall, and then re-enable
     * it just before returning from the syscall into userspace.
     */
    bool_t single_step_enabled;
};
typedef struct user_debug_state user_breakpoint_state_t;
#endif /* CONFIG_HARDWARE_DEBUG_API */

/* X86 FPU context. */
struct user_fpu_state {
    uint8_t state[CONFIG_XSAVE_SIZE];
};
typedef struct user_fpu_state user_fpu_state_t;

/* X86 user-code context */
struct user_context {
    user_fpu_state_t fpuState;
    word_t registers[n_contextRegisters];
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_IA32)
    /* stored pointer to kernel stack used when kernel run in current TCB context. */
    word_t kernelSP;
#endif
#ifdef CONFIG_HARDWARE_DEBUG_API
    user_breakpoint_state_t breakpointState;
#endif
};
typedef struct user_context user_context_t;

void Mode_initContext(user_context_t *context);
void Arch_initContext(user_context_t *context);
word_t Mode_sanitiseRegister(regoff_t reg, word_t v);

/* Ensure FPU state is aligned within user context. */
unverified_compile_assert(fpu_state_alignment_valid,
                          OFFSETOF(user_context_t, fpuState) % MIN_FPU_ALIGNMENT == 0)

#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_IA32)
/* Ensure kernelSP is the first member following the registers. */
unverified_compile_assert(
    kernelSP_alignment_valid,
    OFFSETOF(user_context_t, kernelSP) - OFFSETOF(user_context_t, registers) == sizeof(word_t) * n_contextRegisters
)
#endif

