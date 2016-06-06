/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_REGISTERSET_H
#define __ARCH_MACHINE_REGISTERSET_H

#include <config.h>
#include <arch/types.h>
#include <util.h>
#include <assert.h>

#include <mode/machine/registerset.h>

/* Minimum hardware-enforced alignment needed for FPU state. */
#define MIN_FPU_ALIGNMENT 64

/* X86 FPU context. */
struct user_fpu_state {
    uint8_t state[CONFIG_XSAVE_SIZE];
};
typedef struct user_fpu_state user_fpu_state_t;

/* X86 user-code context */
struct user_context {
    user_fpu_state_t fpuState;
    word_t registers[n_contextRegisters];
};
typedef struct user_context user_context_t;

void Arch_initContext(user_context_t* context);
word_t sanitiseRegister(register_t reg, word_t v);

/* Ensure FPU state is aligned within user context. */
compile_assert(fpu_state_alignment_valid,
               OFFSETOF(user_context_t, fpuState) % MIN_FPU_ALIGNMENT == 0)

#endif
