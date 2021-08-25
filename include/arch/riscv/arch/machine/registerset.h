/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include "hardware.h"

#ifndef __ASSEMBLER__

#include <stdint.h>
#include <util.h>
#include <arch/types.h>

enum _register {

    ra = 0, LR = 0,

    sp = 1, SP = 1,
    gp = 2, GP = 2,
    tp = 3, TP = 3,
    TLS_BASE = tp,

    t0 = 4,
#ifdef CONFIG_KERNEL_MCS
    nbsendRecvDest = 4,
#endif
    t1 = 5,
    t2 = 6,
    s0 = 7,
    s1 = 8,

    /* x10-x17 > a0-a7 */
    a0 = 9, capRegister = 9, badgeRegister = 9,
    a1 = 10, msgInfoRegister = 10,
    a2 = 11,
    a3 = 12,
    a4 = 13,
    a5 = 14,
    a6 = 15,
#ifdef CONFIG_KERNEL_MCS
    replyRegister = 15,
#endif
    a7 = 16,
    s2 = 17,
    s3 = 18,
    s4 = 19,
    s5 = 20,
    s6 = 21,
    s7 = 22,
    s8 = 23,
    s9 = 24,
    s10 = 25,
    s11 = 26,

    t3 = 27,
    t4 = 28,
    t5 = 29,
    t6 = 30,

    /* End of GP registers, the following are additional kernel-saved state. */
    SCAUSE = 31,
    SSTATUS = 32,
    FaultIP = 33, /* SEPC */
    NextIP = 34,

    /* TODO: add other user-level CSRs if needed (i.e. to avoid channels) */

    n_contextRegisters
};

typedef uint8_t register_t;

enum messageSizes {
    n_msgRegisters = 4,
    n_frameRegisters = 16,
    n_gpRegisters = 16,
    n_exceptionMessage = 2,
    n_syscallMessage = 10,
#ifdef CONFIG_KERNEL_MCS
    n_timeoutMessage = 32,
#endif
};

extern const register_t msgRegisters[] VISIBLE;
extern const register_t frameRegisters[] VISIBLE;
extern const register_t gpRegisters[] VISIBLE;

#ifdef CONFIG_HAVE_FPU

#define RISCV_NUM_FP_REGS   32

#if defined(CONFIG_RISCV_EXT_D)
typedef uint64_t fp_reg_t;
#elif defined(CONFIG_RISCV_EXT_F)
typedef uint32_t fp_reg_t;
#else
#error Unknown RISCV FPU extension
#endif

typedef struct user_fpu_state {
    fp_reg_t regs[RISCV_NUM_FP_REGS];
    uint32_t fcsr;
} user_fpu_state_t;

#endif

struct user_context {
    word_t registers[n_contextRegisters];
#ifdef CONFIG_HAVE_FPU
    user_fpu_state_t fpuState;
#endif
};
typedef struct user_context user_context_t;

static inline void Arch_initContext(user_context_t *context)
{
    /* Enable supervisor interrupts (when going to user-mode) */
    context->registers[SSTATUS] = SSTATUS_SPIE;
}

static inline word_t CONST sanitiseRegister(register_t reg, word_t v, bool_t archInfo)
{
    return v;
}


#define EXCEPTION_MESSAGE \
 {\
    [seL4_UserException_FaultIP] = FaultIP,\
    [seL4_UserException_SP] = SP,\
 }

#define SYSCALL_MESSAGE \
{\
    [seL4_UnknownSyscall_FaultIP] = FaultIP,\
    [seL4_UnknownSyscall_SP] = SP,\
    [seL4_UnknownSyscall_RA] = LR,\
    [seL4_UnknownSyscall_A0] = a0,\
    [seL4_UnknownSyscall_A1] = a1,\
    [seL4_UnknownSyscall_A2] = a2,\
    [seL4_UnknownSyscall_A3] = a3,\
    [seL4_UnknownSyscall_A4] = a4,\
    [seL4_UnknownSyscall_A5] = a5,\
    [seL4_UnknownSyscall_A6] = a6,\
}

#define TIMEOUT_REPLY_MESSAGE \
{\
    [seL4_TimeoutReply_FaultIP] = FaultIP,\
    [seL4_TimeoutReply_LR] = LR, \
    [seL4_TimeoutReply_SP] = SP, \
    [seL4_TimeoutReply_GP] = GP, \
    [seL4_TimeoutReply_s0] = s0, \
    [seL4_TimeoutReply_s1] = s1, \
    [seL4_TimeoutReply_s2] = s2, \
    [seL4_TimeoutReply_s3] = s3, \
    [seL4_TimeoutReply_s4] = s4, \
    [seL4_TimeoutReply_s5] = s5, \
    [seL4_TimeoutReply_s6] = s6, \
    [seL4_TimeoutReply_s7] = s7, \
    [seL4_TimeoutReply_s8] = s8, \
    [seL4_TimeoutReply_s9] = s9, \
    [seL4_TimeoutReply_s10] = s10, \
    [seL4_TimeoutReply_s11] = s11, \
    [seL4_TimeoutReply_a0] = a0, \
    [seL4_TimeoutReply_a1] = a1, \
    [seL4_TimeoutReply_a2] = a2, \
    [seL4_TimeoutReply_a3] = a3, \
    [seL4_TimeoutReply_a4] = a4, \
    [seL4_TimeoutReply_a5] = a5, \
    [seL4_TimeoutReply_a6] = a6, \
    [seL4_TimeoutReply_a7] = a7, \
    [seL4_TimeoutReply_t0] = t0, \
    [seL4_TimeoutReply_t1] = t1, \
    [seL4_TimeoutReply_t2] = t2, \
    [seL4_TimeoutReply_t3] = t3, \
    [seL4_TimeoutReply_t4] = t4, \
    [seL4_TimeoutReply_t5] = t5, \
    [seL4_TimeoutReply_t6] = t6, \
    [seL4_TimeoutReply_TP] = TP, \
}

#endif /* __ASSEMBLER__ */

