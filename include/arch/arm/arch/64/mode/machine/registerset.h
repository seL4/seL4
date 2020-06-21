/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

/* CurrentEL register */
#define PEXPL1                  (1 << 2)
#define PEXPL2                  (1 << 3)

/* PSTATE register */
#define PMODE_FIRQ              (1 << 6)
#define PMODE_IRQ               (1 << 7)
#define PMODE_SERROR            (1 << 8)
#define PMODE_DEBUG             (1 << 9)
#define PMODE_EL0t              0
#define PMODE_EL1t              4
#define PMODE_EL1h              5
#define PMODE_EL2h              9

/* DAIF register */
#define DAIF_FIRQ               (1 << 6)
#define DAIF_IRQ                (1 << 7)
#define DAIF_SERROR             (1 << 8)
#define DAIF_DEBUG              (1 << 9)
#define DAIFSET_MASK            0xf

/* ESR register */
#define ESR_EC_SHIFT            26
#define ESR_EC_LEL_DABT         0x24    // Data abort from a lower EL
#define ESR_EC_CEL_DABT         0x25    // Data abort from the current EL
#define ESR_EC_LEL_IABT         0x20    // Instruction abort from a lower EL
#define ESR_EC_CEL_IABT         0x21    // Instruction abort from the current EL
#define ESR_EC_LEL_SVC64        0x15    // SVC from a lower EL in AArch64 state
#define ESR_EC_LEL_HVC64        0x16    // HVC from EL1 in AArch64 state
#define ESR_EL1_EC_ENFP         0x7     // Access to Advanced SIMD or floating-point registers


/* ID_AA64PFR0_EL1 register */
#define ID_AA64PFR0_EL1_FP      16     // HWCap for Floating Point
#define ID_AA64PFR0_EL1_ASIMD   20     // HWCap for Advanced SIMD

/* CPACR_EL1 register */
#define CPACR_EL1_FPEN          20     // FP regiters access

/*
 * We cannot allow async aborts in the verified kernel, but they are useful
 * in identifying invalid memory access bugs so we enable them in debug mode.
 */
#ifdef CONFIG_DEBUG_BUILD
#define PSTATE_EXTRA_FLAGS  0
#else
#define PSTATE_EXTRA_FLAGS  PMODE_SERROR
#endif

#define PSTATE_USER         (PMODE_FIRQ | PMODE_EL0t | PSTATE_EXTRA_FLAGS)

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PSTATE_IDLETHREAD   (PMODE_FIRQ | PMODE_EL2h | PSTATE_EXTRA_FLAGS)
#else
#define PSTATE_IDLETHREAD   (PMODE_FIRQ | PMODE_EL1h | PSTATE_EXTRA_FLAGS)
#endif

/* Offsets within the user context, these need to match the order in
 * register_t below */
#define PT_LR                       (30 * 8)
#define PT_SP_EL0                   (31 * 8)
#define PT_ELR_EL1                  (32 * 8)
#define PT_SPSR_EL1                 (33 * 8)
#define PT_FaultIP                  (34 * 8)
#define PT_TPIDR_EL0                (35 * 8)

#ifndef __ASSEMBLER__ /* C only definitions */

#include <config.h>
#include <stdint.h>
#include <assert.h>
#include <util.h>
#include <arch/types.h>
#include <sel4/plat/api/constants.h>

/* These are the indices of the registers in the saved thread context.
 * The values are determined by the order in which they're saved in the trap handler. */
enum _register {
    X0                          = 0,    /* 0x00 */
    capRegister                 = 0,
    badgeRegister               = 0,

    X1                          = 1,    /* 0x08 */
    msgInfoRegister             = 1,

    X2                          = 2,    /* 0x10 */
    X3                          = 3,    /* 0x18 */
    X4                          = 4,    /* 0x20 */
    X5                          = 5,    /* 0x28 */
    X6                          = 6,    /* 0x30 */
#ifdef CONFIG_KERNEL_MCS
    replyRegister               = 6,
#endif
    X7                          = 7,    /* 0x38 */
    X8                          = 8,    /* 0x40 */
#ifdef CONFIG_KERNEL_MCS
    nbsendRecvDest              = 8,
#endif
    X9                          = 9,    /* 0x48 */
    X10                         = 10,   /* 0x50 */
    X11                         = 11,   /* 0x58 */
    X12                         = 12,   /* 0x60 */
    X13                         = 13,   /* 0x68 */
    X14                         = 14,   /* 0x70 */
    X15                         = 15,   /* 0x78 */
    X16                         = 16,   /* 0x80 */
    X17                         = 17,   /* 0x88 */
    X18                         = 18,   /* 0x90 */
    X19                         = 19,   /* 0x98 */
    X20                         = 20,   /* 0xa0 */
    X21                         = 21,   /* 0xa8 */
    X22                         = 22,   /* 0xb0 */
    X23                         = 23,   /* 0xb8 */
    X24                         = 24,   /* 0xc0 */
    X25                         = 25,   /* 0xc8 */
    X26                         = 26,   /* 0xd0 */
    X27                         = 27,   /* 0xd8 */
    X28                         = 28,   /* 0xe0 */
    X29                         = 29,   /* 0xe8 */

    X30                         = 30,   /* 0xf0 */
    LR                          = 30,

    /* End of GP registers, the following are additional kernel-saved state. */

    SP_EL0                      = 31,   /* 0xf8 */
    ELR_EL1                     = 32,   /* 0x100 */
    NextIP                      = 32,   /* LR_svc */
    SPSR_EL1                    = 33,   /* 0x108 */

    FaultIP                     = 34,   /* 0x110 */
    /* user readable/writable thread ID register.
     * name comes from the ARM manual */
    TPIDR_EL0                   = 35,
    TLS_BASE                    = TPIDR_EL0,
    /* user readonly thread ID register. */
    TPIDRRO_EL0                 = 36,
    n_contextRegisters          = 37,
};

#define NEXT_PC_REG ELR_EL1

compile_assert(sp_offset_correct, SP_EL0 *sizeof(word_t) == PT_SP_EL0)
compile_assert(lr_svc_offset_correct, ELR_EL1 *sizeof(word_t) == PT_ELR_EL1)
compile_assert(faultinstruction_offset_correct, FaultIP *sizeof(word_t) == PT_FaultIP)

typedef word_t register_t;

enum messageSizes {
    n_msgRegisters = seL4_FastMessageRegisters,
    n_frameRegisters = 17,
    n_gpRegisters = 19,
    n_exceptionMessage = 3,
    n_syscallMessage = 12,
#ifdef CONFIG_KERNEL_MCS
    n_timeoutMessage = 34,
#endif
};

#define EXCEPTION_MESSAGE \
 {\
    [seL4_UserException_FaultIP] = FaultIP,\
    [seL4_UserException_SP] = SP_EL0,\
    [seL4_UserException_SPSR] = SPSR_EL1\
 }

#define SYSCALL_MESSAGE \
{\
    [seL4_UnknownSyscall_X0] = X0,\
    [seL4_UnknownSyscall_X1] = X1,\
    [seL4_UnknownSyscall_X2] = X2,\
    [seL4_UnknownSyscall_X3] = X3,\
    [seL4_UnknownSyscall_X4] = X4,\
    [seL4_UnknownSyscall_X5] = X5,\
    [seL4_UnknownSyscall_X6] = X6,\
    [seL4_UnknownSyscall_X7] = X7,\
    [seL4_UnknownSyscall_FaultIP] = FaultIP,\
    [seL4_UnknownSyscall_SP] = SP_EL0,\
    [seL4_UnknownSyscall_LR] = ELR_EL1,\
    [seL4_UnknownSyscall_SPSR] = SPSR_EL1\
}

#define TIMEOUT_REPLY_MESSAGE \
{\
    [seL4_TimeoutReply_FaultIP] = FaultIP,\
    [seL4_TimeoutReply_SP] = SP_EL0,\
    [seL4_TimeoutReply_SPSR_EL1] = SPSR_EL1,\
    [seL4_TimeoutReply_X0] = X0,\
    [seL4_TimeoutReply_X1] = X1,\
    [seL4_TimeoutReply_X2] = X2,\
    [seL4_TimeoutReply_X3] = X3,\
    [seL4_TimeoutReply_X4] = X4,\
    [seL4_TimeoutReply_X5] = X5,\
    [seL4_TimeoutReply_X6] = X6,\
    [seL4_TimeoutReply_X7] = X7,\
    [seL4_TimeoutReply_X8] = X8,\
    [seL4_TimeoutReply_X16] = X16,\
    [seL4_TimeoutReply_X17] = X17,\
    [seL4_TimeoutReply_X18] = X18,\
    [seL4_TimeoutReply_X29] = X29,\
    [seL4_TimeoutReply_X30] = X30,\
    [seL4_TimeoutReply_X9] = X9,\
    [seL4_TimeoutReply_X10] = X10,\
    [seL4_TimeoutReply_X11] = X11,\
    [seL4_TimeoutReply_X12] = X12,\
    [seL4_TimeoutReply_X13] = X13,\
    [seL4_TimeoutReply_X14] = X14,\
    [seL4_TimeoutReply_X15] = X15,\
    [seL4_TimeoutReply_X19] = X19,\
    [seL4_TimeoutReply_X20] = X20,\
    [seL4_TimeoutReply_X21] = X21,\
    [seL4_TimeoutReply_X22] = X22,\
    [seL4_TimeoutReply_X23] = X23,\
    [seL4_TimeoutReply_X24] = X24,\
    [seL4_TimeoutReply_X25] = X25,\
    [seL4_TimeoutReply_X26] = X26,\
    [seL4_TimeoutReply_X27] = X27,\
    [seL4_TimeoutReply_X28] = X28,\
}

extern const register_t msgRegisters[];
extern const register_t frameRegisters[];
extern const register_t gpRegisters[];

#ifdef CONFIG_HAVE_FPU
typedef struct user_fpu_state {
    uint64_t vregs[64];
    uint32_t fpsr;
    uint32_t fpcr;
} user_fpu_state_t;
#endif /* CONFIG_HAVE_FPU */

/* ARM user-code context: size = 72 bytes
 * Or with hardware debug support built in:
 *      72 + sizeof(word_t) * (NUM_BPS + NUM_WPS) * 2
 *
 * The "word_t registers" member of this struct must come first, because in
 * head.S, we assume that an "ldr %0, =ksCurThread" will point to the beginning
 * of the current thread's registers. The assert below should help.
 */
struct user_context {
    word_t registers[n_contextRegisters];
#ifdef CONFIG_HAVE_FPU
    user_fpu_state_t fpuState;
#endif /* CONFIG_HAVE_FPU */
};
typedef struct user_context user_context_t;

unverified_compile_assert(registers_are_first_member_of_user_context,
                          OFFSETOF(user_context_t, registers) == 0)


static inline void Arch_initContext(user_context_t *context)
{
    context->registers[SPSR_EL1] = PSTATE_USER;
}

#endif /* !__ASSEMBLER__ */

