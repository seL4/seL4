/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_REGISTERSET_32_H
#define __ARCH_MACHINE_REGISTERSET_32_H

#include <config.h>

/*
 * We cannot allow async aborts in the verified kernel, but
 * they are useful in identifying invalid memory access bugs
 * so we enable them in debug mode.
 */
#ifdef CONFIG_DEBUG_BUILD
#define CPSR_EXTRA_FLAGS 0
#else
#define CPSR_EXTRA_FLAGS PMASK_ASYNC_ABORT
#endif

#define CPSR_USER            ( PMASK_FIRQ         \
                             | PMODE_USER         \
                             | CPSR_EXTRA_FLAGS   )

#define CPSR_KERNEL          ( PMASK_FIRQ         \
                             | PMASK_IRQ          \
                             | PMODE_KERNEL       \
                             | CPSR_EXTRA_FLAGS   )

#define CPSR_IDLETHREAD      ( PMASK_FIRQ         \
                             | PMODE_IDLE         \
                             | CPSR_EXTRA_FLAGS   )

/* Offsets within the user context, these need to match the order in
 * register_t below */
#define PT_SP               (13  * 4)
#define PT_LR_svc           (15 * 4)
#define PT_ELR_hyp          (15 * 4)
#define PT_TPIDRURW         (18 * 4)
#define PT_FaultInstruction (17 * 4)
#define PT_R8               (8  * 4)

#ifndef __ASSEMBLER__ /* C only definitions */

#include <config.h>
#include <stdint.h>
#include <assert.h>
#include <util.h>
#include <arch/types.h>
#include <arch/machine/debug_conf.h>
#include <plat/api/constants.h>

/* These are the indices of the registers in the
 * saved thread context.  The values are determined
 * by the order in which they're saved in the trap
 * handler. */
enum _register {
    R0 = 0,
    capRegister = 0,
    badgeRegister = 0,

    R1 = 1,
    msgInfoRegister = 1,

    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,

    R13 = 13,
    SP = 13,

    R14 = 14,
    LR = 14,

    /* End of GP registers, the following are additional kernel-saved state. */

    LR_svc = 15,
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    ELR_hyp = 15,
#endif
    CPSR = 16,

    FaultInstruction = 17,
#ifndef CONFIG_ARCH_ARM_V6
    /* user readable/writable thread ID register.
     * name comes from the ARM manual */
    TPIDRURW = 18,
    n_contextRegisters = 19,
#else
    n_contextRegisters = 18,
#endif
};

compile_assert(sp_offset_correct, SP * sizeof(word_t) == PT_SP)
compile_assert(lr_svc_offset_correct, LR_svc * sizeof(word_t) == PT_LR_svc)
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
compile_assert(elr_hyp_offset_correct, ELR_hyp * sizeof(word_t) == PT_ELR_hyp)
#endif
compile_assert(faultinstruction_offset_correct, FaultInstruction * sizeof(word_t) == PT_FaultInstruction)
compile_assert(r8_offset_correct, R8 * sizeof(word_t) == PT_R8)

typedef word_t register_t;

enum messageSizes {
    n_msgRegisters = seL4_FastMessageRegisters,
    n_frameRegisters = 10,
    n_gpRegisters = 7,
    n_exceptionMessage = 3,
    n_syscallMessage = 12,
};

#define EXCEPTION_MESSAGE \
 {\
    [seL4_UserException_FaultIP] = FaultInstruction,\
    [seL4_UserException_SP] = SP,\
    [seL4_UserException_CPSR] = CPSR\
 }

#define SYSCALL_MESSAGE \
{\
    [seL4_UnknownSyscall_R0] = R0,\
    [seL4_UnknownSyscall_R1] = R1,\
    [seL4_UnknownSyscall_R2] = R2,\
    [seL4_UnknownSyscall_R3] = R3,\
    [seL4_UnknownSyscall_R4] = R4,\
    [seL4_UnknownSyscall_R5] = R5,\
    [seL4_UnknownSyscall_R6] = R6,\
    [seL4_UnknownSyscall_R7] = R7,\
    [seL4_UnknownSyscall_FaultIP] = FaultInstruction,\
    [seL4_UnknownSyscall_SP] = SP,\
    [seL4_UnknownSyscall_LR] = LR,\
    [seL4_UnknownSyscall_CPSR] = CPSR\
}

extern const register_t msgRegisters[];
extern const register_t frameRegisters[];
extern const register_t gpRegisters[];

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
typedef struct debug_register_pair {
    word_t cr, vr;
} debug_register_pair_t;

typedef struct user_breakpoint_state {
    /* We don't use context comparisons. */
    debug_register_pair_t breakpoint[seL4_NumExclusiveBreakpoints],
                          watchpoint[seL4_NumExclusiveWatchpoints];
    uint32_t used_breakpoints_bf;
    word_t n_instructions;
    bool_t single_step_enabled;
    uint16_t single_step_hw_bp_num;
} user_breakpoint_state_t;
#endif

#ifdef CONFIG_HAVE_FPU
typedef struct user_fpu_state {
    uint64_t fpregs[32];
    uint32_t fpexc;
    uint32_t fpscr;
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
#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
    user_breakpoint_state_t breakpointState;
#endif /* CONFIG_HARDWARE_DEBUG_API */
#ifdef CONFIG_HAVE_FPU
    user_fpu_state_t fpuState;
#endif /* CONFIG_HAVE_FPU */
};
typedef struct user_context user_context_t;

unverified_compile_assert(registers_are_first_member_of_user_context,
                          OFFSETOF(user_context_t, registers) == 0)

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
void Arch_initBreakpointContext(user_context_t *context);
#endif

static inline void Arch_initContext(user_context_t* context)
{
    context->registers[CPSR] = CPSR_USER;
#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
    Arch_initBreakpointContext(context);
#endif
}

#endif /* !__ASSEMBLER__ */

#endif /* !__ARCH_MACHINE_REGISTERSET_32_H */
