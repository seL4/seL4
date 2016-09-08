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
#ifdef DEBUG
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
                             | PMASK_ASYNC_ABORT  \
                             | CPSR_EXTRA_FLAGS   )

/* Offsets within the user context, these need to match the order in
 * register_t below */
#define PT_SP               (13  * 4)
#define PT_LR_svc           (15 * 4)
#define PT_ELR_hyp          (15 * 4)
#define PT_FaultInstruction (17 * 4)
#define PT_R8               (8  * 4)

#ifndef __ASSEMBLER__ /* C only definitions */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <arch/types.h>

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
    n_contextRegisters = 18,
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
    n_msgRegisters = 4,
    n_frameRegisters = 10,
    n_gpRegisters = 7,
    n_exceptionMessage = 3,
    n_syscallMessage = 12,
};

extern const register_t msgRegisters[] VISIBLE;
extern const register_t frameRegisters[] VISIBLE;
extern const register_t gpRegisters[] VISIBLE;
extern const register_t exceptionMessage[] VISIBLE;
extern const register_t syscallMessage[] VISIBLE;

/* ARM user-code context: size = 72 bytes */
struct user_context {
    word_t registers[n_contextRegisters];
};
typedef struct user_context user_context_t;

static inline void Arch_initContext(user_context_t* context)
{
    context->registers[CPSR] = CPSR_USER;
}

static inline word_t CONST
sanitiseRegister(register_t reg, word_t v)
{
    if (reg == CPSR) {
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            switch (v & 0x1f) {
            case PMODE_USER:
            case PMODE_FIQ:
            case PMODE_IRQ:
            case PMODE_SUPERVISOR:
            case PMODE_ABORT:
            case PMODE_UNDEFINED:
            case PMODE_SYSTEM:
                return v;
            case PMODE_HYPERVISOR:
            default:
                /* For backwards compatibility, Invalid modes revert to USER mode */
                break;
            }
        }

        return (v & 0xf8000000) | CPSR_USER;
    } else {
        return v;
    }
}

#endif /* !__ASSEMBLER__ */

#endif /* !__ARCH_MACHINE_REGISTERSET_32_H */
