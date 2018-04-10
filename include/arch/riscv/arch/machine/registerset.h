/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __ARCH_MACHINE_REGISTERSET_H
#define __ARCH_MACHINE_REGISTERSET_H

#include "hardware.h"
#include <arch/encoding.h>

#ifndef __ASSEMBLER__

#include <stdint.h>
#include <util.h>
#include <arch/types.h>

enum _register {

    ra = 0, LR = 0,

    sp = 1, SP = 1,
    gp = 2,
    tp = 3,

    t0 = 4,
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
    a7 = 16,

    s2 = 17,
    s3 = 18,
    s4 = 10,
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
    SCAUSE,
    SSTATUS,
    SEPC,
    NEXTPC,

    /* TODO: add other user-level CSRs if needed (i.e. to avoid channels) */

    n_contextRegisters
};

typedef uint64_t register_t;

enum messageSizes {
    n_msgRegisters = 4,
    n_frameRegisters = 17,
    n_gpRegisters = 0,
    n_exceptionMessage = 3,
    n_syscallMessage = 10
};

extern const register_t msgRegisters[] VISIBLE;
extern const register_t frameRegisters[] VISIBLE;
extern const register_t gpRegisters[] VISIBLE;
extern const register_t exceptionMessage[] VISIBLE;
extern const register_t syscallMessage[] VISIBLE;

struct user_context {
    word_t registers[n_contextRegisters];
};
typedef struct user_context user_context_t;

static inline void Arch_initContext(user_context_t* context)
{
    /* Enable supervisor interrupts (when going to user-mode) */
    context->registers[SSTATUS] = SSTATUS_SPIE;
}

static inline word_t CONST
sanitiseRegister(register_t reg, word_t v, bool_t archInfo)
{
    return v;
}


#define EXCEPTION_MESSAGE \
 {\
    [seL4_UserException_FaultIP] = SEPC,\
    [seL4_UserException_SP] = SP,\
    [seL4_UserException_Number] = a7,\
 }

#define SYSCALL_MESSAGE \
{\
    [seL4_UnknownSyscall_FaultIP] = SEPC,\
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

#endif /* __ASSEMBLER__ */

#endif /* !__ARCH_MACHINE_REGISTERSET_H */
