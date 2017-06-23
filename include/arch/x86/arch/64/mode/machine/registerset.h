/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODE_MACHINE_REGISTERSET_H_
#define __ARCH_MODE_MACHINE_REGISTERSET_H_

/* These are the indices of the registers in the
 * saved thread context. The values are determined
 * by the order in which they're saved in the trap
 * handler.
 *
 * BEWARE:
 * You will have to adapt traps.S extensively if
 * you change anything in this enum!
 */

/* This register layout is optimized for usage with
 * the syscall and sysret instructions. Interrupts
 * and sysenter have to do some juggling to make
 * things work */
enum _register {
    // User registers that will be preserved during syscall
    // Deliberately place the cap and badge registers early
    // So that when popping on the fastpath we can just not
    // pop these
    RDI                     = 0,    /* 0x00 */
    capRegister             = 0,
    badgeRegister           = 0,
    RSI                     = 1,    /* 0x08 */
    msgInfoRegister         = 1,
    RAX                     = 2,    /* 0x10 */
    RBX                     = 3,    /* 0x18 */
    RBP                     = 4,    /* 0x20 */
    R12                     = 5,    /* 0x28 */
    R13                     = 6,    /* 0x30 */
    R14                     = 7,    /* 0x38 */
    RDX                     = 8,    /* 0x40 */
    // Group the message registers so they can be efficiently copied
    R10                     = 9,    /* 0x48 */
    R8                      = 10,   /* 0x50 */
    R9                      = 11,   /* 0x58 */
    R15                     = 12,   /* 0x60 */
    FLAGS                   = 13,   /* 0x68 */
    // Put the NextIP, which is a virtual register, here as we
    // need to set this in the syscall path
    NextIP                  = 14,   /* 0x70 */
    // Same for the error code
    Error                   = 15,   /* 0x78 */
    RSP                     = 16,   /* 0x80 */
    // For locality put these here as well
    TLS_BASE                = 17,   /* 0x88 */
    FaultIP                 = 18,   /* 0x90 */
    // Now user Registers that get clobbered by syscall
    R11                     = 19,   /* 0x98 */
    RCX                     = 20,   /* 0xa0 */
    CS                      = 21,   /* 0xa8 */
    SS                      = 22,   /* 0xb0 */

    n_contextRegisters      = 23    /* 0xb8 */
};

typedef uint32_t register_t;

enum messageSizes {
    n_msgRegisters = seL4_FastMessageRegisters,
    n_frameRegisters = 18,
    n_gpRegisters = 1,
    n_exceptionMessage = 3,
    n_syscallMessage = 18
};

#define SYSCALL_MESSAGE \
{    \
    [seL4_UnknownSyscall_RAX] = RAX,\
    [seL4_UnknownSyscall_RBX] = RBX,\
    [seL4_UnknownSyscall_RCX] = RCX,\
    [seL4_UnknownSyscall_RDX] = RDX,\
    [seL4_UnknownSyscall_RSI] = RSI,\
    [seL4_UnknownSyscall_RDI] = RDI,\
    [seL4_UnknownSyscall_RBP] = RBP,\
    [seL4_UnknownSyscall_R8]  = R8,\
    [seL4_UnknownSyscall_R9]  = R9,\
    [seL4_UnknownSyscall_R10] = R10,\
    [seL4_UnknownSyscall_R11] = R11,\
    [seL4_UnknownSyscall_R12] = R12,\
    [seL4_UnknownSyscall_R13] = R13,\
    [seL4_UnknownSyscall_R14] = R14,\
    [seL4_UnknownSyscall_R15] = R15,\
    [seL4_UnknownSyscall_FaultIP] = FaultIP,\
    [seL4_UnknownSyscall_SP] = RSP,\
    [seL4_UnknownSyscall_FLAGS] = FLAGS\
}

#define EXCEPTION_MESSAGE \
{ \
    [seL4_UserException_FaultIP] = FaultIP,\
    [seL4_UserException_SP] = RSP,\
    [seL4_UserException_FLAGS] = FLAGS\
}

extern const register_t msgRegisters[];
extern const register_t frameRegisters[];
extern const register_t gpRegisters[];

#define FPU_PADDING word_t padding[1];

#endif /* __ARCH_MODE_MACHINE_REGISTERSET_H_ */
