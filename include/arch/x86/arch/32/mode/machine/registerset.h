/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#pragma once

/* These are the indices of the registers in the
 * saved thread context. The values are determined
 * by the order in which they're saved in the trap
 * handler.
 *
 * BEWARE:
 * You will have to adapt traps.S extensively if
 * you change anything in this enum!
 */
enum _register {
    /*
     * To ensure that the stack use here correctly begins on a
     * double-word boundary, Error + 1 must be an even number of words
     * from the start.
     */

    /* general purpose registers */

    /* 0x04 */  EAX             = 1,
    /* 0x08 */  EBX             = 2,
    capRegister     = EBX,
    badgeRegister   = EBX,
    /* 0x0C */  ECX             = 3,
    /* 0x10 */  EDX             = 4,
    /* 0x14 */  ESI             = 5,
    msgInfoRegister = ESI,
    /* 0x18 */  EDI             = 6,
    /* 0x1C */  EBP             = 7,
#ifdef CONFIG_KERNEL_MCS
    replyRegister               = 7,
    n_generalRegisters          = 7,
#endif

    /* virtual registers (not actually present in hardware) */

    /* 0x20 */  FaultIP  = 8,

    /* values pushed by the CPU directly */

    /* 0x24 */  Error    = 9,
    /* 0x28 */  NextIP   = 10,
    /* 0x2C */  CS       = 11,
    /* 0x30 */  FLAGS    = 12,
    /* 0x34 */  ESP      = 13,
    /* 0x38 */  SS       = 14,

    /* 0x3C */  n_immContextRegisters = 15,

    /* 0x3C */  FS_BASE = 15,
    /* 0x40 */  GS_BASE = 16,
    TLS_BASE = GS_BASE,

    /* 0x44 */  n_contextRegisters = 17
};

typedef word_t register_t;

enum messageSizes {
    n_msgRegisters = seL4_FastMessageRegisters,
    n_frameRegisters = 10,
    n_gpRegisters = 2,
    n_exceptionMessage = 3,
    n_syscallMessage = 10,
#ifdef CONFIG_KERNEL_MCS
    n_timeoutMessage = 13,
#endif
};

#define SYSCALL_MESSAGE \
{    \
    [seL4_UnknownSyscall_EAX] = EAX,\
    [seL4_UnknownSyscall_EBX] = EBX,\
    [seL4_UnknownSyscall_ECX] = ECX,\
    [seL4_UnknownSyscall_EDX] = EDX,\
    [seL4_UnknownSyscall_ESI] = ESI,\
    [seL4_UnknownSyscall_EDI] = EDI,\
    [seL4_UnknownSyscall_EBP] = EBP,\
    [seL4_UnknownSyscall_FaultIP] = FaultIP,\
    [seL4_UnknownSyscall_SP] = ESP,\
    [seL4_UnknownSyscall_FLAGS] = FLAGS\
}

#define EXCEPTION_MESSAGE \
{    \
    [seL4_UserException_FaultIP] = FaultIP,\
    [seL4_UserException_SP] = ESP,\
    [seL4_UserException_FLAGS] = FLAGS\
}

#define TIMEOUT_REPLY_MESSAGE \
{    \
    [seL4_TimeoutReply_FaultIP] = FaultIP,\
    [seL4_TimeoutReply_SP] = ESP,\
    [seL4_TimeoutReply_FLAGS] = FLAGS,\
    [seL4_TimeoutReply_EAX] = EAX,\
    [seL4_TimeoutReply_EBX] = EBX,\
    [seL4_TimeoutReply_ECX] = ECX,\
    [seL4_TimeoutReply_EDX] = EDX,\
    [seL4_TimeoutReply_ESI] = ESI,\
    [seL4_TimeoutReply_EDI] = EDI,\
    [seL4_TimeoutReply_EBP] = EBP,\
    [seL4_TimeoutReply_FS_BASE] = FS_BASE,\
    [seL4_TimeoutReply_GS_BASE] = GS_BASE,\
}

extern const register_t msgRegisters[];
extern const register_t frameRegisters[];
extern const register_t gpRegisters[];
