/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_REGISTERSET_64_H
#define __ARCH_MACHINE_REGISTERSET_64_H

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
    /* extended registers */
    RAX                     = 0,        /* 0x00 */
    RBX                     = 1,        /* 0x08 */
    capRegister             = 1,
    badgeRegister           = 1,
    RCX                     = 2,        /* 0x10 */
    RDX                     = 3,        /* 0x18 */
    RSI                     = 4,        /* 0x20 */
    msgInfoRegister         = 4,
    RDI                     = 5,        /* 0x28 */
    RBP                     = 6,        /* 0x30 */

    /* x86-64 new general purpose registers */
    R8                      = 7,        /* 0x38 */
    R9                      = 8,        /* 0x40 */
    R10                     = 9,        /* 0x48 */
    R11                     = 10,       /* 0x50 */
    R12                     = 11,       /* 0x58 */
    R13                     = 12,       /* 0x60 */
    R14                     = 13,       /* 0x68 */
    R15                     = 14,       /* 0x70 */

    /* segment registers */
    DS                      = 15,       /* 0x78 */
    ES                      = 16,       /* 0x80 */
    FS                      = 17,       /* 0x88 */
    GS                      = 18,       /* 0x90 */

    /* virtual registers (not actually present in hardware) */

    FaultIP                = 19,       /* 0x98 */
    TLS_BASE                = 20,       /* 0xa0 */
    /*
     * The padding exits because the x86-64 need the RIP to be
     * 16-byte aligned for interrupt handling, otherwise, the
     * CPU will adjust the RIP to be 16-byte aligned
     */
    PADDING                 = 21,       /* 0xa8 */
    /* pushed by CPU automatically */
    Error                   = 22,       /* 0xb0 */
    NextIP                 = 23,       /* 0xb8 */
    CS                      = 24,       /* 0xc0 */
    RFLAGS                  = 25,       /* 0xc8 */
    RSP                     = 26,       /* 0xd0 */
    SS                      = 27,       /* 0xd8 */

    n_contextRegisters      = 28        /* 0xe0 */
};

typedef uint32_t register_t;

enum messageSizes {
    n_msgRegisters = 2,
    n_frameRegisters = 18,
    n_gpRegisters = 3,
    n_exceptionMessage = 3,
    n_syscallMessage = 18
};

extern const register_t msgRegisters[];
extern const register_t frameRegisters[];
extern const register_t gpRegisters[];
extern const register_t exceptionMessage[];
extern const register_t syscallMessage[];

#define FPU_PADDING

#endif
