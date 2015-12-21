/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
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
    R15                     = 8,    /* 0x40 */
    RDX                     = 9,    /* 0x48 */
    // Group the message registers so they can be efficiently copied
    R10                     = 10,   /* 0x50 */
    R8                      = 11,   /* 0x58 */
    R9                      = 12,   /* 0x60 */
    RFLAGS                  = 13,   /* 0x68 */
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

#define FPU_PADDING word_t padding[1];
