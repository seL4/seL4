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

#include <arch/types.h>
#include <util.h>
#include <assert.h>

/* Number of bytes required to store FPU state. */
#define FPU_STATE_SIZE 512

/* Minimum hardware-enforced alignment needed for FPU state. */
#define MIN_FPU_ALIGNMENT 16

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
    /* general purpose registers */

    /* 0x00 */  EAX             = 0,
    /* 0x04 */  EBX             = 1,
    capRegister     = 1,
    badgeRegister   = 1,
    /* 0x08 */  ECX             = 2,
    /* 0x0C */  EDX             = 3,
    /* 0x10 */  ESI             = 4,
    msgInfoRegister = 4,
    /* 0x14 */  EDI             = 5,
    /* 0x18 */  EBP             = 6,

    /* segment registers */

    /* 0x1C */  DS = 7,
    /* 0x20 */  ES = 8,
    /* 0x24 */  FS = 9,
    /* 0x28 */  GS = 10,

    /* virtual registers (not actually present in hardware) */

    /* 0x2C */  FaultEIP = 11,
    /* 0x30 */  TLS_BASE = 12,

    /* values pushed by the CPU directly */

    /* 0x34 */  Error    = 13,
    /* 0x38 */  NextEIP  = 14,
    /* 0x3C */  CS       = 15,
    /* 0x40 */  EFLAGS   = 16,
    /* 0x44 */  ESP      = 17,
    /* 0x48 */  SS       = 18,

    /* 0x4C */  n_contextRegisters = 19
};

typedef uint32_t register_t;

enum messageSizes {
    n_msgRegisters = 2,
    n_frameRegisters = 10,
    n_gpRegisters = 3,
    n_exceptionMessage = 3,
    n_syscallMessage = 10
};

extern const register_t msgRegisters[];
extern const register_t frameRegisters[];
extern const register_t gpRegisters[];
extern const register_t exceptionMessage[];
extern const register_t syscallMessage[];

/* IA32 FPU context. */
struct user_fpu_state {
    uint8_t state[FPU_STATE_SIZE];
};
typedef struct user_fpu_state user_fpu_state_t;

/* IA32 user-code context */
struct user_context {
    /* 76 bytes */
    word_t registers[n_contextRegisters];

    /*
     * Padding to 16-byte boundary, required by the IA32 FPU state saving
     * and restoring commands.
     */
    word_t padding;

    /* 512 bytes. */
    user_fpu_state_t fpuState;
};
typedef struct user_context user_context_t;

void Arch_initContext(user_context_t* context);
word_t sanitiseRegister(register_t reg, word_t v);

/* Ensure FPU state is aligned within user context. */
compile_assert(fpu_state_alignment_valid,
               OFFSETOF(user_context_t, fpuState) % MIN_FPU_ALIGNMENT == 0)

#endif
