/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/machine/registerset.h>

const register_t msgRegisters[] = {
    R2, R3, R4, R5
};

const register_t frameRegisters[] = {
    FaultInstruction, SP, CPSR,
    R0, R1, R8, R9, R10, R11, R12
};

const register_t gpRegisters[] = {
    R2, R3, R4, R5, R6, R7, R14
};

const register_t exceptionMessage[] = {
    [seL4_UserException_FaultIP] = FaultInstruction,
    [seL4_UserException_SP] = SP,
    [seL4_UserException_CPSR] = CPSR
};

const register_t syscallMessage[] = {
    [seL4_UnknownSyscall_R0] = R0,
    [seL4_UnknownSyscall_R1] = R1,
    [seL4_UnknownSyscall_R2] = R2,
    [seL4_UnknownSyscall_R3] = R3,
    [seL4_UnknownSyscall_R4] = R4,
    [seL4_UnknownSyscall_R5] = R5,
    [seL4_UnknownSyscall_R6] = R6,
    [seL4_UnknownSyscall_R7] = R7,
    [seL4_UnknownSyscall_FaultIP] = FaultInstruction,
    [seL4_UnknownSyscall_SP] = SP,
    [seL4_UnknownSyscall_LR] = LR,
    [seL4_UnknownSyscall_CPSR] = CPSR
};
