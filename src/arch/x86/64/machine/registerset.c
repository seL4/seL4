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

#include <arch/machine/registerset.h>
#include <machine/fpu.h>
#include <arch/object/structures.h>

const register_t msgRegisters[] = {
    R10, R8, R9, R15
};

const register_t frameRegisters[] = {
    FaultIP, RSP, FLAGS, RAX, RBX, RCX, RDX, RSI, RDI, RBP,
    R8, R9, R10, R11, R12, R13, R14, R15
};

const register_t gpRegisters[] = {
    TLS_BASE
};

const register_t syscallMessage[] = {
    [seL4_UnknownSyscall_RAX] = RAX,
    [seL4_UnknownSyscall_RBX] = RBX,
    [seL4_UnknownSyscall_RCX] = RCX,
    [seL4_UnknownSyscall_RDX] = RDX,
    [seL4_UnknownSyscall_RSI] = RSI,
    [seL4_UnknownSyscall_RDI] = RDI,
    [seL4_UnknownSyscall_RBP] = RBP,
    [seL4_UnknownSyscall_R8]  = R8,
    [seL4_UnknownSyscall_R9]  = R9,
    [seL4_UnknownSyscall_R10] = R10,
    [seL4_UnknownSyscall_R11] = R11,
    [seL4_UnknownSyscall_R12] = R12,
    [seL4_UnknownSyscall_R13] = R13,
    [seL4_UnknownSyscall_R14] = R14,
    [seL4_UnknownSyscall_R15] = R15,
    [seL4_UnknownSyscall_FaultIP] = FaultIP,
    [seL4_UnknownSyscall_SP] = RSP,
    [seL4_UnknownSyscall_FLAGS] = FLAGS
};

void Mode_initContext(user_context_t* context)
{
    context->registers[RAX] = 0;
    context->registers[RBX] = 0;
    context->registers[RCX] = 0;
    context->registers[RDX] = 0;
    context->registers[RSI] = 0;
    context->registers[RDI] = 0;
    context->registers[RBP] = 0;
    context->registers[R8]  = 0;
    context->registers[R9]  = 0;
    context->registers[R10] = 0;
    context->registers[R11] = 0;
    context->registers[R12] = 0;
    context->registers[R13] = 0;
    context->registers[R14] = 0;
    context->registers[R15] = 0;
    context->registers[RSP] = 0;
}

word_t Mode_sanitiseRegister(register_t reg, word_t v)
{
    if (reg == FaultIP || reg == NextIP) {
        /* ensure instruction address is canonical */
        if (v > 0x00007fffffffffff && v < 0xffff800000000000) {
            /* no way to guess what the user wanted so give them zero */
            v = 0;
        }
    }
    return v;
}
