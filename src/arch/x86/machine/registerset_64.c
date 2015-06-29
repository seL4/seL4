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
#include <arch/machine/fpu.h>
#include <arch/object/structures.h>

const register_t msgRegisters[] = {
    RDI, RBP
};

const register_t frameRegisters[] = {
    FaultIP, RSP, RFLAGS, RAX, RBX, RCX, RDX, RSI, RDI, RBP,
    R8, R9, R10, R11, R12, R13, R14, R15
};

const register_t gpRegisters[] = {
    TLS_BASE, FS, GS
};

const register_t exceptionMessage[] = {
    FaultIP, RSP, RFLAGS
};

const register_t syscallMessage[] = {
    RAX, RBX, RCX, RDX, RSI, RDI, RBP, R8, R9, R10, R11, R12,
    R13, R14, R15, NextIP, RSP, RFLAGS
};

void Arch_initContext(user_context_t* context)
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
    context->registers[DS] = SEL_DS_3;
    context->registers[ES] = SEL_DS_3;
    context->registers[FS] = SEL_NULL;
    context->registers[GS] = SEL_NULL;
    context->registers[TLS_BASE] = 0;
    context->registers[Error] = 0;
    context->registers[FaultIP] = 0;
    context->registers[NextIP] = 0;            /* overwritten by setNextPC() later on */
    context->registers[CS] = SEL_CS_3;
    context->registers[RFLAGS] = BIT(9) | BIT(1); /* enable interrupts and set bit 1 which is always 1 */
    context->registers[RSP] = 0;                /* userland has to set it after entry */
    context->registers[SS] = SEL_DS_3;

    Arch_initFpuContext(context);
}

word_t sanitiseRegister(register_t reg, word_t v)
{
    if (reg == RFLAGS) {
        v |=  BIT(1);   /* reserved bit that must be set to 1 */
        v &= ~BIT(3);   /* reserved bit that must be set to 0 */
        v &= ~BIT(5);   /* reserved bit that must be set to 0 */
        v |=  BIT(9);   /* interrupts must be enabled in userland */
        v &=  MASK(12); /* bits 12:31 have to be 0 */
    }
    if (reg == FS || reg == GS) {
        if (v != SEL_TLS && v != SEL_IPCBUF) {
            v = 0;
        }
    }
    return v;
}
