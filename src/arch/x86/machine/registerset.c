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

#include <config.h>
#include <arch/machine/registerset.h>

void Arch_initContext(user_context_t* context)
{
    Mode_initContext(context);
    context->registers[TLS_BASE] = 0;
    context->registers[Error] = 0;
    context->registers[FaultIP] = 0;
    context->registers[NextIP] = 0;            /* overwritten by setNextPC() later on */
    context->registers[CS] = SEL_CS_3;
    context->registers[FLAGS] = FLAGS_USER_DEFAULT;
    context->registers[SS] = SEL_DS_3;

    Arch_initFpuContext(context);
#ifdef CONFIG_HARDWARE_DEBUG_API
    Arch_initBreakpointContext(&context->breakpointState);
#endif
}

word_t sanitiseRegister(register_t reg, word_t v, bool_t archInfo)
{
    /* First perform any mode specific sanitization */
    v = Mode_sanitiseRegister(reg, v);
    if (reg == FLAGS) {
        /* Set architecturally defined high and low bits */
        v |=  FLAGS_HIGH;
        v &= ~FLAGS_LOW;
        /* require user to have interrupts and no traps */
        v |=  FLAGS_IF;
        v &= ~FLAGS_TF;
        /* remove any other bits that shouldn't be set */
        v &=  FLAGS_MASK;
    }
    return v;
}
