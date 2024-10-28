/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <arch/machine/registerset.h>

void Arch_initContext(user_context_t *context)
{
    Mode_initContext(context);
    context->registers[FS_BASE] = 0;
    context->registers[GS_BASE] = 0;
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

word_t sanitiseRegister(regoff_t reg, word_t v, bool_t archInfo)
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
