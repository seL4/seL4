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

#include <object.h>
#include <machine.h>
#include <arch/model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/thread.h>
#include <arch/linker.h>

void
Arch_switchToThread(tcb_t* tcb)
{
    /* set PD */
    setVMRoot(tcb);
#if CONFIG_MAX_NUM_NODES > 1
    asm volatile("movq %[value], %%gs:%c[offset]"
                 :
                 : [value] "r" (&tcb->tcbArch.tcbContext.registers[Error + 1]),
                 [offset] "i" (OFFSETOF(nodeInfo_t, currentThreadUserContext)));
#endif
}

BOOT_CODE void
Arch_configureIdleThread(tcb_t* tcb)
{
    setRegister(tcb, FLAGS, FLAGS_USER_DEFAULT);
    setRegister(tcb, NextIP, (uint64_t)idleThreadStart);
    setRegister(tcb, CS, SEL_CS_0);
    setRegister(tcb, SS, SEL_DS_0);
}

void
Arch_switchToIdleThread(void)
{
    tcb_t *tcb = NODE_STATE(ksIdleThread);
    /* Force the idle thread to run on kernel page table */
    setVMRoot(tcb);
    /* In 64-bit mode the CPU unconditionally pushes to the stack when
     * taking an exception. Therefore we need to provide the idle thread
     * with a stack */
    setRegister(tcb, RSP, (uint64_t)&MODE_NODE_STATE(x64KSIRQStack)[IRQ_STACK_SIZE]);
#if CONFIG_MAX_NUM_NODES > 1
    asm volatile("movq %[value], %%gs:%c[offset]"
                 :
                 : [value] "r"(&tcb->tcbArch.tcbContext.registers[Error + 1]),
                 [offset] "i" (OFFSETOF(nodeInfo_t, currentThreadUserContext)));
#endif
}

void
Arch_activateIdleThread(tcb_t* tcb)
{
    /* Don't need to do anything */
}
