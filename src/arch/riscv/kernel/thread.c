/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 * Copyright (c) 2018, Hesham Almatary <Hesham.Almatary@cl.cam.ac.uk>
 * All rights reserved.
 *
 * This software was was developed in part by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#include <object.h>
#include <machine.h>
#include <arch/model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/thread.h>
#include <linker.h>

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

void
Arch_switchToThread(tcb_t *tcb)
{
    setVMRoot(tcb);
    setRegister(tcb, tp, tcb->tcbIPCBuffer);
}

BOOT_CODE void
Arch_configureIdleThread(tcb_t *tcb)
{
    setRegister(tcb, NEXTPC, (word_t)idleThreadStart);

    /* Enable interrupts and keep working in X privilege mode */
    if (config_set(CONFIG_SEL4_RV_MACHINE)) {
        setRegister(tcb, STATUS, (word_t) MSTATUS_MPP | MSTATUS_MPIE | MSTATUS_MIE);
    } else {
        setRegister(tcb, STATUS, (word_t) SSTATUS_SPP | SSTATUS_SPIE | SSTATUS_SIE);
    }

    setRegister(tcb, SP, (word_t)kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS));
}

void
Arch_switchToIdleThread(void)
{
    tcb_t *tcb = NODE_STATE(ksIdleThread);

    /* Force the idle thread to run on kernel page table */
    setVMRoot(tcb);
}

void CONST
Arch_activateIdleThread(tcb_t *tcb)
{
    /* Don't need to do anything */
}

void
Arch_postModifyRegisters(tcb_t *tptr)
{
    /* Nothing to do */
}
