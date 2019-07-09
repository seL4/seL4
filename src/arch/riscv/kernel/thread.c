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
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#include <config.h>
#include <object.h>
#include <machine.h>
#include <arch/model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/thread.h>
#include <linker.h>

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

void Arch_switchToThread(tcb_t *tcb)
{

#ifdef CONFIG_RISCV_HE
    hstatus_set(HSTATUS_SPV);
#endif
    setVMRoot(tcb);
}

BOOT_CODE void Arch_configureIdleThread(tcb_t *tcb)
{
    setRegister(tcb, NextIP, (word_t)idleThreadStart);

    /* Enable interrupts and keep working in supervisor mode */
    setRegister(tcb, SSTATUS, (word_t) SSTATUS_SPP | SSTATUS_SPIE);
#ifdef ENABLE_SMP_SUPPORT
    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (NODE_STATE_ON_CORE(ksIdleThread, i) == tcb) {
            setRegister(tcb, SP, (word_t)kernel_stack_alloc + (i + 1) * BIT(CONFIG_KERNEL_STACK_BITS));
            break;
        }
    }
#else
    setRegister(tcb, SP, (word_t)kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS));
#endif
}

void Arch_switchToIdleThread(void)
{
#ifdef CONFIG_RISCV_HE
    /* Idle thread runs in HS-mode */
    hstatus_clear(HSTATUS_SPV);
    vcpu_switch(NULL);
#else
    tcb_t *tcb = NODE_STATE(ksIdleThread);

    /* Force the idle thread to run on kernel page table */
    setVMRoot(tcb);
#endif
}

void Arch_activateIdleThread(tcb_t *tcb)
{
    /* Don't need to do anything */
}

void Arch_postModifyRegisters(tcb_t *tptr)
{
    /* Nothing to do */
}
