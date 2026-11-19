/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <object.h>
#include <machine.h>
#include <arch/model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/thread.h>
#include <linker.h>

#if defined(CONFIG_HAVE_CHERI)
#include <cheri/cheri.h>
#endif

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

void Arch_switchToThread(tcb_t *tcb)
{
    setVMRoot(tcb);
}

BOOT_CODE void Arch_configureIdleThread(tcb_t *tcb)
{
#if defined(CONFIG_HAVE_CHERI)
    /* Derive an idle thread's PCC from the kernel's PCC */
    void *__capability idle_pcc = __builtin_cheri_address_set(CheriArch_get_pcc(), (word_t)idle_thread);
    idle_pcc = __builtin_cheri_seal_entry(idle_pcc);
    setRegister(tcb, NextIP, (rword_t)idle_pcc);

    /* The idle thread should not be using any data memory (e.g., stack) */
    setRegister(tcb, DDC, 0);
#else
    setRegister(tcb, NextIP, (rword_t)&idle_thread);
#endif

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
    tcb_t *tcb = NODE_STATE(ksIdleThread);

    /* Force the idle thread to run on kernel page table */
    setVMRoot(tcb);
}

void Arch_activateIdleThread(tcb_t *tcb)
{
    /* Don't need to do anything */
}

void Arch_postModifyRegisters(tcb_t *tptr)
{
    /* Nothing to do */
}

void Arch_prepareNextDomain(void)
{
    /* Don't need to do anything */
}

void Arch_prepareSetDomain(tcb_t *tptr, dom_t dom)
{
    /* Don't need to do anything */
}
