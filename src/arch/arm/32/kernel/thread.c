/*
 * Copyright 2014, General Dynamics C4 Systems
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

void Arch_switchToThread(tcb_t *tcb)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(tcb->tcbArch.tcbVCPU);
    }

    setVMRoot(tcb);
    clearExMonitor();
}

BOOT_CODE void Arch_configureIdleThread(tcb_t *tcb)
{
    setRegister(tcb, CPSR, CPSR_IDLETHREAD);
    setRegister(tcb, NextIP, (word_t)&idle_thread);
}

void Arch_switchToIdleThread(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(NULL);
    }

    /* Force the idle thread to run on kernel page table */
    setVMRoot(NODE_STATE(ksIdleThread));
}

void Arch_activateIdleThread(tcb_t *tcb)
{
    /* Don't need to do anything */
}
