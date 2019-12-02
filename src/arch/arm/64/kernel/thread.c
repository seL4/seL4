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
}

BOOT_CODE void Arch_configureIdleThread(tcb_t *tcb)
{
    setRegister(tcb, SPSR_EL1, PSTATE_IDLETHREAD);
    setRegister(tcb, ELR_EL1, (word_t)idleThreadStart);
}

void Arch_switchToIdleThread(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(NULL);
    }
    setCurrentUserVSpaceRoot(ttbr_new(0, pptr_to_paddr(armKSGlobalUserVSpace)));
}

void Arch_activateIdleThread(tcb_t *tcb)
{
    /* Don't need to do anything */
}
