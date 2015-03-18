/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_VCPU_H
#define __ARCH_OBJECT_VCPU_H

struct vcpu {
    /* Storage for VMCS region. First field of vcpu_t so they share address.
     * Will use at most 4KiB of memory. Statically reserve 4KiB for convenience. */
    char vmcs[4096];
    uint32_t io[2048];

    /* General purpose registers that we have to save and restore as they
     * are not part of the vmcs */
    uint32_t gp_registers[EBP + 1];

    /* TCB associated with this VCPU. */
    struct tcb *tcb;
    bool_t launched;
    uint32_t interrupt_info;
    cap_t io_port;
    uint32_t io_min;
    uint32_t io_max;
    /* These are the values the user last set these to. We may have written
     * other values for the purposes of lazy fpu management */
    uint32_t exception_mask;
    uint32_t cr0_shadow;
    uint32_t cr0_mask;
    uint32_t cr0;
    /* Last values we wrote the VMCS */
    uint32_t written_exception_mask;
    uint32_t written_cr0_shadow;
    uint32_t written_cr0_mask;
    uint32_t written_cr0;

    /* Last used EPT root */
    uint32_t last_ept_root;

    /* Last set host cr3 */
    uint32_t last_host_cr3;
};
typedef struct vcpu vcpu_t;

bool_t init_vtx_fixed_values(bool_t useTrueMsrs);
void vcpu_init(vcpu_t *vcpu);

void vcpu_finalise(vcpu_t *vcpu);

uint16_t vpid_for_vcpu(vcpu_t *vcpu);

void associateVcpuTcb(tcb_t *tcb, vcpu_t *vcpu);

void dissociateVcpuTcb(tcb_t *tcb, vcpu_t *vcpu);

exception_t decodeIA32VCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
);

void vcpu_update_vmenter_state(vcpu_t *vcpu);
exception_t decodeReadVMCS(cap_t cap, unsigned int length, word_t* buffer);
exception_t decodeWriteVMCS(cap_t cap, unsigned int length, word_t* buffer);
exception_t decodeSetTCB(cap_t cap, unsigned int length, word_t* buffer, extra_caps_t extraCaps);
exception_t decodeSetIOPort(cap_t cap, unsigned int length, word_t* buffer, extra_caps_t extraCaps);
exception_t invokeReadVMCS(vcpu_t *vcpu, int num_fields, uint32_t *fields);
exception_t invokeWriteVMCS(vcpu_t *vcpu, int num_fields, uint32_t *fields, uint32_t *values);
exception_t invokeSetTCB(vcpu_t *vcpu, tcb_t *tcb);
exception_t invokeSetIOPort(vcpu_t *vcpu, cap_t cap);
exception_t decodeSetIOPortMask(cap_t cap, unsigned int length, word_t *buffer);
exception_t invokeSetIOPortMask(vcpu_t *vcpu, uint32_t low, uint32_t high, int mask);
exception_t decodeVCPUWriteRegisters(cap_t cap, unsigned int length, word_t *buffer);
exception_t invokeVCPUWriteRegisters(vcpu_t *vcpu, word_t *buffer);

#endif
