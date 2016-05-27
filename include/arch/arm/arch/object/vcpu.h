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

#ifdef ARM_HYP

#include <api/failures.h>
struct cpXRegs {
    uint32_t sctlr;
    uint32_t actlr;
};

struct gicVCpuIface {
    uint32_t hcr;
    uint32_t vmcr;
    uint32_t apr;
    uint32_t lr[64];
};

struct vcpu {
    /* TCB associated with this VCPU. */
    struct tcb *tcb;
    struct cpXRegs cpx;
    struct gicVCpuIface vgic;
};
typedef struct vcpu vcpu_t;
compile_assert(vcpu_size_correct, sizeof(struct vcpu) <= BIT(VCPU_SIZE_BITS))

void VGICMaintenance(void);
void handleVCPUFault(word_t hsr) VISIBLE;

void vcpu_init(vcpu_t *vcpu);

void vcpu_finalise(vcpu_t *vcpu);

void associateVcpuTcb(tcb_t *tcb, vcpu_t *vcpu);

void dissociateVcpuTcb(tcb_t *tcb, vcpu_t *vcpu);

exception_t decodeARMVCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
);

void vcpu_restore(vcpu_t *cpu);
void vcpu_switch(vcpu_t *cpu);

exception_t decodeVCPUWriteReg(cap_t cap, unsigned int length, word_t* buffer);
exception_t decodeVCPUReadReg(cap_t cap, unsigned int length, word_t* buffer);
exception_t decodeVCPUInjectIRQ(cap_t cap, unsigned int length, word_t* buffer);
exception_t decodeVCPUSetTCB(cap_t cap, unsigned int length, word_t* buffer, extra_caps_t extraCaps);

exception_t invokeVCPUWriteReg(vcpu_t *vcpu, uint32_t field, uint32_t value);
exception_t invokeVCPUReadReg(vcpu_t *vcpu, uint32_t field);
exception_t invokeVCPUInjectIRQ(vcpu_t *vcpu, int index, int group, int priority, int irq);
exception_t invokeVCPUSetTCB(vcpu_t *vcpu, tcb_t *tcb);

#else /* end of ARM_HYP */

/* used in boot.c with a guard, use a marco to avoid exposing vcpu_t */
#define vcpu_restore(x)
#define vcpu_switch(x)
static inline void VGICMaintenance(void) {}

#endif /* end of !ARM_HYP */

#endif /* __ARCH_OBJECT_VCPU_H */
