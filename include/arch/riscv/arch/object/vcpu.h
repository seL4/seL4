
#ifndef __ARCH_OBJECT_VCPU_H
#define __ARCH_OBJECT_VCPU_H

#include <config.h>

#ifdef CONFIG_RISCV_HE

#include <api/failures.h>
#include <linker.h>

struct vcpu {
    struct tcb *vcpuTCB;
    word_t regs[seL4_VCPUReg_Num];
    bool_t init;
};

typedef struct vcpu vcpu_t;
compile_assert(vcpu_size_correct, sizeof(struct vcpu) <= BIT(VCPU_SIZE_BITS));

BOOT_CODE void vcpu_boot_init(void);
void vcpu_init(vcpu_t *vcpu);
void vcpu_finalise(vcpu_t *vcpu);
void associateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb);
void dissociateVCPUTCB(vcpu_t *vpcu, tcb_t *tcb);

exception_t decodeRISCVVCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t *slot,
    cap_t cap,
    extra_caps_t extraCaps,
    bool_t call,
    word_t *buffer
);

void vcpu_restore(vcpu_t *cpu);
void vcpu_switch(vcpu_t *cpu);

exception_t decodeVCPUWriteReg(cap_t cap, unsigned int length, word_t *buffer);
exception_t decodeVCPUReadReg(cap_t cap, unsigned int length, bool_t call, word_t *buffer);
exception_t decodeVCPUSetTCB(cap_t cap, extra_caps_t extraCaps);

exception_t invokeVCPUWriteReg(vcpu_t *vcpu, word_t field, word_t value);
exception_t invokeVCPUReadReg(vcpu_t *vcpu, word_t field, bool_t call);
exception_t invokeVCPUSetTCB(vcpu_t *vcpu, tcb_t *tcb);

void handleVCPUFault(word_t cause);
#endif


#endif
