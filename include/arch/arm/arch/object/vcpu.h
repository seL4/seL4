/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <api/failures.h>
#include <linker.h>

#define HCR_RW       BIT(31)     /* Execution state control        */
#define HCR_TRVM     BIT(30)     /* trap reads of VM controls      */
#define HCR_HCD      BIT(29)     /* Disable HVC                    */
#define HCR_TDZ      BIT(28)     /* trap DC ZVA AArch64 only       */
#define HCR_TGE      BIT(27)     /* Trap general exceptions        */
#define HCR_TVM      BIT(26)     /* Trap MMU access                */
#define HCR_TTLB     BIT(25)     /* Trap TLB operations            */
#define HCR_TPU      BIT(24)     /* Trap cache maintenance         */
#define HCR_TPC      BIT(23)     /* Trap cache maintenance PoC     */
#define HCR_TSW      BIT(22)     /* Trap cache maintenance set/way */
#define HCR_TCACHE   (HCR_TPU | HCR_TPC | HCR_TSW)
#define HCR_TAC      BIT(21)     /* Trap ACTLR access              */
#define HCR_TIDCP    BIT(20)     /* Trap lockdown                  */
#define HCR_TSC      BIT(19)     /* Trap SMC instructions          */
#define HCR_TID3     BIT(18)     /* Trap ID register 3             */
#define HCR_TID2     BIT(17)     /* Trap ID register 2             */
#define HCR_TID1     BIT(16)     /* Trap ID register 1             */
#define HCR_TID0     BIT(15)     /* Trap ID register 0             */
#define HCR_TID      (HCR_TID0 | HCR_TID1 | HCR_TID2 | HCR_TID3)
#define HCR_TWE      BIT(14)     /* Trap WFE                       */
#define HCR_TWI      BIT(13)     /* Trap WFI                       */
#define HCR_DC       BIT(12)     /* Default cacheable              */
#define HCR_BSU(x)   ((x) << 10) /* Barrier sharability upgrade    */
#define HCR_FB       BIT( 9)     /* Force broadcast                */
#define HCR_VA       BIT( 8)     /* Virtual async abort            */
#define HCR_VI       BIT( 7)     /* Virtual IRQ                    */
#define HCR_VF       BIT( 6)     /* Virtual FIRQ                   */
#define HCR_AMO      BIT( 5)     /* CPSR.A override enable         */
#define HCR_IMO      BIT( 4)     /* CPSR.I override enable         */
#define HCR_FMO      BIT( 3)     /* CPSR.F override enable         */
#define HCR_PTW      BIT( 2)     /* Protected table walk           */
#define HCR_SWIO     BIT( 1)     /* set/way invalidate override    */
#define HCR_VM       BIT( 0)     /* Virtualization MMU enable      */


struct gicVCpuIface {
    uint32_t hcr;
    uint32_t vmcr;
    uint32_t apr;
    virq_t lr[GIC_VCPU_MAX_NUM_LR];
};

#ifdef CONFIG_VTIMER_UPDATE_VOFFSET
struct vTimer {
    uint64_t last_pcount;
};
#endif

enum VPPIEventIRQ {
    VPPIEventIRQ_VTimer,
    n_VPPIEventIRQ,
    VPPIEventIRQ_invalid = n_VPPIEventIRQ,
};
typedef word_t VPPIEventIRQ_t;

struct vcpu {
    /* TCB associated with this VCPU. */
    struct tcb *vcpuTCB;
    struct gicVCpuIface vgic;
    word_t regs[seL4_VCPUReg_Num];
    bool_t vppi_masked[n_VPPIEventIRQ];
#ifdef CONFIG_VTIMER_UPDATE_VOFFSET
    /* vTimer is 8-bytes wide and has same alignment requirement.
     * To keep the struct packed on 32-bit platforms when accompanied by an
     * odd number of 32-bit words, we need to add a padding word.
     * */
    word_t vcpu_padding;
    struct vTimer virtTimer;
#endif
};
typedef struct vcpu vcpu_t;
compile_assert(vcpu_size_correct, sizeof(struct vcpu) <= BIT(VCPU_SIZE_BITS))

void VGICMaintenance(void);
void handleVCPUFault(word_t hsr);
void VPPIEvent(irq_t irq);

void vcpu_init(vcpu_t *vcpu);

/* Performs one off initialization of VCPU state and structures. Should be
 * called in boot code before any other VCPU functions */
BOOT_CODE void vcpu_boot_init(void);

void vcpu_finalise(vcpu_t *vcpu);

void associateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb);

void dissociateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb);

exception_t decodeARMVCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t *slot,
    cap_t cap,
    bool_t call,
    word_t *buffer
);

void vcpu_restore(vcpu_t *cpu);
void vcpu_switch(vcpu_t *cpu);
#ifdef ENABLE_SMP_SUPPORT
void handleVCPUInjectInterruptIPI(vcpu_t *vcpu, unsigned long index, virq_t virq);
#endif /* ENABLE_SMP_SUPPORT */

exception_t decodeVCPUWriteReg(cap_t cap, unsigned int length, word_t *buffer);
exception_t decodeVCPUReadReg(cap_t cap, unsigned int length, bool_t call, word_t *buffer);
exception_t decodeVCPUInjectIRQ(cap_t cap, unsigned int length, word_t *buffer);
exception_t decodeVCPUSetTCB(cap_t cap);
exception_t decodeVCPUAckVPPI(cap_t cap, unsigned int length, word_t *buffer);

exception_t invokeVCPUWriteReg(vcpu_t *vcpu, word_t field, word_t value);
exception_t invokeVCPUReadReg(vcpu_t *vcpu, word_t field, bool_t call);
exception_t invokeVCPUInjectIRQ(vcpu_t *vcpu, unsigned long index, virq_t virq);
exception_t invokeVCPUSetTCB(vcpu_t *vcpu, tcb_t *tcb);
exception_t invokeVCPUAckVPPI(vcpu_t *vcpu, VPPIEventIRQ_t vppi);
static word_t vcpu_hw_read_reg(word_t reg_index);
static void vcpu_hw_write_reg(word_t reg_index, word_t reg);

static inline void vcpu_save_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu->regs[reg] = vcpu_hw_read_reg(reg);
}

static inline void vcpu_save_reg_range(vcpu_t *vcpu, word_t start, word_t end)
{
    for (word_t i = start; i <= end; i++) {
        vcpu_save_reg(vcpu, i);
    }
}

static inline void vcpu_restore_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu_hw_write_reg(reg, vcpu->regs[reg]);
}

static inline void vcpu_restore_reg_range(vcpu_t *vcpu, word_t start, word_t end)
{
    for (word_t i = start; i <= end; i++) {
        vcpu_restore_reg(vcpu, i);
    }
}

static inline word_t vcpu_read_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return 0;
    }
    return vcpu->regs[reg];
}

static inline void vcpu_write_reg(vcpu_t *vcpu, word_t reg, word_t value)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu->regs[reg] = value;
}

static inline VPPIEventIRQ_t irqVPPIEventIndex(irq_t irq)
{
    switch (IRQT_TO_IRQ(irq)) {
    case INTERRUPT_VTIMER_EVENT:
        return VPPIEventIRQ_VTimer;

    default:
        return VPPIEventIRQ_invalid;
    }
}

#else /* end of CONFIG_ARM_HYPERVISOR_SUPPORT */

/* used in boot.c with a guard, use a marco to avoid exposing vcpu_t */
#define vcpu_boot_init() do {} while(0)
#define vcpu_switch(x) do {} while(0)
static inline void VGICMaintenance(void) {}

#endif /* end of !CONFIG_ARM_HYPERVISOR_SUPPORT */

