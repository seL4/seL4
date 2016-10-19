/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <arch/object/vcpu.h>
#include <plat/machine/devices.h>

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

/* Trap WFI/WFE/SMC and override CPSR.AIF */
#define HCR_COMMON ( HCR_TSC | HCR_TWE | HCR_TWI | HCR_AMO | HCR_IMO \
                   | HCR_FMO | HCR_DC  | HCR_VM)
/* Allow native tasks to run at PL1, but restrict access */
#define HCR_NATIVE ( HCR_COMMON | HCR_TGE | HCR_TVM | HCR_TTLB | HCR_TCACHE \
                   | HCR_TAC | HCR_SWIO)
#define HCR_VCPU   (HCR_COMMON)

/* Amongst other things we set the caches to enabled by default. This
 * may cause problems when booting guests that expect caches to be
 * disabled */
#define SCTLR_DEFAULT 0xc5187c
#define ACTLR_DEFAULT 0x40

#define VGIC_HCR_EOI_INVALID_COUNT(hcr) (((hcr) >> 27) & 0x1f)
#define VGIC_HCR_VGRP1DIE               (1U << 7)
#define VGIC_HCR_VGRP1EIE               (1U << 6)
#define VGIC_HCR_VGRP0DIE               (1U << 5)
#define VGIC_HCR_VGRP0EIE               (1U << 4)
#define VGIC_HCR_NPIE                   (1U << 3)
#define VGIC_HCR_LRENPIE                (1U << 2)
#define VGIC_HCR_UIE                    (1U << 1)
#define VGIC_HCR_EN                     (1U << 0)
#define VGIC_MISR_VGRP1D                VGIC_HCR_VGRP1DIE
#define VGIC_MISR_VGRP1E                VGIC_HCR_VGRP1EIE
#define VGIC_MISR_VGRP0D                VGIC_HCR_VGRP0DIE
#define VGIC_MISR_VGRP0E                VGIC_HCR_VGRP0EIE
#define VGIC_MISR_NP                    VGIC_HCR_NPIE
#define VGIC_MISR_LRENP                 VGIC_HCR_LRENPIE
#define VGIC_MISR_U                     VGIC_HCR_UIE
#define VGIC_MISR_EOI                   VGIC_HCR_EN
#define VGIC_VTR_NLISTREGS(vtr)         ((((vtr) >>  0) & 0x3f) + 1)
#define VGIC_VTR_NPRIOBITS(vtr)         ((((vtr) >> 29) & 0x07) + 1)
#define VGIC_VTR_NPREBITS(vtr)          ((((vtr) >> 26) & 0x07) + 1)

struct gich_vcpu_ctrl_map {
    uint32_t hcr;    /* 0x000 RW 0x00000000 Hypervisor Control Register */
    uint32_t vtr;    /* 0x004 RO IMPLEMENTATION DEFINED VGIC Type Register */
    /* Save restore on VCPU switch */
    uint32_t vmcr;   /* 0x008 RW IMPLEMENTATION DEFINED Virtual Machine Control Register */
    uint32_t res1[1];
    /* IRQ pending flags */
    uint32_t misr;   /* 0x010 RO 0x00000000 Maintenance Interrupt Status Register */
    uint32_t res2[3];
    /* Bitfield of list registers that have EOI */
    uint32_t eisr0;  /* 0x020 RO 0x00000000 End of Interrupt Status Registers 0 and 1, see EISRn */
    uint32_t eisr1;  /* 0x024 RO 0x00000000 */
    uint32_t res3[2];
    /* Bitfield of list registers that are empty */
    uint32_t elsr0;  /* 0x030 RO IMPLEMENTATION DEFINED a */
    uint32_t elsr1;  /* 0x034 RO IMPLEMENTATION DEFINED a Empty List Register Status Registers 0 and 1, see ELRSRn */
    uint32_t res4[46];
    /* Active priority: bitfield of active priorities */
    uint32_t apr;    /* 0x0F0 RW 0x00000000 Active Priorities Register */
    uint32_t res5[3];
    uint32_t lr[64]; /* 0x100 RW 0x00000000 List Registers 0-63, see LRn */
};

#ifndef GIC_PL400_VCPUCTRL_PPTR
#error GIC_PL400_VCPUCTRL_PPTR must be defined for virtual memory access to the gic virtual cpu interface control
#else  /* GIC_PL400_GICVCPUCTRL_PPTR */
static volatile struct gich_vcpu_ctrl_map *gic_vcpu_ctrl =
    (volatile struct gich_vcpu_ctrl_map*)(GIC_PL400_VCPUCTRL_PPTR);
#endif /* GIC_PL400_GICVCPUCTRL_PPTR */

static unsigned int gic_vcpu_num_list_regs;

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_hcr(void)
{
    return gic_vcpu_ctrl->hcr;
}

/** DONT_TRANSLATE */
static inline void
set_gic_vcpu_ctrl_hcr(uint32_t hcr)
{
    gic_vcpu_ctrl->hcr = hcr;
}

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_vmcr(void)
{
    return gic_vcpu_ctrl->vmcr;
}

/** DONT_TRANSLATE */
static inline void
set_gic_vcpu_ctrl_vmcr(uint32_t vmcr)
{
    gic_vcpu_ctrl->vmcr = vmcr;
}

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_apr(void)
{
    return gic_vcpu_ctrl->apr;
}

/** DONT_TRANSLATE */
static inline void
set_gic_vcpu_ctrl_apr(uint32_t apr)
{
    gic_vcpu_ctrl->apr = apr;
}

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_vtr(void)
{
    return gic_vcpu_ctrl->vtr;
}

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_eisr0(void)
{
    return gic_vcpu_ctrl->eisr0;
}

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_eisr1(void)
{
    return gic_vcpu_ctrl->eisr1;
}

/** DONT_TRANSLATE */
static inline uint32_t
get_gic_vcpu_ctrl_misr(void)
{
    return gic_vcpu_ctrl->misr;
}

/** DONT_TRANSLATE */
static inline virq_t
get_gic_vcpu_ctrl_lr(int num)
{
    virq_t virq;
    virq.words[0] = gic_vcpu_ctrl->lr[num];
    return virq;
}

/** DONT_TRANSLATE */
static inline void
set_gic_vcpu_ctrl_lr(int num, virq_t lr)
{
    gic_vcpu_ctrl->lr[num] = lr.words[0];
}

static void
vcpu_enable(vcpu_t *vcpu)
{
    setSCTLR(vcpu->cpx.sctlr);
    setHCR(HCR_VCPU);
    isb();

    /* Turn on the VGIC */
    set_gic_vcpu_ctrl_hcr(vcpu->vgic.hcr);
}

static void
vcpu_disable(vcpu_t *vcpu)
{
    dsb();
    if (likely(vcpu)) {
        vcpu->vgic.hcr = get_gic_vcpu_ctrl_hcr();
        vcpu->cpx.sctlr = getSCTLR();
        isb();
    }
    /* Turn off the VGIC */
    set_gic_vcpu_ctrl_hcr(0);
    isb();

    /* Stage 1 MMU off */
    setSCTLR(SCTLR_DEFAULT);
    setHCR(HCR_NATIVE);
    isb();
}

BOOT_CODE void
vcpu_boot_init(void)
{
    gic_vcpu_num_list_regs = VGIC_VTR_NLISTREGS(get_gic_vcpu_ctrl_vtr());
    if (gic_vcpu_num_list_regs > GIC_VCPU_MAX_NUM_LR) {
        printf("Warning: VGIC is reporting more list registers than we support. Truncating\n");
        gic_vcpu_num_list_regs = GIC_VCPU_MAX_NUM_LR;
    }
    vcpu_disable(NULL);
    armHSCurVCPU = NULL;
    armHSVCPUActive = false;
}

static void
vcpu_save(vcpu_t *vcpu, bool_t active)
{
    int i;

    assert(vcpu);
    dsb();
    /* If we aren't active then this state already got stored when
     * we were disabled */
    if (active) {
        vcpu->cpx.sctlr = getSCTLR();
        vcpu->vgic.hcr = get_gic_vcpu_ctrl_hcr();
    }
    /* Store VCPU state */
    vcpu->cpx.actlr = getACTLR();

    /* Store GIC VCPU control state */
    vcpu->vgic.vmcr = get_gic_vcpu_ctrl_vmcr();
    vcpu->vgic.apr = get_gic_vcpu_ctrl_apr();
    for (i = 0; i < gic_vcpu_num_list_regs; i++) {
        vcpu->vgic.lr[i] = get_gic_vcpu_ctrl_lr(i);
    }

    isb();
}


static uint32_t
readVCPUReg(vcpu_t *vcpu, uint32_t field)
{
    switch (field) {
    case 0:
        return vcpu->cpx.sctlr;
    default:
        fail("Unknown VCPU field");
    }
}

static void
writeVCPUReg(vcpu_t *vcpu, uint32_t field, uint32_t value)
{
    switch (field) {
    case 0:
        vcpu->cpx.sctlr = value;
        break;
    default:
        fail("Unknown VCPU field");
    }
}

void
vcpu_restore(vcpu_t *vcpu)
{
    assert(vcpu);
    int i;
    /* Turn off the VGIC */
    set_gic_vcpu_ctrl_hcr(0);
    isb();

    /* Restore GIC VCPU control state */
    set_gic_vcpu_ctrl_vmcr(vcpu->vgic.vmcr);
    set_gic_vcpu_ctrl_apr(vcpu->vgic.apr);
    for (i = 0; i < gic_vcpu_num_list_regs; i++) {
        set_gic_vcpu_ctrl_lr(i, vcpu->vgic.lr[i]);
    }

    /* Restore and enable VCPU state */
    setACTLR(vcpu->cpx.actlr);
    vcpu_enable(vcpu);
}

void
VGICMaintenance(void)
{
    uint32_t eisr0, eisr1;
    uint32_t flags;
    eisr0 = get_gic_vcpu_ctrl_eisr0();
    eisr1 = get_gic_vcpu_ctrl_eisr1();
    flags = get_gic_vcpu_ctrl_misr();

    if (flags & VGIC_MISR_EOI) {
        int irq_idx;
        if (eisr0) {
            irq_idx = __builtin_ctz(eisr0);
        } else if (eisr1) {
            irq_idx = __builtin_ctz(eisr1) + 32;
        } else {
            irq_idx = -1;
        }
        if (irq_idx == -1) {
            current_fault = fault_vgic_maintenance_new(0, 0);
        } else {
            current_fault = fault_vgic_maintenance_new(irq_idx, 1);
            /* the hardware should never give us an invalid index, but we don't
             * want to trust it that far */
            if (irq_idx < gic_vcpu_num_list_regs) {
                virq_t virq = get_gic_vcpu_ctrl_lr(irq_idx);
                switch (virq_get_virqType(virq)) {
                case virq_virq_active:
                    virq = virq_virq_active_set_virqEOIIRQEN(virq, 0);
                    break;
                case virq_virq_pending:
                    virq = virq_virq_pending_set_virqEOIIRQEN(virq, 0);
                    break;
                case virq_virq_invalid:
                    virq = virq_virq_invalid_set_virqEOIIRQEN(virq, 0);
                    break;
                }
                set_gic_vcpu_ctrl_lr(irq_idx, virq);
            }
        }

    } else {
        /* Assume that it was an EOI for a LR that was not present */
        current_fault = fault_vgic_maintenance_new(0, 0);
    }

    handleFault(ksCurThread);
}

void
vcpu_init(vcpu_t *vcpu)
{
    /* CPX registers */
    vcpu->cpx.sctlr = SCTLR_DEFAULT;
    vcpu->cpx.actlr = ACTLR_DEFAULT;
    /* GICH VCPU interface control */
    vcpu->vgic.hcr = VGIC_HCR_EN;
}

void
vcpu_switch(vcpu_t *new)
{
    if (likely(armHSCurVCPU != new)) {
        if (unlikely(new != NULL)) {
            if (unlikely(armHSCurVCPU != NULL)) {
                vcpu_save(armHSCurVCPU, armHSVCPUActive);
            }
            vcpu_restore(new);
            armHSCurVCPU = new;
            armHSVCPUActive = true;
        } else if (unlikely(armHSVCPUActive)) {
            /* leave the current VCPU state loaded, but disable vgic and mmu */
            vcpu_disable(armHSCurVCPU);
            armHSVCPUActive = false;
        }
    } else if (likely(!armHSVCPUActive && new != NULL)) {
        isb();
        vcpu_enable(new);
        armHSVCPUActive = true;
    }
}

static void
vcpu_invalidate_active(void)
{
    if (armHSVCPUActive) {
        vcpu_disable(NULL);
        armHSVCPUActive = false;
    }
    armHSCurVCPU = NULL;
}

static void
vcpu_clean_invalidate_active(void)
{
    vcpu_save(armHSCurVCPU, armHSVCPUActive);
    vcpu_invalidate_active();
}

void
vcpu_finalise(vcpu_t *vcpu)
{
    if (vcpu->tcb) {
        dissociateVCPUTCB(vcpu, vcpu->tcb);
    }
    if (vcpu == armHSCurVCPU) {
        vcpu_invalidate_active();
    }
}

void
associateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    if (tcb->tcbArch.vcpu) {
        dissociateVCPUTCB(tcb->tcbArch.vcpu, tcb);
    }
    if (vcpu->tcb) {
        dissociateVCPUTCB(vcpu, vcpu->tcb);
    }
    vcpu->tcb = tcb;
    tcb->tcbArch.vcpu = vcpu;
}

void
dissociateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    if (tcb->tcbArch.vcpu != vcpu || vcpu->tcb != tcb) {
        fail("TCB and VCPU not associated.");
    }
    tcb->tcbArch.vcpu = NULL;
    vcpu->tcb = NULL;
}

exception_t
invokeVCPUWriteReg(vcpu_t *vcpu, uint32_t field, uint32_t value)
{
    if (armHSCurVCPU == vcpu) {
        vcpu_clean_invalidate_active();
    }
    writeVCPUReg(vcpu, field, value);
    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeVCPUWriteReg(cap_t cap, unsigned int length, word_t* buffer)
{
    uint32_t field;
    uint32_t value;
    if (length < 2) {
        userError("VCPUWriteReg: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    field = getSyscallArg(0, buffer);
    value = getSyscallArg(1, buffer);
    switch (field) {
    case 0:
        break;
    default:
        userError("VCPUWriteReg: Invalid field 0x%lx.", (long)field);
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    return invokeVCPUWriteReg(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), field, value);
}

exception_t
invokeVCPUReadReg(vcpu_t *vcpu, uint32_t field)
{
    tcb_t *thread;
    thread = ksCurThread;
    if (armHSCurVCPU == vcpu) {
        vcpu_clean_invalidate_active();
    }
    setRegister(thread, msgRegisters[0], readVCPUReg(vcpu, field));
    setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                    seL4_MessageInfo_new(0, 0, 0, 1)));
    setThreadState(thread, ThreadState_Running);
    return EXCEPTION_NONE;
}

exception_t
decodeVCPUReadReg(cap_t cap, unsigned int length, word_t* buffer)
{
    uint32_t field;
    if (length < 1) {
        userError("VCPUReadReg: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    field = getSyscallArg(0, buffer);

    switch (field) {
    case 0:
        break;
    default:
        userError("VCPUReadReg: Invalid field 0x%lx.", (long)field);
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return invokeVCPUReadReg(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), field);
}

exception_t
invokeVCPUInjectIRQ(vcpu_t* vcpu, int index, virq_t virq)
{
    if (likely(armHSCurVCPU == vcpu)) {
        set_gic_vcpu_ctrl_lr(index, virq);
    } else {
        vcpu->vgic.lr[index] = virq;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeVCPUInjectIRQ(cap_t cap, unsigned int length, word_t* buffer)
{
    word_t vid, priority, group, index;
    vcpu_t *vcpu;
    uint32_t mr0, mr1;

    vcpu = VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap));

    if (length < 2) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    mr0 = getSyscallArg(0, buffer);
    mr1 = getSyscallArg(1, buffer);
    vid = mr0 & 0xffff;
    priority = (mr0 >> 16) & 0xff;
    group = (mr0 >> 24) & 0xff;
    index = mr1 & 0xff;

    /* Check IRQ parameters */
    if (vid > (1U << 10) - 1) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = (1U << 10) - 1;
        current_syscall_error.invalidArgumentNumber = 1;
        current_syscall_error.type = seL4_RangeError;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (priority > 31) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = 31;
        current_syscall_error.invalidArgumentNumber = 2;
        current_syscall_error.type = seL4_RangeError;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (group > 1) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = 1;
        current_syscall_error.invalidArgumentNumber = 3;
        current_syscall_error.type = seL4_RangeError;
        return EXCEPTION_SYSCALL_ERROR;
    }
    /* LR index out of range */
    if (index >= gic_vcpu_num_list_regs) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = gic_vcpu_num_list_regs;
        current_syscall_error.invalidArgumentNumber = 4;
        current_syscall_error.type = seL4_RangeError;
        return EXCEPTION_SYSCALL_ERROR;
    }
    /* LR index is in use */
    if (virq_get_virqType(vcpu->vgic.lr[index]) == virq_virq_active) {
        userError("VGIC List register in use.");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }
    virq_t virq = virq_virq_pending_new(group, priority, 1, vid);

    return invokeVCPUInjectIRQ(vcpu, index, virq);
}

exception_t decodeARMVCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (label) {
    case ARMVCPUSetTCB:
        return decodeVCPUSetTCB(cap, extraCaps);
    case ARMVCPUReadReg:
        return decodeVCPUReadReg(cap, length, buffer);
    case ARMVCPUWriteReg:
        return decodeVCPUWriteReg(cap, length, buffer);
    case ARMVCPUInjectIRQ:
        return decodeVCPUInjectIRQ(cap, length, buffer);
    default:
        userError("VCPU: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

exception_t
decodeVCPUSetTCB(cap_t cap, extra_caps_t extraCaps)
{
    cap_t tcbCap;
    if ( extraCaps.excaprefs[0] == NULL) {
        userError("VCPU SetTCB: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    tcbCap  = extraCaps.excaprefs[0]->cap;

    if (cap_get_capType(tcbCap) != cap_thread_cap) {
        userError("TCB cap is not a TCB cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return invokeVCPUSetTCB(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), TCB_PTR(cap_thread_cap_get_capTCBPtr(tcbCap)));
}

exception_t
invokeVCPUSetTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    associateVCPUTCB(vcpu, tcb);

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

void
handleVCPUFault(word_t hsr)
{
    current_fault = fault_vcpu_fault_new(hsr);
    handleFault(ksCurThread);
    schedule();
    activateThread();
}

#endif
