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
#include <armv/vcpu.h>
#include <plat/machine/devices.h>
#include <arch/machine/debug.h> /* Arch_debug[A/Di]ssociateVCPUTCB() */
#include <arch/machine/debug_conf.h>
#include <arch/machine/gic_pl390.h>

static inline word_t
get_cntv_tval(void)
{
    word_t ret = 0;
    MRC(CNTV_TVAL, ret);
    return ret;
}

static inline void
set_cntv_tval(word_t val)
{
    MCR(CNTV_TVAL, val);
}

static inline word_t
get_cntv_ctl(void)
{
    word_t ret = 0;
    MRC(CNTV_CTL, ret);
    return ret;
}

static inline void
set_cntv_ctl(word_t val)
{
    MCR(CNTV_CTL, val);
}


static word_t
vcpu_hw_read_reg(word_t reg_index)
{
    word_t reg = 0;
    switch (reg_index) {
        case seL4_VCPUReg_SCTLR:
            return getSCTLR();
        case seL4_VCPUReg_ACTLR:
            return getACTLR();
        case seL4_VCPUReg_TTBCR:
            return readTTBCR();
        case seL4_VCPUReg_TTBR0:
            return readTTBR0();
        case seL4_VCPUReg_TTBR1:
            return readTTBR1();
        case seL4_VCPUReg_DACR:
            return readDACR();
        case seL4_VCPUReg_DFSR:
            return getDFSR();
        case seL4_VCPUReg_IFSR:
            return getIFSR();
        case seL4_VCPUReg_ADFSR:
            return getADFSR();
        case seL4_VCPUReg_AIFSR:
            return getAIFSR();
        case seL4_VCPUReg_DFAR:
            return getDFAR();
        case seL4_VCPUReg_IFAR:
            return getIFAR();
        case seL4_VCPUReg_PRRR:
            return getPRRR();
        case seL4_VCPUReg_NMRR:
            return getNMRR();
        case seL4_VCPUReg_CIDR:
            return getCIDR();
        case seL4_VCPUReg_TPIDRPRW:
            return readTPIDRPRW();
        case seL4_VCPUReg_TPIDRURO:
            return readTPIDRURO();
        case seL4_VCPUReg_TPIDRURW:
            return readTPIDRURW();
        case seL4_VCPUReg_FPEXC:
            return reg;
        case seL4_VCPUReg_CNTV_TVAL:
            return get_cntv_tval();
        case seL4_VCPUReg_CNTV_CTL:
            return get_cntv_ctl();
        case seL4_VCPUReg_LRsvc:
            return get_lr_svc();
        case seL4_VCPUReg_SPsvc:
            return get_sp_svc();
        case seL4_VCPUReg_LRabt:
            return get_lr_abt();
        case seL4_VCPUReg_SPabt:
            return get_sp_abt();
        case seL4_VCPUReg_LRund:
            return get_lr_und();
        case seL4_VCPUReg_SPund:
            return get_sp_und();
        case seL4_VCPUReg_LRirq:
            return get_lr_irq();
        case seL4_VCPUReg_SPirq:
            return get_sp_irq();
        case seL4_VCPUReg_LRfiq:
            return get_lr_fiq();
        case seL4_VCPUReg_SPfiq:
            return get_sp_fiq();
        case seL4_VCPUReg_R8fiq:
            return get_r8_fiq();
        case seL4_VCPUReg_R9fiq:
            return get_r9_fiq();
        case seL4_VCPUReg_R10fiq:
            return get_r10_fiq();
        case seL4_VCPUReg_R11fiq:
            return get_r11_fiq();
        case seL4_VCPUReg_R12fiq:
            return get_r12_fiq();
        case seL4_VCPUReg_SPSRsvc:
            return get_spsr_svc();
        case seL4_VCPUReg_SPSRabt:
            return get_spsr_abt();
        case seL4_VCPUReg_SPSRund:
            return get_spsr_und();
        case seL4_VCPUReg_SPSRirq:
            return get_spsr_irq();
        case seL4_VCPUReg_SPSRfiq:
            return get_spsr_fiq();
        default:
            fail("ARM/HYP: Invalid register index");
    }
}

static void
vcpu_hw_write_reg(word_t reg_index, word_t reg)
{
    switch (reg_index) {
        case seL4_VCPUReg_SCTLR:
            return setSCTLR(reg);
        case seL4_VCPUReg_ACTLR:
            return setACTLR(reg);
        case seL4_VCPUReg_TTBCR:
            return writeTTBCR(reg);
        case seL4_VCPUReg_TTBR0:
            return writeTTBR0(reg);
        case seL4_VCPUReg_TTBR1:
            return writeTTBR1(reg);
        case seL4_VCPUReg_DACR:
            return writeDACR(reg);
        case seL4_VCPUReg_DFSR:
            return setDFSR(reg);
        case seL4_VCPUReg_IFSR:
            return setIFSR(reg);
        case seL4_VCPUReg_ADFSR:
            return setADFSR(reg);
        case seL4_VCPUReg_AIFSR:
            return setAIFSR(reg);
        case seL4_VCPUReg_DFAR:
            return setDFAR(reg);
        case seL4_VCPUReg_IFAR:
            return setIFAR(reg);
        case seL4_VCPUReg_PRRR:
            return setPRRR(reg);
        case seL4_VCPUReg_NMRR:
            return setNMRR(reg);
        case seL4_VCPUReg_CIDR:
            return setCIDR(reg);
        case seL4_VCPUReg_TPIDRPRW:
            return writeTPIDRPRW(reg);
        case seL4_VCPUReg_TPIDRURO:
            return writeTPIDRURO(reg);
        case seL4_VCPUReg_TPIDRURW:
            return writeTPIDRURW(reg);
        case seL4_VCPUReg_FPEXC:
            return;
        case seL4_VCPUReg_CNTV_TVAL:
            return set_cntv_tval(reg);
        case seL4_VCPUReg_CNTV_CTL:
            return set_cntv_ctl(reg);
        case seL4_VCPUReg_LRsvc:
            return set_lr_svc(reg);
        case seL4_VCPUReg_SPsvc:
            return set_sp_svc(reg);
        case seL4_VCPUReg_LRabt:
            return set_lr_abt(reg);
        case seL4_VCPUReg_SPabt:
            return set_sp_abt(reg);
        case seL4_VCPUReg_LRund:
            return set_lr_und(reg);
        case seL4_VCPUReg_SPund:
            return set_sp_und(reg);
        case seL4_VCPUReg_LRirq:
            return set_lr_irq(reg);
        case seL4_VCPUReg_SPirq:
            return set_sp_irq(reg);
        case seL4_VCPUReg_LRfiq:
            return set_lr_fiq(reg);
        case seL4_VCPUReg_SPfiq:
            return set_sp_fiq(reg);
        case seL4_VCPUReg_R8fiq:
            return set_r8_fiq(reg);
        case seL4_VCPUReg_R9fiq:
            return set_r9_fiq(reg);
        case seL4_VCPUReg_R10fiq:
            return set_r10_fiq(reg);
        case seL4_VCPUReg_R11fiq:
            return set_r11_fiq(reg);
        case seL4_VCPUReg_R12fiq:
            return set_r12_fiq(reg);
        case seL4_VCPUReg_SPSRsvc:
            return set_spsr_svc(reg);
        case seL4_VCPUReg_SPSRabt:
            return set_spsr_abt(reg);
        case seL4_VCPUReg_SPSRund:
            return set_spsr_und(reg);
        case seL4_VCPUReg_SPSRirq:
            return set_spsr_irq(reg);
        case seL4_VCPUReg_SPSRfiq:
            return set_spsr_fiq(reg);
        default:
            fail("ARM/HYP: Invalid register index");
    }
}



static inline void
vcpu_save_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu->regs[reg] = vcpu_hw_read_reg(reg);
}

static inline void
vcpu_save_reg_range(vcpu_t *vcpu, word_t start, word_t end)
{
    for (word_t i = start; i <= end; i++) {
        vcpu_save_reg(vcpu, i);
    }
}

static inline void
vcpu_restore_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu_hw_write_reg(reg, vcpu->regs[reg]);
}

static inline void
vcpu_restore_reg_range(vcpu_t *vcpu, word_t start, word_t end)
{
    for (word_t i = start; i <= end; i++) {
        vcpu_restore_reg(vcpu, i);
    }
}

static inline word_t
vcpu_read_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return 0;
    }
    return vcpu->regs[reg];
}

static inline void
vcpu_write_reg(vcpu_t *vcpu, word_t reg, word_t value)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu->regs[reg] = value;
}

#ifdef CONFIG_HAVE_FPU
static inline void
access_fpexc(vcpu_t *vcpu, bool_t write)
{
    /* save a copy of the current status since
     * the enableFpuHyp modifies the armHSFPUEnabled
     */
    bool_t flag = armHSFPUEnabled;
    if (!flag) {
        enableFpuInstInHyp();
    }
    if (write) {
        MCR(FPEXC, vcpu_read_reg(vcpu, seL4_VCPUReg_FPEXC));
    } else {
        word_t fpexc;
        MRC(FPEXC, fpexc);
        vcpu_write_reg(vcpu, seL4_VCPUReg_FPEXC, fpexc);
    }
    /* restore the status */
    if (!flag) {
        trapFpuInstToHyp();
    }
}
#endif

static void
vcpu_enable(vcpu_t *vcpu)
{
#ifdef CONFIG_ARCH_AARCH64
    armv_vcpu_enable(vcpu);
#else
    vcpu_restore_reg(vcpu, seL4_VCPUReg_SCTLR);
    setHCR(HCR_VCPU);
    isb();

    /* Turn on the VGIC */
    set_gic_vcpu_ctrl_hcr(vcpu->vgic.hcr);

#if !defined(ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS) && defined(ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS)
    /* This is guarded by an #ifNdef (negation) ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS
     * because if it wasn't, we'd be calling restore_user_debug_context twice
     * on a debug-API build; recall that restore_user_debug_context is called
     * in restore_user_context.
     *
     * We call restore_user_debug_context here, because vcpu_restore calls this
     * function (vcpu_enable). It's better to embed the
     * restore_user_debug_context call in here than to call it in the outer
     * level caller (vcpu_switch), because if the structure of this VCPU code
     * changes later on, it will be less likely that the person who changes
     * the code will be able to omit the debug register context restore, if
     * it's done here.
     */
    restore_user_debug_context(vcpu->vcpuTCB);
#endif
#if defined(ARM_HYP_TRAP_CP14_IN_NATIVE_USER_THREADS)
    /* Disable debug exception trapping and let the PL1 Guest VM handle all
     * of its own debug faults.
     */
    setHDCRTrapDebugExceptionState(false);
#endif
#ifdef CONFIG_HAVE_FPU
    /* We need to restore the FPEXC value early for the following reason:
     *
     * 1: When an application inside a VM is trying to execute an FPU
     * instruction and the EN bit of FPEXC is disabled, an undefined
     * instruction exception is sent to the guest Linux kernel instead of
     * the seL4. Until the Linux kernel examines the EN bit of the FPEXC
     * to determine if the exception FPU related, a VCPU trap is sent to
     * the seL4 kernel. However, it can be too late to restore the value
     * of saved FPEXC in the VCPU trap handler: if the EN bit of the saved
     * FPEXC is enabled, the Linux kernel thinks the FPU is enabled and
     * thus refuses to handle the exception. The result is the application
     * is killed with the cause of illegal instruction.
     *
     * Note that we restore the FPEXC here, but the current FPU owner
     * can be a different thread. Thus, it seems that we are modifying
     * another thread's FPEXC. However, the modification is OK.
     *
     * 1: If the other thread is a native thread, even if the EN bit of
     * the FPEXC is enabled, a trap th HYP mode will be triggered when
     * the thread tries to use the FPU.
     *
     * 2: If the other thread has a VCPU, the FPEXC is already saved
     * in the VCPU's vcpu->fpexc when the VCPU is saved or disabled.
     *
     * We also overwrite the fpuState.fpexc with the value saved in
     * vcpu->fpexc. Since the following scenario can happen:
     *
     * VM0 (the FPU owner) -> VM1 (update the FPEXC in vcpu_enable) ->
     * switchLocalFpuOwner (save VM0 with modified FPEXC) ->
     * VM1 (the new FPU owner)
     *
     * In the case above, the fpuState.fpexc of VM0 saves the value written
     * by the VM1, but the vcpu->fpexc of VM0 still contains the correct
     * value when VM0 is disabed (vcpu_disable) or saved (vcpu_save).
     *
     *
     */

    vcpu->vcpuTCB->tcbArch.tcbContext.fpuState.fpexc = vcpu_read_reg(vcpu, seL4_VCPUReg_FPEXC);
    access_fpexc(vcpu, true);
#endif
#endif
}

static void
vcpu_disable(vcpu_t *vcpu)
{
#ifdef CONFIG_ARCH_AARCH64
    armv_vcpu_disable(vcpu);
#else
    uint32_t hcr;
    dsb();
    if (likely(vcpu)) {
        hcr = get_gic_vcpu_ctrl_hcr();
        vcpu->vgic.hcr = hcr;
        vcpu_save_reg(vcpu, seL4_VCPUReg_SCTLR);
        isb();
#ifdef CONFIG_HAVE_FPU
        if (nativeThreadUsingFPU(vcpu->vcpuTCB)) {
            access_fpexc(vcpu, false);
        }
#endif
    }
    /* Turn off the VGIC */
    set_gic_vcpu_ctrl_hcr(0);
    isb();

    /* Stage 1 MMU off */
    setSCTLR(SCTLR_DEFAULT);
    setHCR(HCR_NATIVE);

#if defined(ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS)
    /* Disable all breakpoint registers from triggering their
     * respective events, so that when we switch from a guest VM
     * to a native thread, the native thread won't trigger events
     * that were caused by things the guest VM did.
     */
    loadAllDisabledBreakpointState();
#endif
#if defined(ARM_HYP_TRAP_CP14_IN_NATIVE_USER_THREADS)
    /* Enable debug exception trapping and let seL4 trap all PL0 (user) native
     * seL4 threads' debug exceptions, so it can deliver them as fault messages.
     */
    setHDCRTrapDebugExceptionState(true);
#endif
    isb();
#endif
}

BOOT_CODE void
vcpu_boot_init(void)
{
#ifdef CONFIG_ARCH_AARCH64
    armv_vcpu_boot_init();
#endif
    gic_vcpu_num_list_regs = VGIC_VTR_NLISTREGS(get_gic_vcpu_ctrl_vtr());
    if (gic_vcpu_num_list_regs > GIC_VCPU_MAX_NUM_LR) {
        printf("Warning: VGIC is reporting more list registers than we support. Truncating\n");
        gic_vcpu_num_list_regs = GIC_VCPU_MAX_NUM_LR;
    }
    vcpu_disable(NULL);
    armHSCurVCPU = NULL;
    armHSVCPUActive = false;

#if defined(ARM_HYP_TRAP_CP14_IN_VCPU_THREADS) || defined(ARM_HYP_TRAP_CP14_IN_NATIVE_USER_THREADS)
    /* On the verified build, we have implemented a workaround that ensures
     * that we don't need to save and restore the debug coprocessor's state
     * (and therefore don't have to expose the CP14 registers to verification).
     *
     * This workaround is simple: we just trap and intercept all Guest VM
     * accesses to the debug coprocessor, and deliver them as VMFault
     * messages to the VM Monitor. To that end, the VM Monitor can then
     * choose to either kill the Guest VM, or it can also choose to silently
     * step over the Guest VM's accesses to the debug coprocessor, thereby
     * silently eliminating the communication channel between the Guest VMs
     * (because the debug coprocessor acted as a communication channel
     * unless we saved/restored its state between VM switches).
     *
     * This workaround delegates the communication channel responsibility
     * from the kernel to the VM Monitor, essentially.
     */
    initHDCR();
#endif
}

static void
vcpu_save(vcpu_t *vcpu, bool_t active)
{
    word_t i;
    unsigned int lr_num;

    assert(vcpu);
    dsb();
    /* If we aren't active then this state already got stored when
     * we were disabled */
    if (active) {
        vcpu_save_reg(vcpu, seL4_VCPUReg_SCTLR);
        vcpu->vgic.hcr = get_gic_vcpu_ctrl_hcr();
    }

    /* Store GIC VCPU control state */
    vcpu->vgic.vmcr = get_gic_vcpu_ctrl_vmcr();
    vcpu->vgic.apr = get_gic_vcpu_ctrl_apr();
    lr_num = gic_vcpu_num_list_regs;
    for (i = 0; i < lr_num; i++) {
        vcpu->vgic.lr[i] = get_gic_vcpu_ctrl_lr(i);
    }

#ifdef CONFIG_ARCH_AARCH64
    vcpu_save_reg_range(vcpu, seL4_VCPUReg_TTBR0, seL4_VCPUReg_SPSR_EL1);
#else
    /* save registers */
    vcpu_save_reg_range(vcpu, seL4_VCPUReg_ACTLR, seL4_VCPUReg_SPSRfiq);

#ifdef ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
    /* This is done when we are asked to save and restore the CP14 debug context
     * of VCPU threads; the register context is saved into the underlying TCB.
     */
    saveAllBreakpointState(vcpu->vcpuTCB);
#endif
    isb();
#ifdef CONFIG_HAVE_FPU
    /* Other FPU registers are still lazily saved and restored when
     * handleFPUFault is called. See the comments in vcpu_enable
     * for more information.
     */
    if (active && nativeThreadUsingFPU(vcpu->vcpuTCB)) {
        access_fpexc(vcpu, false);
    }
#endif
#endif
}


static uint32_t
readVCPUReg(vcpu_t *vcpu, uint32_t field)
{
    if (likely(armHSCurVCPU == vcpu)) {
        switch (field) {
        case seL4_VCPUReg_SCTLR:
            /* The SCTLR value is switched to/from hardware when we enable/disable
             * the vcpu, not when we switch vcpus */
            if (armHSVCPUActive) {
                return getSCTLR();
            } else {
                return vcpu->regs[seL4_VCPUReg_SCTLR];
            }
        default:
            return vcpu_hw_read_reg(field);
        }
    } else {
        return vcpu_read_reg(vcpu, field);
    }
}

static void
writeVCPUReg(vcpu_t *vcpu, uint32_t field, uint32_t value)
{
    if (likely(armHSCurVCPU == vcpu)) {
        switch (field) {
        case seL4_VCPUReg_SCTLR:
            if (armHSVCPUActive) {
                setSCTLR(value);
            } else {
                vcpu->regs[seL4_VCPUReg_SCTLR] = value;
            }
            break;
        default:
            vcpu_hw_write_reg(field, value);
        }
    } else {
       vcpu_write_reg(vcpu, field, value);
    }
}

void
vcpu_restore(vcpu_t *vcpu)
{
    assert(vcpu);
    word_t i;
    unsigned int lr_num;
    /* Turn off the VGIC */
    set_gic_vcpu_ctrl_hcr(0);
    isb();

    /* Restore GIC VCPU control state */
    set_gic_vcpu_ctrl_vmcr(vcpu->vgic.vmcr);
    set_gic_vcpu_ctrl_apr(vcpu->vgic.apr);
    lr_num = gic_vcpu_num_list_regs;
    for (i = 0; i < lr_num; i++) {
        set_gic_vcpu_ctrl_lr(i, vcpu->vgic.lr[i]);
    }

    /* restore registers */
#ifdef CONFIG_ARCH_AARCH64
    vcpu_restore_reg_range(vcpu, seL4_VCPUReg_TTBR0, seL4_VCPUReg_SPSR_EL1);
#else
    vcpu_restore_reg_range(vcpu, seL4_VCPUReg_ACTLR, seL4_VCPUReg_SPSRfiq);
#endif
    vcpu_enable(vcpu);
}

void
VGICMaintenance(void)
{
    uint32_t eisr0, eisr1;
    uint32_t flags;

    /* The current thread must be runnable at this point as we can only get
     * a VGIC maintenance whilst we are actively running a thread with an
     * associated VCPU. For the moment for the proof we leave a redundant
     * check in here that this is indeed not happening */
    if (!isRunnable(NODE_STATE(ksCurThread))) {
        printf("Received VGIC maintenance on non-runnable thread!\n");
        return;
    }

    eisr0 = get_gic_vcpu_ctrl_eisr0();
    eisr1 = get_gic_vcpu_ctrl_eisr1();
    flags = get_gic_vcpu_ctrl_misr();

    if (flags & VGIC_MISR_EOI) {
        int irq_idx;
        if (eisr0) {
            irq_idx = ctzl(eisr0);
        } else if (eisr1) {
            irq_idx = ctzl(eisr1) + 32;
        } else {
            irq_idx = -1;
        }

        /* the hardware should never give us an invalid index, but we don't
         * want to trust it that far */
        if (irq_idx == -1  || irq_idx >= gic_vcpu_num_list_regs) {
            current_fault = seL4_Fault_VGICMaintenance_new(0, 0);
        } else {
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
            /* decodeVCPUInjectIRQ below checks the vgic.lr register,
             * so we should also sync the shadow data structure as well */
            assert(armHSCurVCPU != NULL && armHSVCPUActive);
            if (armHSCurVCPU != NULL && armHSVCPUActive) {
                armHSCurVCPU->vgic.lr[irq_idx] = virq;
            } else {
                /* FIXME This should not happen */
            }
            current_fault = seL4_Fault_VGICMaintenance_new(irq_idx, 1);
        }

    } else {
        /* Assume that it was an EOI for a LR that was not present */
        current_fault = seL4_Fault_VGICMaintenance_new(0, 0);
    }

    handleFault(NODE_STATE(ksCurThread));
}

void
vcpu_init(vcpu_t *vcpu)
{
#ifdef CONFIG_ARCH_AARCH64
    armv_vcpu_init(vcpu);
#else
    vcpu_write_reg(vcpu, seL4_VCPUReg_SCTLR, SCTLR_DEFAULT);
    vcpu_write_reg(vcpu, seL4_VCPUReg_ACTLR, ACTLR_DEFAULT);
#endif
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
#ifdef ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
            saveAllBreakpointState(armHSCurVCPU->vcpuTCB);
#endif
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

void
vcpu_finalise(vcpu_t *vcpu)
{
    if (vcpu->vcpuTCB) {
        dissociateVCPUTCB(vcpu, vcpu->vcpuTCB);
    }
}

void
associateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    if (tcb->tcbArch.tcbVCPU) {
        dissociateVCPUTCB(tcb->tcbArch.tcbVCPU, tcb);
    }
    if (vcpu->vcpuTCB) {
        dissociateVCPUTCB(vcpu, vcpu->vcpuTCB);
    }
    tcb->tcbArch.tcbVCPU = vcpu;
    vcpu->vcpuTCB = tcb;
}

void
dissociateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    if (tcb->tcbArch.tcbVCPU != vcpu || vcpu->vcpuTCB != tcb) {
        fail("TCB and VCPU not associated.");
    }
    if (vcpu == armHSCurVCPU) {
        vcpu_invalidate_active();
    }
    tcb->tcbArch.tcbVCPU = NULL;
    vcpu->vcpuTCB = NULL;
#ifdef ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
    Arch_debugDissociateVCPUTCB(tcb);
#endif

    /* sanitize the CPSR as without a VCPU a thread should only be in user mode */
#ifdef CONFIG_ARCH_AARCH64
    setRegister(tcb, SPSR_EL1, sanitiseRegister(SPSR_EL1, getRegister(tcb, SPSR_EL1), false));
#else
    setRegister(tcb, CPSR, sanitiseRegister(CPSR, getRegister(tcb, CPSR), false));
#endif
}

exception_t
invokeVCPUWriteReg(vcpu_t *vcpu, word_t field, word_t value)
{
    writeVCPUReg(vcpu, field, value);
    return EXCEPTION_NONE;
}

exception_t
decodeVCPUWriteReg(cap_t cap, unsigned int length, word_t* buffer)
{
    word_t field;
    word_t value;
    if (length < 2) {
        userError("VCPUWriteReg: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    field = getSyscallArg(0, buffer);
    value = getSyscallArg(1, buffer);
    if (field >= seL4_VCPUReg_Num) {
        userError("VCPUWriteReg: Invalid field 0x%lx.", (long)field);
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUWriteReg(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), field, value);
}

exception_t
invokeVCPUReadReg(vcpu_t *vcpu, word_t field, bool_t call)
{
    tcb_t *thread;
    thread = NODE_STATE(ksCurThread);
    word_t value = readVCPUReg(vcpu, field);
    if (call) {
        word_t *ipcBuffer = lookupIPCBuffer(true, thread);
        setRegister(thread, badgeRegister, 0);
        unsigned int length = setMR(thread, ipcBuffer, 0, value);
        setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, length)));
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Running);
    return EXCEPTION_NONE;
}

exception_t
decodeVCPUReadReg(cap_t cap, unsigned int length, bool_t call, word_t* buffer)
{
    word_t field;
    if (length < 1) {
        userError("VCPUReadReg: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    field = getSyscallArg(0, buffer);

    if (field >= seL4_VCPUReg_Num) {
        userError("VCPUReadReg: Invalid field 0x%lx.", (long)field);
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUReadReg(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), field, call);
}

exception_t
invokeVCPUInjectIRQ(vcpu_t* vcpu, unsigned long index, virq_t virq)
{
    if (likely(armHSCurVCPU == vcpu)) {
        set_gic_vcpu_ctrl_lr(index, virq);
    } else {
        vcpu->vgic.lr[index] = virq;
    }

    return EXCEPTION_NONE;
}

exception_t
decodeVCPUInjectIRQ(cap_t cap, unsigned int length, word_t* buffer)
{
    word_t vid, priority, group, index;
    vcpu_t *vcpu;
#ifdef CONFIG_ARCH_AARCH64
    word_t mr0;

    vcpu = VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap));

    if (length < 1) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    mr0 = getSyscallArg(0, buffer);
    vid = mr0 & 0xffff;
    priority = (mr0 >> 16) & 0xff;
    group = (mr0 >> 24) & 0xff;
    index = (mr0 >> 32) & 0xff;
#else
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
#endif

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
        current_syscall_error.rangeErrorMax = gic_vcpu_num_list_regs - 1;
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

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUInjectIRQ(vcpu, index, virq);
}

exception_t decodeARMVCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    bool_t call,
    word_t* buffer
)
{
    switch (label) {
    case ARMVCPUSetTCB:
        return decodeVCPUSetTCB(cap, extraCaps);
    case ARMVCPUReadReg:
        return decodeVCPUReadReg(cap, length, call, buffer);
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

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUSetTCB(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), TCB_PTR(cap_thread_cap_get_capTCBPtr(tcbCap)));
}

exception_t
invokeVCPUSetTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    associateVCPUTCB(vcpu, tcb);

    return EXCEPTION_NONE;
}

#define HSR_FPU_FAULT   (0x1fe0000a)
#define HSR_TASE_FAULT  (0x1fe00020)

void
handleVCPUFault(word_t hsr)
{
#ifdef CONFIG_ARCH_AARCH64
    if (armv_handleVCPUFault(hsr)) {
        return;
    }
#endif
#ifdef CONFIG_HAVE_FPU
    if (hsr == HSR_FPU_FAULT || hsr == HSR_TASE_FAULT) {
        assert(!isFpuEnable());
        handleFPUFault();
        setNextPC(NODE_STATE(ksCurThread), getRestartPC(NODE_STATE(ksCurThread)));
        return;
    }
#endif
    current_fault = seL4_Fault_VCPUFault_new(hsr);
    handleFault(NODE_STATE(ksCurThread));
    schedule();
    activateThread();
}

#endif
