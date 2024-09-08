/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <arch/object/vcpu.h>
#include <drivers/timer/arm_generic.h>

/* Trap SMC and override CPSR.AIF */
#define HCR_COMMON ( HCR_TSC | HCR_AMO | HCR_IMO \
                   | HCR_FMO | HCR_DC  | HCR_VM)

/* Allow native tasks to run at PL1, but restrict access */
#define HCR_NATIVE ( HCR_COMMON | HCR_TGE | HCR_TVM | HCR_TTLB | HCR_TCACHE \
                   | HCR_TAC | HCR_SWIO)

#ifdef CONFIG_DISABLE_WFI_WFE_TRAPS
#define HCR_VCPU   (HCR_COMMON)
#else
#define HCR_VCPU   (HCR_COMMON | HCR_TWE | HCR_TWI)
#endif

/* Amongst other things we set the caches to enabled by default. This
 * may cause problems when booting guests that expect caches to be
 * disabled */
#define SCTLR_DEFAULT 0xc5187c
#define ACTLR_DEFAULT 0x40

static inline word_t get_lr_svc(void)
{
    word_t ret;
    asm("mrs %[ret], lr_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void set_lr_svc(word_t val)
{
    asm("msr lr_svc, %[val]" :: [val]"r"(val));
}

static inline word_t get_sp_svc(void)
{
    word_t ret;
    asm("mrs %[ret], sp_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void set_sp_svc(word_t val)
{
    asm("msr sp_svc, %[val]" :: [val]"r"(val));
}

static inline word_t get_spsr_svc(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void set_spsr_svc(word_t val)
{
    asm("msr spsr_svc, %[val]" :: [val]"r"(val));
}

static inline word_t get_lr_abt(void)
{
    word_t ret;
    asm("mrs %[ret], lr_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void set_lr_abt(word_t val)
{
    asm("msr lr_abt, %[val]" :: [val]"r"(val));
}

static inline word_t get_sp_abt(void)
{
    word_t ret;
    asm("mrs %[ret], sp_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void set_sp_abt(word_t val)
{
    asm("msr sp_abt, %[val]" :: [val]"r"(val));
}

static inline word_t get_spsr_abt(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void set_spsr_abt(word_t val)
{
    asm("msr spsr_abt, %[val]" :: [val]"r"(val));
}

static inline word_t get_lr_und(void)
{
    word_t ret;
    asm("mrs %[ret], lr_und" : [ret]"=r"(ret));
    return ret;
}

static inline void set_lr_und(word_t val)
{
    asm("msr lr_und, %[val]" :: [val]"r"(val));
}

static inline word_t get_sp_und(void)
{
    word_t ret;
    asm("mrs %[ret], sp_und" : [ret]"=r"(ret));
    return ret;
}

static inline void set_sp_und(word_t val)
{
    asm("msr sp_und, %[val]" :: [val]"r"(val));
}

static inline word_t get_spsr_und(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_und" : [ret]"=r"(ret));
    return ret;
}

static inline void set_spsr_und(word_t val)
{
    asm("msr spsr_und, %[val]" :: [val]"r"(val));
}

static inline word_t get_lr_irq(void)
{
    word_t ret;
    asm("mrs %[ret], lr_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_lr_irq(word_t val)
{
    asm("msr lr_irq, %[val]" :: [val]"r"(val));
}

static inline word_t get_sp_irq(void)
{
    word_t ret;
    asm("mrs %[ret], sp_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_sp_irq(word_t val)
{
    asm("msr sp_irq, %[val]" :: [val]"r"(val));
}

static inline word_t get_spsr_irq(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_spsr_irq(word_t val)
{
    asm("msr spsr_irq, %[val]" :: [val]"r"(val));
}

static inline word_t get_lr_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], lr_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_lr_fiq(word_t val)
{
    asm("msr lr_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_sp_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], sp_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_sp_fiq(word_t val)
{
    asm("msr sp_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_spsr_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_spsr_fiq(word_t val)
{
    asm("msr spsr_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_r8_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r8_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_r8_fiq(word_t val)
{
    asm("msr r8_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_r9_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r9_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_r9_fiq(word_t val)
{
    asm("msr r9_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_r10_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r10_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_r10_fiq(word_t val)
{
    asm("msr r10_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_r11_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r11_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_r11_fiq(word_t val)
{
    asm("msr r11_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t get_r12_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r12_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void set_r12_fiq(word_t val)
{
    asm("msr r12_fiq, %[val]" :: [val]"r"(val));
}
static inline word_t get_cntv_tval(void)
{
    word_t ret = 0;
    MRC(CNTV_TVAL, ret);
    return ret;
}

static inline void set_cntv_tval(word_t val)
{
    MCR(CNTV_TVAL, val);
}

static inline word_t get_cntv_ctl(void)
{
    word_t ret = 0;
    MRC(CNTV_CTL, ret);
    return ret;
}

static inline void set_cntv_ctl(word_t val)
{
    MCR(CNTV_CTL, val);
}

static inline word_t get_cntkctl(void)
{
    word_t ret = 0;
    MRC(CNTKCTL, ret);
    return ret;
}

static inline void set_cntkctl(word_t val)
{
    MCR(CNTKCTL, val);
}

static inline word_t get_vmpidr(void)
{
    word_t ret = 0;
    MRC(VMPIDR, ret);
    return ret;
}

static inline void set_vmpidr(word_t val)
{
    MCR(VMPIDR, val);
}

/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void set_cntv_cval_64(uint64_t val)
{
    MCRR(CNTV_CVAL, val);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline uint64_t get_cntv_cval_64(void)
{
    uint64_t ret = 0;
    MRRC(CNTV_CVAL, ret);
    return ret;
}

static inline void set_cntv_cval_high(word_t val)
{
    uint64_t ret = get_cntv_cval_64();
    uint64_t cval_high = (uint64_t) val << 32 ;
    uint64_t cval_low = (ret << 32) >> 32;
    set_cntv_cval_64(cval_high | cval_low);
}

static inline word_t get_cntv_cval_high(void)
{
    uint64_t ret = get_cntv_cval_64();
    return (word_t)(ret >> 32);
}

static inline void set_cntv_cval_low(word_t val)
{
    uint64_t ret = get_cntv_cval_64();
    uint64_t cval_high = (ret >> 32) << 32;
    uint64_t cval_low = (uint64_t) val;
    set_cntv_cval_64(cval_high | cval_low);
}

static inline word_t get_cntv_cval_low(void)
{
    uint64_t ret = get_cntv_cval_64();
    return (word_t) ret;
}

/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void set_cntv_off_64(uint64_t val)
{
    MCRR(CNTVOFF, val);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline uint64_t get_cntv_off_64(void)
{
    uint64_t ret = 0;
    MRRC(CNTVOFF, ret);
    return ret;
}

static inline void set_cntv_off_high(word_t val)
{
    uint64_t ret = get_cntv_off_64();
    uint64_t cv_off_high = (uint64_t) val << 32 ;
    uint64_t cv_off_low = (ret << 32) >> 32;
    set_cntv_off_64(cv_off_high | cv_off_low);
}

static inline word_t get_cntv_off_high(void)
{
    uint64_t ret = get_cntv_off_64();
    return (word_t)(ret >> 32);
}

static inline void set_cntv_off_low(word_t val)
{
    uint64_t ret = get_cntv_off_64();
    uint64_t cv_off_high = (ret >> 32) << 32;
    uint64_t cv_off_low = (uint64_t) val;
    set_cntv_off_64(cv_off_high | cv_off_low);
}

static inline word_t get_cntv_off_low(void)
{
    uint64_t ret = get_cntv_off_64();
    return (word_t) ret;
}

static word_t vcpu_hw_read_reg(word_t reg_index)
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
    case seL4_VCPUReg_FPEXC:
        return reg;
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
    case seL4_VCPUReg_CNTV_CTL:
        return get_cntv_ctl();
    case seL4_VCPUReg_CNTV_CVALhigh:
        return get_cntv_cval_high();
    case seL4_VCPUReg_CNTV_CVALlow:
        return get_cntv_cval_low();
    case seL4_VCPUReg_CNTVOFFhigh:
        return get_cntv_off_high();
    case seL4_VCPUReg_CNTVOFFlow:
        return get_cntv_off_low();
    case seL4_VCPUReg_CNTKCTL:
        return get_cntkctl();
    case seL4_VCPUReg_VMPIDR:
        return get_vmpidr();
    default:
        fail("ARM/HYP: Invalid register index");
    }
}

static void vcpu_hw_write_reg(word_t reg_index, word_t reg)
{
    switch (reg_index) {
    case seL4_VCPUReg_SCTLR:
        setSCTLR(reg);
        break;
    case seL4_VCPUReg_ACTLR:
        setACTLR(reg);
        break;
    case seL4_VCPUReg_TTBCR:
        writeTTBCR(reg);
        break;
    case seL4_VCPUReg_TTBR0:
        writeTTBR0(reg);
        break;
    case seL4_VCPUReg_TTBR1:
        writeTTBR1(reg);
        break;
    case seL4_VCPUReg_DACR:
        writeDACR(reg);
        break;
    case seL4_VCPUReg_DFSR:
        setDFSR(reg);
        break;
    case seL4_VCPUReg_IFSR:
        setIFSR(reg);
        break;
    case seL4_VCPUReg_ADFSR:
        setADFSR(reg);
        break;
    case seL4_VCPUReg_AIFSR:
        setAIFSR(reg);
        break;
    case seL4_VCPUReg_DFAR:
        setDFAR(reg);
        break;
    case seL4_VCPUReg_IFAR:
        setIFAR(reg);
        break;
    case seL4_VCPUReg_PRRR:
        setPRRR(reg);
        break;
    case seL4_VCPUReg_NMRR:
        setNMRR(reg);
        break;
    case seL4_VCPUReg_CIDR:
        setCIDR(reg);
        break;
    case seL4_VCPUReg_TPIDRPRW:
        writeTPIDRPRW(reg);
        break;
    case seL4_VCPUReg_FPEXC:
        break;
    case seL4_VCPUReg_LRsvc:
        set_lr_svc(reg);
        break;
    case seL4_VCPUReg_SPsvc:
        set_sp_svc(reg);
        break;
    case seL4_VCPUReg_LRabt:
        set_lr_abt(reg);
        break;
    case seL4_VCPUReg_SPabt:
        set_sp_abt(reg);
        break;
    case seL4_VCPUReg_LRund:
        set_lr_und(reg);
        break;
    case seL4_VCPUReg_SPund:
        set_sp_und(reg);
        break;
    case seL4_VCPUReg_LRirq:
        set_lr_irq(reg);
        break;
    case seL4_VCPUReg_SPirq:
        set_sp_irq(reg);
        break;
    case seL4_VCPUReg_LRfiq:
        set_lr_fiq(reg);
        break;
    case seL4_VCPUReg_SPfiq:
        set_sp_fiq(reg);
        break;
    case seL4_VCPUReg_R8fiq:
        set_r8_fiq(reg);
        break;
    case seL4_VCPUReg_R9fiq:
        set_r9_fiq(reg);
        break;
    case seL4_VCPUReg_R10fiq:
        set_r10_fiq(reg);
        break;
    case seL4_VCPUReg_R11fiq:
        set_r11_fiq(reg);
        break;
    case seL4_VCPUReg_R12fiq:
        set_r12_fiq(reg);
        break;
    case seL4_VCPUReg_SPSRsvc:
        set_spsr_svc(reg);
        break;
    case seL4_VCPUReg_SPSRabt:
        set_spsr_abt(reg);
        break;
    case seL4_VCPUReg_SPSRund:
        set_spsr_und(reg);
        break;
    case seL4_VCPUReg_SPSRirq:
        set_spsr_irq(reg);
        break;
    case seL4_VCPUReg_SPSRfiq:
        set_spsr_fiq(reg);
        break;
    case seL4_VCPUReg_CNTV_CTL:
        set_cntv_ctl(reg);
        break;
    case seL4_VCPUReg_CNTV_CVALhigh:
        set_cntv_cval_high(reg);
        break;
    case seL4_VCPUReg_CNTV_CVALlow:
        set_cntv_cval_low(reg);
        break;
    case seL4_VCPUReg_CNTVOFFhigh:
        set_cntv_off_high(reg);
        break;
    case seL4_VCPUReg_CNTVOFFlow:
        set_cntv_off_low(reg);
        break;
    case seL4_VCPUReg_CNTKCTL:
        set_cntkctl(reg);
        break;
    case seL4_VCPUReg_VMPIDR:
        set_vmpidr(reg);
        break;
    default:
        fail("ARM/HYP: Invalid register index");
    }
}

#ifdef CONFIG_HAVE_FPU
static inline void access_fpexc(vcpu_t *vcpu, bool_t write)
{
    /* save a copy of the current status since
     * the enableFpuHyp modifies the armHSFPUEnabled
     */
    bool_t flag = ARCH_NODE_STATE(armHSFPUEnabled);
    if (!flag) {
        enableFpuInstInHyp();
    }
    if (write) {
        VMSR(FPEXC, vcpu_read_reg(vcpu, seL4_VCPUReg_FPEXC));
    } else {
        word_t fpexc;
        VMRS(FPEXC, fpexc);
        vcpu_write_reg(vcpu, seL4_VCPUReg_FPEXC, fpexc);
    }
    /* restore the status */
    if (!flag) {
        trapFpuInstToHyp();
    }
}
#endif

static inline void armv_vcpu_boot_init(void)
{
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
static inline void armv_vcpu_save(vcpu_t *vcpu, bool_t active)
{
    /* save registers */
    vcpu_save_reg_range(vcpu, seL4_VCPURegSaveRange_start, seL4_VCPURegSaveRange_end);

#ifdef ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
    /* This is done when we are asked to save and restore the CP14 debug context
     * of VCPU threads; the register context is saved into the underlying TCB.
     */
    saveAllBreakpointState(vcpu->vcpuTCB);
#endif
    isb();
#ifdef CONFIG_HAVE_FPU
    /* Other FPU registers are still lazily saved and restored.
     * See the comments in vcpu_enable for more information.
     */
    if (active && nativeThreadUsingFPU(vcpu->vcpuTCB)) {
        access_fpexc(vcpu, false);
    }
#endif
}


static inline void vcpu_enable(vcpu_t *vcpu)
{
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
     * value when VM0 is disabled (vcpu_disable) or saved (vcpu_save).
     *
     *
     */

    vcpu->vcpuTCB->tcbArch.tcbContext.fpuState.fpexc = vcpu_read_reg(vcpu, seL4_VCPUReg_FPEXC);
    access_fpexc(vcpu, true);
#endif
    /* Restore virtual timer state */
    restore_virt_timer(vcpu);

}

static inline void vcpu_disable(vcpu_t *vcpu)
{
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
    if (likely(vcpu)) {
        /* Save virtual timer state */
        save_virt_timer(vcpu);
        /* Mask the virtual timer interrupt */
        maskInterrupt(true, CORE_IRQ_TO_IRQT(CURRENT_CPU_INDEX(), INTERRUPT_VTIMER_EVENT));
    }
}

static inline void armv_vcpu_init(vcpu_t *vcpu)
{
    vcpu_write_reg(vcpu, seL4_VCPUReg_SCTLR, SCTLR_DEFAULT);
    vcpu_write_reg(vcpu, seL4_VCPUReg_ACTLR, ACTLR_DEFAULT);
}

#define HSR_FPU_FAULT   (0x1fe0000a)
#define HSR_TASE_FAULT  (0x1fe00020)

static inline bool_t armv_handleVCPUFault(word_t hsr)
{
    return false;
}

static inline bool_t vcpu_reg_saved_when_disabled(word_t field)
{
    switch (field) {
    case seL4_VCPUReg_SCTLR:
    case seL4_VCPUReg_CNTV_CTL:
    case seL4_VCPUReg_CNTV_CVALhigh:
    case seL4_VCPUReg_CNTV_CVALlow:
    case seL4_VCPUReg_CNTVOFFhigh:
    case seL4_VCPUReg_CNTVOFFlow:
    case seL4_VCPUReg_CNTKCTL:
        return true;
    default:
        return false;
    }
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */


