/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_ARMV_VCPU_H_
#define __ARCH_ARMV_VCPU_H_

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <arch/object/vcpu.h>

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

static inline word_t
get_lr_svc(void)
{
    word_t ret;
    asm("mrs %[ret], lr_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_svc(word_t val)
{
    asm("msr lr_svc, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_svc(void)
{
    word_t ret;
    asm("mrs %[ret], sp_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_svc(word_t val)
{
    asm("msr sp_svc, %[val]" :: [val]"r"(val));
}

static inline word_t
get_spsr_svc(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_spsr_svc(word_t val)
{
    asm("msr spsr_svc, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_abt(void)
{
    word_t ret;
    asm("mrs %[ret], lr_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_abt(word_t val)
{
    asm("msr lr_abt, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_abt(void)
{
    word_t ret;
    asm("mrs %[ret], sp_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_abt(word_t val)
{
    asm("msr sp_abt, %[val]" :: [val]"r"(val));
}

static inline word_t
get_spsr_abt(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_spsr_abt(word_t val)
{
    asm("msr spsr_abt, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_und(void)
{
    word_t ret;
    asm("mrs %[ret], lr_und" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_und(word_t val)
{
    asm("msr lr_und, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_und(void)
{
    word_t ret;
    asm("mrs %[ret], sp_und" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_und(word_t val)
{
    asm("msr sp_und, %[val]" :: [val]"r"(val));
}

static inline word_t
get_spsr_und(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_und" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_spsr_und(word_t val)
{
    asm("msr spsr_und, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_irq(void)
{
    word_t ret;
    asm("mrs %[ret], lr_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_irq(word_t val)
{
    asm("msr lr_irq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_irq(void)
{
    word_t ret;
    asm("mrs %[ret], sp_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_irq(word_t val)
{
    asm("msr sp_irq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_spsr_irq(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_spsr_irq(word_t val)
{
    asm("msr spsr_irq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], lr_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_fiq(word_t val)
{
    asm("msr lr_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], sp_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_fiq(word_t val)
{
    asm("msr sp_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_spsr_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], spsr_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_spsr_fiq(word_t val)
{
    asm("msr spsr_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r8_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r8_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r8_fiq(word_t val)
{
    asm("msr r8_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r9_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r9_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r9_fiq(word_t val)
{
    asm("msr r9_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r10_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r10_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r10_fiq(word_t val)
{
    asm("msr r10_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r11_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r11_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r11_fiq(word_t val)
{
    asm("msr r11_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r12_fiq(void)
{
    word_t ret;
    asm("mrs %[ret], r12_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r12_fiq(word_t val)
{
    asm("msr r12_fiq, %[val]" :: [val]"r"(val));
}
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
    case seL4_VCPUReg_CNTV_TVAL:
        set_cntv_tval(reg);
        break;
    case seL4_VCPUReg_CNTV_CTL:
        set_cntv_ctl(reg);
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
    default:
        fail("ARM/HYP: Invalid register index");
    }
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */


#endif

