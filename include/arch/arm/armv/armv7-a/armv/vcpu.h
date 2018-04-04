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
    asm ("mrs %[ret], lr_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_svc(word_t val)
{
    asm ("msr lr_svc, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_svc(void)
{
    word_t ret;
    asm ("mrs %[ret], sp_svc" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_svc(word_t val)
{
    asm ("msr sp_svc, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_abt(void)
{
    word_t ret;
    asm ("mrs %[ret], lr_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_abt(word_t val)
{
    asm ("msr lr_abt, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_abt(void)
{
    word_t ret;
    asm ("mrs %[ret], sp_abt" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_abt(word_t val)
{
    asm ("msr sp_abt, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_und(void)
{
    word_t ret;
    asm ("mrs %[ret], lr_und" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_und(word_t val)
{
    asm ("msr lr_und, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_und(void)
{
    word_t ret;
    asm ("mrs %[ret], sp_und" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_und(word_t val)
{
    asm ("msr sp_und, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_irq(void)
{
    word_t ret;
    asm ("mrs %[ret], lr_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_irq(word_t val)
{
    asm ("msr lr_irq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_irq(void)
{
    word_t ret;
    asm ("mrs %[ret], sp_irq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_irq(word_t val)
{
    asm ("msr sp_irq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_lr_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], lr_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_lr_fiq(word_t val)
{
    asm ("msr lr_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_sp_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], sp_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_sp_fiq(word_t val)
{
    asm ("msr sp_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r8_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], r8_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r8_fiq(word_t val)
{
    asm ("msr r8_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r9_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], r9_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r9_fiq(word_t val)
{
    asm ("msr r9_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r10_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], r10_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r10_fiq(word_t val)
{
    asm ("msr r10_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r11_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], r11_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r11_fiq(word_t val)
{
    asm ("msr r11_fiq, %[val]" :: [val]"r"(val));
}

static inline word_t
get_r12_fiq(void)
{
    word_t ret;
    asm ("mrs %[ret], r12_fiq" : [ret]"=r"(ret));
    return ret;
}

static inline void
set_r12_fiq(word_t val)
{
    asm ("msr r12_fiq, %[val]" :: [val]"r"(val));
}

static word_t
readVCPUReg(vcpu_t *vcpu, word_t field)
{
    if (likely(armHSCurVCPU == vcpu)) {
        switch (field) {
        case seL4_VCPUReg_SCTLR:
            /* The SCTLR value is switched to/from hardware when we enable/disable
             * the vcpu, not when we switch vcpus */
            if (armHSVCPUActive) {
                return getSCTLR();
            } else {
                return vcpu->cpx.sctlr;
            }
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
        default:
            fail("Unknown VCPU field");
        }
    } else {
        switch (field) {
        case seL4_VCPUReg_SCTLR:
            return vcpu->cpx.sctlr;
        case seL4_VCPUReg_LRsvc:
            return vcpu->lr_svc;
        case seL4_VCPUReg_SPsvc:
            return vcpu->sp_svc;
        case seL4_VCPUReg_LRabt:
            return vcpu->lr_abt;
        case seL4_VCPUReg_SPabt:
            return vcpu->sp_abt;
        case seL4_VCPUReg_LRund:
            return vcpu->lr_und;
        case seL4_VCPUReg_SPund:
            return vcpu->sp_und;
        case seL4_VCPUReg_LRirq:
            return vcpu->lr_irq;
        case seL4_VCPUReg_SPirq:
            return vcpu->sp_irq;
        case seL4_VCPUReg_LRfiq:
            return vcpu->lr_fiq;
        case seL4_VCPUReg_SPfiq:
            return vcpu->sp_fiq;
        case seL4_VCPUReg_R8fiq:
            return vcpu->r8_fiq;
        case seL4_VCPUReg_R9fiq:
            return vcpu->r9_fiq;
        case seL4_VCPUReg_R10fiq:
            return vcpu->r10_fiq;
        case seL4_VCPUReg_R11fiq:
            return vcpu->r11_fiq;
        case seL4_VCPUReg_R12fiq:
            return vcpu->r12_fiq;
        default:
            fail("Unknown VCPU field");
        }
    }
}

static void
writeVCPUReg(vcpu_t *vcpu, word_t field, word_t value)
{
    if (likely(armHSCurVCPU == vcpu)) {
        switch (field) {
        case seL4_VCPUReg_SCTLR:
            if (armHSVCPUActive) {
                setSCTLR(value);
            } else {
                vcpu->cpx.sctlr = value;
            }
            break;
        case seL4_VCPUReg_LRsvc:
            set_lr_svc(value);
            break;
        case seL4_VCPUReg_SPsvc:
            set_sp_svc(value);
            break;
        case seL4_VCPUReg_LRabt:
            set_lr_abt(value);
            break;
        case seL4_VCPUReg_SPabt:
            set_sp_abt(value);
            break;
        case seL4_VCPUReg_LRund:
            set_lr_und(value);
            break;
        case seL4_VCPUReg_SPund:
            set_sp_und(value);
            break;
        case seL4_VCPUReg_LRirq:
            set_lr_irq(value);
            break;
        case seL4_VCPUReg_SPirq:
            set_sp_irq(value);
            break;
        case seL4_VCPUReg_LRfiq:
            set_lr_fiq(value);
            break;
        case seL4_VCPUReg_SPfiq:
            set_sp_fiq(value);
            break;
        case seL4_VCPUReg_R8fiq:
            set_r8_fiq(value);
            break;
        case seL4_VCPUReg_R9fiq:
            set_r9_fiq(value);
            break;
        case seL4_VCPUReg_R10fiq:
            set_r10_fiq(value);
            break;
        case seL4_VCPUReg_R11fiq:
            set_r11_fiq(value);
            break;
        case seL4_VCPUReg_R12fiq:
            set_r12_fiq(value);
            break;
        default:
            fail("Unknown VCPU field");
        }
    } else {
        switch (field) {
        case seL4_VCPUReg_SCTLR:
            vcpu->cpx.sctlr = value;
            break;
        case seL4_VCPUReg_LRsvc:
            vcpu->lr_svc = value;
            break;
        case seL4_VCPUReg_SPsvc:
            vcpu->sp_svc  = value;
            break;
        case seL4_VCPUReg_LRabt:
            vcpu->lr_abt = value;
            break;
        case seL4_VCPUReg_SPabt:
            vcpu->sp_abt = value;
            break;
        case seL4_VCPUReg_LRund:
            vcpu->lr_und = value;
            break;
        case seL4_VCPUReg_SPund:
            vcpu->sp_und = value;
            break;
        case seL4_VCPUReg_LRirq:
            vcpu->lr_irq = value;
            break;
        case seL4_VCPUReg_SPirq:
            vcpu->sp_irq = value;
            break;
        case seL4_VCPUReg_LRfiq:
            vcpu->lr_fiq = value;
            break;
        case seL4_VCPUReg_SPfiq:
            vcpu->sp_fiq = value;
            break;
        case seL4_VCPUReg_R8fiq:
            vcpu->r8_fiq = value;
            break;
        case seL4_VCPUReg_R9fiq:
            vcpu->r9_fiq = value;
            break;
        case seL4_VCPUReg_R10fiq:
            vcpu->r10_fiq = value;
            break;
        case seL4_VCPUReg_R11fiq:
            vcpu->r11_fiq = value;
            break;
        case seL4_VCPUReg_R12fiq:
            vcpu->r12_fiq = value;
            break;
        default:
            fail("Unknown VCPU field");
        }
    }
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */


#endif

