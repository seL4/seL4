/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODE_MACHINE_H
#define __ARCH_MODE_MACHINE_H

#include <config.h>
#include <stdint.h>
#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <plat/machine/hardware.h>
#include <armv/machine.h>
#include <arch/model/smp.h>

#include <machine/io.h>
#include <mode/machine_pl2.h>
#include <mode/hardware.h>

#define MRS(reg, v)  asm volatile("mrs %0," reg : "=r"(v))
#define MSR(reg, v)                                \
    do {                                           \
        word_t _v = v;                             \
        asm volatile("msr " reg ",%0" :: "r" (_v));\
    }while(0)

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

static inline word_t getProcessorID(void)
{
    word_t processor_id;
    MRS("midr_el1", processor_id);
    return processor_id;
}

static inline word_t readSystemControlRegister(void)
{
    word_t scr;
    MRS("sctlr_el1", scr);
    return scr;
}

static inline void writeSystemControlRegister(word_t scr)
{
    MSR("sctlr_el1", scr);
}

static inline word_t readAuxiliaryControlRegister(void)
{
    word_t acr;
    MRS("actlr_el1", acr);
    return acr;
}

static inline void writeAuxiliaryControlRegister(word_t acr)
{
    MSR("actlr_el1", acr);
}

static inline void writeTPIDRPRW(word_t reg)
{
    MSR("tpidr_el1", reg);
}

static inline void writeTPIDRURW(word_t reg)
{
    MSR("tpidr_el0", reg);
}

static inline word_t readTPIDRURW(void)
{
    word_t reg;
    MRS("tpidr_el0", reg);
    return reg;
}

static inline void setCurrentKernelVSpaceRoot(ttbr_t ttbr)
{
    dsb();
    MSR("ttbr1_el1", ttbr.words[0]);
    isb();
}

static inline void setCurrentUserVSpaceRoot(ttbr_t ttbr)
{
    dsb();
    MSR("ttbr0_el1", ttbr.words[0]);
    isb();
}

static inline void setKernelStack(word_t stack_address)
{
    writeTPIDRPRW(stack_address);
}

static inline void setVtable(pptr_t addr)
{
    dsb();
    MSR("vbar_el1", addr);
    isb();
}

static inline void invalidateTLB(void)
{
    dsb();
    asm volatile("tlbi vmalle1");
    dsb();
    isb();
}

static inline void invalidateTLB_ASID(asid_t asid)
{
    assert(asid < BIT(16));

    dsb();
    asm volatile("tlbi aside1, %0" : : "r" (asid << 48));
    dsb();
    isb();
}

static inline void invalidateTLB_VAASID(word_t mva_plus_asid)
{
    dsb();
    asm volatile("tlbi vae1, %0" : : "r" (mva_plus_asid));
    dsb();
    isb();
}

void lockTLBEntry(vptr_t vaddr);

static inline void cleanByVA(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("dc cvac, %0" : : "r" (vaddr));
    dmb();
}

static inline void cleanByVA_PoU(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("dc cvau, %0" : : "r" (vaddr));
    dmb();
}

static inline void invalidateByVA(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("dc ivac, %0" : : "r" (vaddr));
    dmb();
}

static inline void invalidateByVA_I(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("ic ivau, %0" : : "r" (vaddr));
    isb();
}

static inline void invalidate_I_PoU(void)
{
    asm volatile("ic iallu");
    isb();
}

static inline void cleanInvalByVA(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("dc civac, %0" : : "r" (vaddr));
    dsb();
}

static inline void branchFlush(vptr_t vaddr, paddr_t paddr)
{

}

#define getDFSR getESR
#define getIFSR getESR
static inline word_t PURE getESR(void)
{
    word_t ESR;
    MRS("esr_el1", ESR);
    return ESR;
}

static inline word_t PURE getFAR(void)
{
    word_t FAR;
    MRS("far_el1", FAR);
    return FAR;
}

void arch_clean_invalidate_caches(void);

#endif /* __ARCH_MODE_MACHINE_H */
