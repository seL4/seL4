/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

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
#include <kernel/stack.h>

/* The new spec requires the use of vmsr/vmrs to access floating point
 * registers (including control registers).
 *
 * GCC will still accept the old MRC/MCR instructions but Clang will not.
 * Both result in the same encoding and are here only to satisfy compilers. */
#define VMRS(vfp_reg, v) asm volatile(".fpu vfp\n" \
                                      "vmrs %0, " vfp_reg : "=r"(v))
#define VMSR(vfp_reg, v)                                 \
    do {                                                 \
        word_t _v = v;                                   \
        asm volatile(".fpu vfp\n"                        \
                     "vmsr " vfp_reg ", %0" :: "r"(_v)); \
    } while(0)

/* VFP registers. */
#define FPEXC      "fpexc" /* 32-bit Floating-Point Exception Control register */
#define FPSCR      "fpscr" /* 32-bit Floating-Point Status and Control register */

/** Generic timer CP15 registers **/
#define CNTFRQ     " p15, 0,  %0, c14,  c0, 0" /* 32-bit RW Counter Frequency register */
#define CNTPCT     " p15, 0, %Q0, %R0, c14   " /* 64-bit RO Physical Count register */
#define CNTKCTL    " p15, 0,  %0, c14,  c1, 0" /* 32-bit RW Timer PL1 Control register */
#define CNTP_TVAL  " p15, 0,  %0, c14,  c2, 0" /* 32-bit RW PL1 Physical TimerValue register */
#define CNTP_CTL   " p15, 0,  %0, c14,  c2, 1" /* 32-bit RW PL1 Physical Timer Control register */
#define CNTV_TVAL  " p15, 0,  %0, c14,  c3, 0" /* 32-bit RW Virtual TimerValue register */
#define CNTV_CTL   " p15, 0,  %0, c14,  c3, 1" /* 32-bit RW Virtual Timer Control register */
#define CNTVCT     " p15, 1, %Q0, %R0, c14   " /* 64-bit RO Virtual Count register */
#define CNTP_CVAL  " p15, 2, %Q0, %R0, c14   " /* 64-bit RW PL1 Physical Timer CompareValue register */
#define CNTV_CVAL  " p15, 3, %Q0, %R0, c14   " /* 64-bit RW Virtual Timer CompareValue register */
#define CNTVOFF    " p15, 4, %Q0, %R0, c14   " /* 64-bit RW Virtual Offset register */
#define CNTHCTL    " p15, 4,  %0, c14,  c1, 0" /* 32-bit RW Timer PL2 Control register */
#define CNTHP_TVAL " p15, 4,  %0, c14,  c2, 0" /* 32-bit RW PL2 Physical TimerValue register */
#define CNTHP_CTL  " p15, 4,  %0, c14,  c2, 1" /* 32-bit RW PL2 Physical Timer Control register */
#define CNTHP_CVAL " p15, 6, %Q0, %R0, c14   " /* 64-bit RW PL2 Physical Timer CompareValue register */
#define PMUSERENR  " p15, 0,  %0,  c9, c14, 0" /* 32-bit RW PMU PL0 enable */
#define ID_DFR0    " p15, 0,  %0,  c0,  c1, 2" /* 32-bit RO Debug feature register */
#define ID_PFR1    " p15, 0,  %0,  c0,  c1, 1" /* 32-bit RO CPU feature register */
#define CPACR      " p15, 0,  %0,  c1,  c0, 2" /* 32-bit Architectural Feature Access Control Register */
#define VMPIDR     " p15, 4,  %0,  c0,  c0, 5" /* 32-bit RW Virtualization Multiprocessor ID Register */

/* Use Hypervisor Physical timer */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define CNT_TVAL CNTHP_TVAL
#define CNT_CT   CNTPCT
#define CNT_CTL  CNTHP_CTL
#define CNT_CVAL CNTHP_CVAL
#else
/* Use virtual timer */
#define CNT_TVAL CNTV_TVAL
#define CNT_CT   CNTVCT
#define CNT_CTL  CNTV_CTL
#define CNT_CVAL CNTV_CVAL
#endif

#ifdef ENABLE_SMP_SUPPORT
/* Use the first two SGI (Software Generated Interrupt) IDs
 * for seL4 IPI implementation. SGIs are per-core banked.
 */
#define irq_remote_call_ipi        0
#define irq_reschedule_ipi         1
#endif /* ENABLE_SMP_SUPPORT */

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

/* Architecture specific machine operations */

static inline word_t getProcessorID(void)
{
    word_t processor_id;
    MRC("p15, 0, %0, c0, c0, 0", processor_id);
    return processor_id;
}


static inline word_t readSystemControlRegister(void)
{
    word_t scr;
    MRC("p15, 0, %0, c1, c0, 0", scr);
    return scr;
}


static inline void writeSystemControlRegister(word_t scr)
{
    MCR("p15, 0, %0, c1, c0, 0", scr);
}


static inline word_t readAuxiliaryControlRegister(void)
{
    word_t acr;
    MRC("p15, 0, %0, c1, c0, 1", acr);
    return acr;
}


static inline void writeAuxiliaryControlRegister(word_t acr)
{
    MCR("p15, 0, %0, c1, c0, 1", acr);
}

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void clearExMonitor(void)
{
    word_t tmp;
    asm volatile("strex r0, r1, [%0]" : : "r"(&tmp) : "r0");
}

static inline void flushBTAC(void)
{
    asm volatile("mcr p15, 0, %0, c7, c5, 6" : : "r"(0));
}

static inline void writeContextID(word_t id)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        writeContextIDPL2(id);
    } else {
        asm volatile("mcr p15, 0, %0, c13, c0, 1" : : "r"(id));
        isb();
    }
}

/* Address space control */

static inline word_t readTTBR0(void)
{
    word_t val = 0;
    asm volatile("mrc p15, 0, %0, c2, c0, 0":"=r"(val):);
    return val;
}

static inline void writeTTBR0(word_t val)
{
    asm volatile("mcr p15, 0, %0, c2, c0, 0":: "r"(val));
}

static inline void writeTTBR0Ptr(paddr_t addr)
{
    /* Mask supplied address (retain top 19 bits).  Set the lookup cache bits:
     * outer write-back cacheable, no allocate on write, inner non-cacheable.
     */
    writeTTBR0((addr & 0xffffe000) | 0x18);
}

static inline word_t readTTBR1(void)
{
    word_t val = 0;
    asm volatile("mrc p15, 0, %0, c2, c0, 1":"=r"(val):);
    return val;
}

static inline void writeTTBR1(word_t val)
{
    asm volatile("mcr p15, 0, %0, c2, c0, 1":: "r"(val));
}


static inline word_t readTTBCR(void)
{
    word_t val = 0;
    asm volatile("mrc p15, 0, %0, c2, c0, 2":"=r"(val):);
    return val;
}

static inline void writeTTBCR(word_t val)
{
    asm volatile("mcr p15, 0, %0, c2, c0, 2":: "r"(val));
}

static inline void writeTPIDRURW(word_t reg)
{
    asm volatile("mcr p15, 0, %0, c13, c0, 2" :: "r"(reg));
}

static inline word_t readTPIDRURW(void)
{
    word_t reg;
    asm volatile("mrc p15, 0, %0, c13, c0, 2" : "=r"(reg));
    return reg;
}

static inline void writeTPIDRURO(word_t reg)
{
    asm volatile("mcr p15, 0, %0, c13, c0, 3" :: "r"(reg));
}

static inline word_t readTPIDRURO(void)
{
    word_t reg;
    asm volatile("mrc p15, 0, %0, c13, c0, 3" : "=r"(reg));
    return reg;
}


static inline void writeTPIDRPRW(word_t reg)
{
    asm volatile("mcr p15, 0, %0, c13, c0, 4" :: "r"(reg));
}

static inline word_t readTPIDRPRW(void)
{
    word_t reg;
    asm volatile("mrc p15, 0, %0, c13, c0, 4" :"=r"(reg));
    return reg;
}

static void arm_save_thread_id(tcb_t *thread)
{
    /* TPIDRURW is writeable from EL0 but not with globals frame. */
    setRegister(thread, TPIDRURW, readTPIDRURW());
    /* This register is read only from userlevel, but could still be updated
     * if the thread is running in a higher priveleged level with a VCPU attached.
     */
    setRegister(thread, TPIDRURO, readTPIDRURO());
}

static void arm_load_thread_id(tcb_t *thread)
{
    writeTPIDRURW(getRegister(thread, TPIDRURW));
    writeTPIDRURO(getRegister(thread, TPIDRURO));
}

static inline word_t readMPIDR(void)
{
    word_t reg;
    asm volatile("mrc p15, 0, %0, c0, c0, 5" : "=r"(reg));
    return reg;
}

static inline void writeDACR(word_t reg)
{
    asm volatile("mcr p15, 0, %0, c3, c0, 0" :: "r"(reg));
}

static inline word_t readDACR(void)
{
    word_t reg;
    asm volatile("mrc p15, 0, %0, c3, c0, 0" : "=r"(reg));
    return reg;
}

static inline void setCurrentPD(paddr_t addr)
{
    /* Before changing the PD ensure all memory stores have completed */
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        setCurrentPDPL2(addr);
    } else {
        dsb();
        writeTTBR0Ptr(addr);
        /* Ensure the PD switch completes before we do anything else */
        isb();
    }
}

static inline void setKernelStack(word_t stack_address)
{
    /* Setup kernel stack pointer.
     * Load the (per-core) kernel stack pointer to TPIDRPRW for faster reloads on traps.
     */
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        writeHTPIDR(stack_address);
    } else {
        writeTPIDRPRW(stack_address);
    }
}

static inline word_t getKernelStack(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        return readHTPIDR();
    } else {
        return readTPIDRPRW();
    }
}

#ifdef ENABLE_SMP_SUPPORT
static inline word_t getHWCPUID(void)
{
    /* See ARM Referce Manual (ARMv7-A and ARMv7-R edition), Section B4.1.106
     * for more details about MPIDR register.
     */
    return readMPIDR() & 0xff;
}
#endif /* ENABLE_SMP_SUPPORT */

/* TLB control */

static inline void invalidateLocalTLB(void)
{
    dsb();
    asm volatile("mcr p15, 0, %0, c8, c7, 0" : : "r"(0));
    dsb();
    isb();
}

static inline void invalidateLocalTLB_ASID(hw_asid_t hw_asid)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        invalidateLocalTLB();
    } else {
        dsb();
        asm volatile("mcr p15, 0, %0, c8, c7, 2" : : "r"(hw_asid));
        dsb();
        isb();
    }
}

static inline void invalidateLocalTLB_VAASID(word_t mva_plus_asid)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        invalidateLocalTLB();
    } else {
        dsb();
        asm volatile("mcr p15, 0, %0, c8, c7, 1" : : "r"(mva_plus_asid));
        dsb();
        isb();
    }
}

void lockTLBEntry(vptr_t vaddr);

static inline void cleanByVA(vptr_t vaddr, paddr_t paddr)
{
#ifdef CONFIG_ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
    /* Erratum 586320 -- clean twice with interrupts disabled. */
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#endif
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
    /* Erratum 586323 - end with DMB to ensure the write goes out. */
    dmb();
}

/* D-Cache clean to PoU (L2 cache) (v6/v7 common) */
static inline void cleanByVA_PoU(vptr_t vaddr, paddr_t paddr)
{
#ifdef CONFIG_ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
    asm volatile("mcr p15, 0, %0, c7, c11, 1" : : "r"(vaddr));
#elif defined(CONFIG_ARCH_ARM_V6)
    /* V6 doesn't distinguish PoU and PoC, so use the basic flush. */
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#elif defined(CONFIG_ARM_CORTEX_A7) || defined(CONFIG_ARM_CORTEX_A15) || \
    defined(CONFIG_ARM_CORTEX_A53)
    /* Flush to coherency for table walks... Why? */
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#else
    asm volatile("mcr p15, 0, %0, c7, c11, 1" : : "r"(vaddr));
#endif
    /* Erratum 586323 - end with DMB to ensure the write goes out. */
    dmb();
}

/* D-Cache invalidate to PoC (v6/v7 common) */
static inline void invalidateByVA(vptr_t vaddr, paddr_t paddr)
{
#ifdef CONFIG_ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
#endif
    asm volatile("mcr p15, 0, %0, c7, c6, 1" : : "r"(vaddr));
    dmb();
}

/* I-Cache invalidate to PoU (L2 cache) (v6/v7 common) */
static inline void invalidateByVA_I(vptr_t vaddr, paddr_t paddr)
{
#ifdef CONFIG_ARM_CORTEX_A8
    /* On A8, we just invalidate the lot. */
    asm volatile("mcr p15, 0, %0, c7, c5, 0" : : "r"(0));
#else
    asm volatile("mcr p15, 0, %0, c7, c5, 1" : : "r"(vaddr));
#endif
    isb();
}

/* I-Cache invalidate all to PoU (L2 cache) (v6/v7 common) */
static inline void invalidate_I_PoU(void)
{
#ifdef CONFIG_ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
#endif
    asm volatile("mcr p15, 0, %0, c7, c5, 0" : : "r"(0));
    isb();
}

/* D-Cache clean & invalidate to PoC (v6/v7 common) */
static inline void cleanInvalByVA(vptr_t vaddr, paddr_t paddr)
{
#ifdef CONFIG_ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
    /* Erratum 586320 -- clean twice with interrupts disabled. */
    asm volatile("mcr p15, 0, %0, c7, c14, 1" : : "r"(vaddr));
#endif
    asm volatile("mcr p15, 0, %0, c7, c14, 1" : : "r"(vaddr));
    dsb();
}

/* Invalidate branch predictors by VA (v6/v7 common) */
static inline void branchFlush(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("mcr p15, 0, %0, c7, c5, 7" : : "r"(vaddr));
}

/* Fault status */

static inline word_t PURE getIFSR(void)
{
    word_t IFSR;
    asm volatile("mrc p15, 0, %0, c5, c0, 1" : "=r"(IFSR));
    return IFSR;
}

static inline void setIFSR(word_t ifsr)
{
    asm volatile("mcr p15, 0, %0, c5, c0, 1" : : "r"(ifsr));
}

static inline word_t PURE getDFSR(void)
{
    word_t DFSR;
    asm volatile("mrc p15, 0, %0, c5, c0, 0" : "=r"(DFSR));
    return DFSR;
}

static inline void setDFSR(word_t dfsr)
{
    asm volatile("mcr p15, 0, %0, c5, c0, 0" : : "r"(dfsr));
}

static inline word_t PURE getADFSR(void)
{
    word_t ADFSR;
    asm volatile("mrc p15, 0, %0, c5, c1, 0" : "=r"(ADFSR));
    return ADFSR;
}

static inline void setADFSR(word_t adfsr)
{
    asm volatile("mcr p15, 0, %0, c5, c1, 0" : : "r"(adfsr));
}

static inline word_t PURE getAIFSR(void)
{
    word_t AIFSR;
    asm volatile("mrc p15, 0, %0, c5, c1, 1" : "=r"(AIFSR));
    return AIFSR;
}

static inline void setAIFSR(word_t aifsr)
{
    asm volatile("mcr p15, 0, %0, c5, c1, 1" : : "r"(aifsr));
}

static inline word_t PURE getDFAR(void)
{
    word_t DFAR;
    asm volatile("mrc p15, 0, %0, c6, c0, 0" : "=r"(DFAR));
    return DFAR;
}

static inline void setDFAR(word_t dfar)
{
    asm volatile("mcr p15, 0, %0, c6, c0, 0" : : "r"(dfar));
}

static inline word_t PURE getIFAR(void)
{
    word_t IFAR;
    asm volatile("mrc p15, 0, %0, c6, c0, 2" : "=r"(IFAR));
    return IFAR;
}

static inline void setIFAR(word_t ifar)
{
    asm volatile("mcr p15, 0, %0, c6, c0, 2" : : "r"(ifar));
}

static inline word_t getPRRR(void)
{
    word_t PRRR;
    asm volatile("mrc p15, 0, %0, c10, c2, 0" : "=r"(PRRR));
    return PRRR;
}

static inline void setPRRR(word_t prrr)
{
    asm volatile("mcr p15, 0, %0, c10, c2, 0" : : "r"(prrr));
}

static inline word_t getNMRR(void)
{
    word_t NMRR;
    asm volatile("mrc p15, 0, %0, c10, c2, 1" : "=r"(NMRR));
    return NMRR;
}

static inline void setNMRR(word_t nmrr)
{
    asm volatile("mcr p15, 0, %0, c10, c2, 1" : : "r"(nmrr));
}

static inline word_t PURE getFAR(void)
{
    word_t FAR;
    asm volatile("mrc p15, 0, %0, c6, c0, 0" : "=r"(FAR));
    return FAR;
}

static inline word_t getCIDR(void)
{
    word_t CIDR;
    asm volatile("mrc p15, 0, %0, c13, c0, 1" : "=r"(CIDR));
    return CIDR;
}

static inline void setCIDR(word_t cidr)
{
    asm volatile("mcr p15, 0, %0, c13, c0, 1" : : "r"(cidr));
}

static inline word_t getACTLR(void)
{
    word_t ACTLR;
    asm volatile("mrc p15, 0, %0, c1, c0, 1" : "=r"(ACTLR));
    return ACTLR;
}

static inline void setACTLR(word_t actlr)
{
    asm volatile("mcr p15, 0, %0, c1, c0, 1" :: "r"(actlr));
}

void arch_clean_invalidate_caches(void);
void arch_clean_invalidate_L1_caches(word_t type);
