/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_32_H
#define __ARCH_MACHINE_32_H

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

#define MRC(cpreg, v)  asm volatile("mrc  " cpreg :  "=r"(v))
#define MRRC(cpreg, v) asm volatile("mrrc " cpreg :  "=r"(v))
#define MCR(cpreg, v)                               \
    do {                                            \
        word_t _v = v;                            \
        asm volatile("mcr  " cpreg :: "r" (_v));    \
    }while(0)
#define MCRR(cpreg, v)                              \
    do {                                            \
        uint64_t _v = v;                            \
        asm volatile("mcrr " cpreg :: "r" (_v));    \
    }while(0)

#define SYSTEM_WRITE_WORD(reg, v) MCR(reg, v)
#define SYSTEM_READ_WORD(reg, v)  MRC(reg, v)
#define SYSTEM_WRITE_64(reg, v)  MCRR(reg, v)
#define SYSTEM_READ_64(reg, v)   MRRC(reg, v)

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
#define FPEXC      " p10, 7,  %0, cr8, cr0, 0" /* 32-bit Floating-Point Exception Control register */

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

static inline void writeTTBR0(paddr_t addr)
{
    /* Mask supplied address (retain top 19 bits).  Set the lookup cache bits:
     * outer write-back cacheable, no allocate on write, inner non-cacheable.
     */
    asm volatile("mcr p15, 0, %0, c2, c0, 0" : :
                 "r"((addr & 0xffffe000) | 0x18));
}

static inline void writeTPIDRURW(word_t reg)
{
    asm volatile("mcr p15, 0, %0, c13, c0, 2" :: "r"(reg));
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

static inline word_t readTPIDRURW(void)
{
    word_t reg;
    asm volatile("mrc p15, 0, %0, c13, c0, 2" : "=r"(reg));
    return reg;
}

static inline word_t readMPIDR(void)
{
    word_t reg;
    asm volatile ("mrc p15, 0, %0, c0, c0, 5" : "=r"(reg));
    return reg;
}

static inline void setCurrentPD(paddr_t addr)
{
    /* Mask supplied address (retain top 19 bits).  Set the lookup cache bits:
     * outer write-back cacheable, no allocate on write, inner non-cacheable.
     */
    /* Before changing the PD ensure all memory stores have completed */
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        setCurrentPDPL2(addr);
    } else {
        dsb();
        writeTTBR0(addr);
        /* Ensure the PD switch completes before we do anything else */
        isb();
    }
}

static inline void setKernelStack(word_t stack_address)
{
#ifndef CONFIG_ARCH_ARM_V6
    /* Setup kernel stack pointer.
     * Load the (per-core) kernel stack pointer to TPIDRPRW for faster reloads on traps.
     */
    writeTPIDRPRW(stack_address);
#endif /* CONFIG_ARCH_ARM_V6 */
}

static inline word_t getKernelStack(void)
{
#ifndef CONFIG_ARCH_ARM_V6
    return readTPIDRPRW();
#else
    return ((word_t) kernel_stack_alloc[0]) + BIT(CONFIG_KERNEL_STACK_BITS);
#endif /* CONFIG_ARCH_ARM_V6 */
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
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#else
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#endif
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
#elif defined(CONFIG_PLAT_EXYNOS5)
    /* Flush to coherency for table walks... Why? */
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#elif defined(CONFIG_PLAT_IMX7)
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#elif defined(CONFIG_PLAT_TK1)
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#elif defined(CONFIG_ARM_CORTEX_A53)
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
    asm volatile("mcr p15, 0, %0, c7, c6, 1" : : "r"(vaddr));
#else
    asm volatile("mcr p15, 0, %0, c7, c6, 1" : : "r"(vaddr));
#endif
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
    asm volatile("mcr p15, 0, %0, c7, c14, 1" : : "r"(vaddr));
#else
    asm volatile("mcr p15, 0, %0, c7, c14, 1" : : "r"(vaddr));
#endif
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

static inline word_t PURE getDFSR(void)
{
    word_t DFSR;
    asm volatile("mrc p15, 0, %0, c5, c0, 0" : "=r"(DFSR));
    return DFSR;
}

static inline word_t PURE getFAR(void)
{
    word_t FAR;
    asm volatile("mrc p15, 0, %0, c6, c0, 0" : "=r"(FAR));
    return FAR;
}

static inline word_t getACTLR(void)
{
    word_t ACTLR;
    asm volatile ("mrc p15, 0, %0, c1, c0, 1" : "=r"(ACTLR));
    return ACTLR;
}

static inline void setACTLR(word_t actlr)
{
    asm volatile ("mcr p15, 0, %0, c1, c0, 1" :: "r"(actlr));
}

void arch_clean_invalidate_caches(void);

#endif /* __ARCH_MACHINE_32_H */
