/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_H
#define __ARCH_MACHINE_H

#define wordRadix 5
#define wordBits (1 << wordRadix)

#ifndef __ASSEMBLER__
#include <stdint.h>
#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <plat/machine/hardware.h>
#include <armv/machine.h>

#include <machine/io.h>


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


word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

/* Architecture specific machine operations */

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline word_t getProcessorID(void)
{
    word_t processor_id;
    MRC("p15, 0, %0, c0, c0, 0", processor_id);
    return processor_id;
}

/** DONT_TRANSLATE */
static inline word_t readSystemControlRegister(void)
{
    word_t scr;
    MRC("p15, 0, %0, c1, c0, 0", scr);
    return scr;
}

/** DONT_TRANSLATE */
static inline void writeSystemControlRegister(word_t scr)
{
    MCR("p15, 0, %0, c1, c0, 0", scr);
}

/** DONT_TRANSLATE */
static inline word_t readAuxiliaryControlRegister(void)
{
    word_t acr;
    MRC("p15, 0, %0, c1, c0, 1", acr);
    return acr;
}

/** DONT_TRANSLATE */
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

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void flushBTAC(void)
{
    asm volatile("mcr p15, 0, %0, c7, c5, 6" : : "r"(0));
}

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void writeContextID(word_t id)
{
    asm volatile("mcr p15, 0, %0, c13, c0, 1" : : "r"(id));
    isb();
}

/* Address space control */
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void writeTTBR0(paddr_t addr)
{
    /* Mask supplied address (retain top 19 bits).  Set the lookup cache bits:
     * outer write-back cacheable, no allocate on write, inner non-cacheable.
     */
    asm volatile("mcr p15, 0, %0, c2, c0, 0" : :
                 "r"((addr & 0xffffe000) | 0x18));
}
static inline void setCurrentPD(paddr_t addr)
{
    /* Mask supplied address (retain top 19 bits).  Set the lookup cache bits:
     * outer write-back cacheable, no allocate on write, inner non-cacheable.
     */
    /* Before changing the PD ensure all memory stores have completed */
    dsb();
    writeTTBR0(addr);
    /* Ensure the PD switch completes before we do anything else */
    isb();
}

/* TLB control */
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void invalidateTLB(void)
{
    dsb();
    asm volatile("mcr p15, 0, %0, c8, c7, 0" : : "r"(0));
    dsb();
    isb();
}
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void invalidateTLB_ASID(hw_asid_t hw_asid)
{
    dsb();
    asm volatile("mcr p15, 0, %0, c8, c7, 2" : : "r"(hw_asid));
    dsb();
    isb();
}
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void invalidateTLB_VAASID(word_t mva_plus_asid)
{
    dsb();
    asm volatile("mcr p15, 0, %0, c8, c7, 1" : : "r"(mva_plus_asid));
    dsb();
    isb();
}
/** MODIFIES: [*] */
void lockTLBEntry(vptr_t vaddr);

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void cleanByVA(vptr_t vaddr, paddr_t paddr)
{
#ifdef ARM_CORTEX_A8
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
/** MODIFIES: [*] */
static inline void cleanByVA_PoU(vptr_t vaddr, paddr_t paddr)
{
#ifdef ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
    asm volatile("mcr p15, 0, %0, c7, c11, 1" : : "r"(vaddr));
#elif defined(CONFIG_ARCH_ARM_V6)
    /* V6 doesn't distinguish PoU and PoC, so use the basic flush. */
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#elif defined(PLAT_EXYNOS5)
    /* Flush to coherency for table walks... Why? */
    asm volatile("mcr p15, 0, %0, c7, c10, 1" : : "r"(vaddr));
#else
    asm volatile("mcr p15, 0, %0, c7, c11, 1" : : "r"(vaddr));
#endif
    /* Erratum 586323 - end with DMB to ensure the write goes out. */
    dmb();
}
/* D-Cache invalidate to PoC (v6/v7 common) */
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void invalidateByVA(vptr_t vaddr, paddr_t paddr)
{
#ifdef ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
    asm volatile("mcr p15, 0, %0, c7, c6, 1" : : "r"(vaddr));
#else
    asm volatile("mcr p15, 0, %0, c7, c6, 1" : : "r"(vaddr));
#endif
    dmb();
}
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
/* I-Cache invalidate to PoU (L2 cache) (v6/v7 common) */
static inline void invalidateByVA_I(vptr_t vaddr, paddr_t paddr)
{
#ifdef ARM_CORTEX_A8
    /* On A8, we just invalidate the lot. */
    asm volatile("mcr p15, 0, %0, c7, c5, 0" : : "r"(0));
#else
    asm volatile("mcr p15, 0, %0, c7, c5, 1" : : "r"(vaddr));
#endif
    isb();
}
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
/* I-Cache invalidate all to PoU (L2 cache) (v6/v7 common) */
static inline void invalidate_I_PoU(void)
{
#ifdef ARM_CORTEX_A8
    /* Erratum 586324 -- perform a dummy cached load before flushing. */
    asm volatile("ldr r0, [sp]" : : : "r0");
#endif
    asm volatile("mcr p15, 0, %0, c7, c5, 0" : : "r"(0));
    isb();
}
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
/* D-Cache clean & invalidate to PoC (v6/v7 common) */
static inline void cleanInvalByVA(vptr_t vaddr, paddr_t paddr)
{
#ifdef ARM_CORTEX_A8
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
/** MODIFIES: [*] */
/** DONT_TRANSLATE */
/* Invalidate branch predictors by VA (v6/v7 common) */
static inline void branchFlush(vptr_t vaddr, paddr_t paddr)
{
    asm volatile("mcr p15, 0, %0, c7, c5, 7" : : "r"(vaddr));
}

/** MODIFIES: [*] */
void cleanInvalidateCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void cleanCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void cleanCacheRange_PoU(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void invalidateCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void invalidateCacheRange_I(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void branchFlushRange(word_t start, word_t end, paddr_t pstart);

/** MODIFIES: [*] */
void clean_D_PoU(void);
/** MODIFIES: [*] */
void cleanInvalidate_D_PoC(void);
/** MODIFIES: [*] */
void cleanCaches_PoU(void);
/** MODIFIES: [*] */
void cleanInvalidateL1Caches(void);

/* Fault status */
/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t PURE getIFSR(void)
{
    word_t IFSR;
    asm volatile("mrc p15, 0, %0, c5, c0, 1" : "=r"(IFSR));
    return IFSR;
}
/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t PURE getDFSR(void)
{
    word_t DFSR;
    asm volatile("mrc p15, 0, %0, c5, c0, 0" : "=r"(DFSR));
    return DFSR;
}
/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t PURE getFAR(void)
{
    word_t FAR;
    asm volatile("mrc p15, 0, %0, c6, c0, 0" : "=r"(FAR));
    return FAR;
}

/* Cleaning memory before user-level access */
static inline void clearMemory(word_t* ptr, word_t bits)
{
    memzero(ptr, BIT(bits));
    cleanCacheRange_PoU((word_t)ptr, (word_t)ptr + BIT(bits) - 1,
                        addrFromPPtr(ptr));
}
#endif /* !__ASSEMBLER__ */

#endif
