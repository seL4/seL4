/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MODE_MACHINE_H
#define __MODE_MACHINE_H

#define wordRadix 5
#define wordBits (1 << wordRadix)

/* Address space control */
static inline paddr_t getCurrentPD(void)
{
    return ia32KSCurrentPD;
}

static inline void setCurrentPD(paddr_t addr)
{
    ia32KSCurrentPD = addr;
    write_cr3(addr);
}

/* TLB control */
static inline void invalidateTLB(void)
{
    /* rewrite the current page directory */
    write_cr3(ia32KSCurrentPD);
}

static inline void invalidateTLBentry(vptr_t vptr)
{
    asm volatile("invlpg (%[vptr])" :: [vptr] "r"(vptr));
}

/* Invalidates page structures cache */
static inline void invalidatePageStructureCache(void)
{
    /* invalidate an arbitrary line to invalidate the page structure cache */
    invalidateTLBentry(0);
}

/* Flushes entire CPU Cache */
static inline void ia32_wbinvd(void)
{
    asm volatile("wbinvd" ::: "memory");
}

/* GDT installation */
void ia32_install_gdt(gdt_idt_ptr_t* gdt_idt_ptr);

/* IDT installation */
void ia32_install_idt(gdt_idt_ptr_t* gdt_idt_ptr);

/* LDT installation */
void ia32_install_ldt(uint32_t ldt_sel);

/* TSS installation */
void ia32_install_tss(uint32_t tss_sel);

/* Get page fault address from CR2 register */
static inline uint32_t getFaultAddr(void)
{
    return read_cr2();
}

/* Get current stack pointer */
static inline void* get_current_esp(void)
{
    word_t stack;
    void *result;
    asm volatile("movl %[stack_address], %[result]" : [result] "=r"(result) : [stack_address] "r"(&stack));
    return result;
}

/* Cleaning memory before user-level access */
static inline void clearMemory(void* ptr, word_t bits)
{
    memzero(ptr, BIT(bits));
    /* no cleaning of caches necessary on IA-32 */
}

/* Initialises MSRs required to setup sysenter and sysexit */
void init_sysenter_msrs(void);

static uint64_t ia32_rdmsr(const uint32_t reg)
{
    uint64_t value;
    asm volatile("rdmsr" : "=A"(value) : "c"(reg));
    return value;
}

/* Read model specific register */
static inline uint32_t ia32_rdmsr_low(const uint32_t reg)
{
    return (uint32_t)ia32_rdmsr(reg);
}

static inline uint32_t ia32_rdmsr_high(const uint32_t reg)
{
    return (uint32_t)(ia32_rdmsr(reg) >> 32ull);
}

/* Write model specific register */
static inline void ia32_wrmsr(const uint32_t reg, const uint32_t val_high, const uint32_t val_low)
{
    uint64_t val = ((uint64_t)val_high << 32ull) | (uint64_t)val_low;
    asm volatile("wrmsr" :: "A"(val), "c"(reg));
}

/* Read different parts of CPUID */
static inline uint32_t ia32_cpuid_edx(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a" (eax),
                 "=b" (ebx),
                 "=c" (ecx),
                 "=d" (edx)
                 : "a" (eax), "c" (ecx)
                 : "memory");
    return edx;
}

static inline uint32_t ia32_cpuid_eax(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a" (eax),
                 "=b" (ebx),
                 "=c" (ecx),
                 "=d" (edx)
                 : "a" (eax), "c" (ecx)
                 : "memory");
    return eax;
}

/* Read/write memory fence */
static inline void ia32_mfence(void)
{
    asm volatile("mfence" ::: "memory");
}

#endif
