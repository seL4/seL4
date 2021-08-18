/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <model/statedata.h>
#include <arch/machine/cpu_registers.h>
#include <arch/model/smp.h>
#include <arch/machine.h>

/* Address space control */
static inline paddr_t getCurrentPD(void)
{
    return MODE_NODE_STATE(ia32KSCurrentPD);
}

static inline void setCurrentPD(paddr_t addr)
{
    MODE_NODE_STATE(ia32KSCurrentPD) = addr;
    write_cr3(addr);
}

static inline void setCurrentVSpaceRoot(paddr_t addr, word_t pcid)
{
    /* pcid is not supported on ia32 and so we should always be passed zero */
    assert(pcid == 0);
    setCurrentPD(addr);
}

static inline cr3_t getCurrentCR3(void)
{
    /* on ia32 the PD is the full value of CR3, so we can just return that */
    return cr3_new(getCurrentPD());
}

static inline void invalidateLocalTLBEntry(vptr_t vptr)
{
    asm volatile("invlpg (%[vptr])" :: [vptr] "r"(vptr));
}

/* Invalidates page structures cache */
static inline void invalidateLocalPageStructureCache(void)
{
    /* invalidate an arbitrary line to invalidate the page structure cache */
    invalidateLocalTLBEntry(0);
}

static inline void invalidateLocalPageStructureCacheASID(paddr_t root, asid_t asid)
{
    /* ignore asid */
    invalidateLocalPageStructureCache();
}

static inline void invalidateLocalTLB(void)
{
    /* rewrite the current page directory */
    write_cr3(MODE_NODE_STATE(ia32KSCurrentPD));
}

static inline void invalidateLocalTranslationSingle(vptr_t vptr)
{
    /* Just invalidate a single entry in the TLB */
    invalidateLocalTLBEntry(vptr);
}

static inline void invalidateLocalTranslationSingleASID(vptr_t vptr, asid_t asid)
{
    /* no asid support in 32-bit, just invalidate TLB */
    invalidateLocalTLBEntry(vptr);
}

static inline void invalidateLocalTranslationAll(void)
{
    invalidateLocalTLB();
    invalidateLocalPageStructureCache();
}

static inline rdmsr_safe_result_t x86_rdmsr_safe(const uint32_t reg)
{
    uint32_t low;
    uint32_t high;
    word_t returnto;
    rdmsr_safe_result_t result;
    asm volatile(
        "movl $1f, (%[returnto_addr]) \n\
         rdmsr \n\
         1: \n\
         movl (%[returnto_addr]), %[returnto] \n\
         movl $0, (%[returnto_addr])"
        : [returnto] "=&r"(returnto),
        [high] "=&d"(high),
        [low] "=&a"(low)
        : [returnto_addr] "r"(&ARCH_NODE_STATE(x86KSGPExceptReturnTo)),
        [reg] "c"(reg)
        : "memory"
    );
    result.success = returnto != 0;
    result.value = ((uint64_t)high << 32) | (uint64_t)low;
    return result;
}

/* GDT installation */
void ia32_install_gdt(gdt_idt_ptr_t *gdt_idt_ptr);

/* IDT installation */
void ia32_install_idt(gdt_idt_ptr_t *gdt_idt_ptr);

/* LDT installation */
void ia32_install_ldt(uint32_t ldt_sel);

/* TSS installation */
void ia32_install_tss(uint32_t tss_sel);

void ia32_load_fs(word_t selector);
void ia32_load_gs(word_t selector);

#if defined(CONFIG_FSGSBASE_GDT) || !defined(CONFIG_FSGSBASE_MSR)

static inline void FORCE_INLINE x86_write_fs_base_impl(word_t base)
{
    gdt_entry_gdt_data_ptr_set_base_low(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_FS], base & 0xFFFF);
    gdt_entry_gdt_data_ptr_set_base_mid(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_FS], (base >> 16) & 0xFF);
    gdt_entry_gdt_data_ptr_set_base_high(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_FS], (base >> 24) & 0xFF);
    asm volatile(
        "movw %0, %%ax\n"
        "movw %%ax, %%fs\n"
        :
        : "i"(SEL_FS)
        : "ax"
    );
}

static inline void FORCE_INLINE x86_write_gs_base_impl(word_t base)
{
    gdt_entry_gdt_data_ptr_set_base_low(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_GS], base & 0xFFFF);
    gdt_entry_gdt_data_ptr_set_base_mid(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_GS], (base >> 16) & 0xFF);
    gdt_entry_gdt_data_ptr_set_base_high(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_GS], (base >> 24) & 0xFF);
    asm volatile(
        "movw %0, %%ax\n"
        "movw %%ax, %%gs\n"
        :
        : "i"(SEL_GS)
        : "ax"
    );
}

static inline word_t FORCE_INLINE x86_read_fs_base_impl(void)
{
    word_t base = 0;
    base &= gdt_entry_gdt_data_ptr_get_base_low(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_FS]) & 0xFFFF;
    base &= (gdt_entry_gdt_data_ptr_get_base_mid(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_FS]) & 0xFF) << 16;
    base &= (gdt_entry_gdt_data_ptr_get_base_high(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_FS]) & 0xFF) << 24;
    return base;
}

static inline word_t FORCE_INLINE x86_read_gs_base_impl(void)
{
    word_t base = 0;
    base &= gdt_entry_gdt_data_ptr_get_base_low(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_GS]) & 0xFFFF;
    base &= (gdt_entry_gdt_data_ptr_get_base_mid(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_GS]) & 0xFF) << 16;
    base &= (gdt_entry_gdt_data_ptr_get_base_high(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt[GDT_GS]) & 0xFF) << 24;
    return base;
}

#elif defined(CONFIG_FSGSBASE_MSR)

static inline void x86_write_gs_base_impl(word_t base)
{
    x86_wrmsr(IA32_GS_BASE_MSR, base);
}

static inline word_t x86_read_gs_base_impl(void)
{
    return x86_rdmsr(IA32_GS_BASE_MSR);
}

#endif

static inline void x86_set_tls_segment_base(word_t tls_base)
{
    x86_write_gs_base(tls_base, CURRENT_CPU_INDEX());
}

static inline void init_syscall_msrs(void)
{
    fail("syscall not supported on ia32");
}

