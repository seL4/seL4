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

#include <model/statedata.h>
#include <arch/machine/cpu_registers.h>
#include <arch/model/smp.h>

/* Get current stack pointer */
static inline void* get_current_esp(void)
{
    word_t stack;
    void *result;
    asm volatile("movl %[stack_address], %[result]" : [result] "=r"(result) : [stack_address] "r"(&stack));
    return result;
}

#if CONFIG_MAX_NUM_NODES > 1
extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(seL4_PageBits)];

static inline cpu_id_t cpuIndexToID(word_t index)
{
    return cpu_mapping.index_to_cpu_id[index];
}

static inline PURE cpu_id_t getCurrentCPUIndex(void)
{
    cpu_id_t cpu_id;
    uint32_t esp = (uint32_t)get_current_esp();

    esp -= (uint32_t)kernel_stack_alloc;
    cpu_id = esp >> 12;
    return cpu_id;
}

static inline PURE word_t getCurrentCPUID(void)
{
    return cpu_mapping.index_to_cpu_id[getCurrentCPUIndex()];
}
#else
extern char kernel_stack_alloc[4096];
#endif /* CONFIG_MAX_NUM_NODES */

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

/* Flushes entire CPU Cache */
static inline void ia32_wbinvd(void)
{
    asm volatile("wbinvd" ::: "memory");
}

static inline void
arch_clean_invalidate_caches(void)
{
    ia32_wbinvd();
}

/* GDT installation */
void ia32_install_gdt(gdt_idt_ptr_t* gdt_idt_ptr);

/* IDT installation */
void ia32_install_idt(gdt_idt_ptr_t* gdt_idt_ptr);

/* LDT installation */
void ia32_install_ldt(uint32_t ldt_sel);

/* TSS installation */
void ia32_install_tss(uint32_t tss_sel);

#if defined(CONFIG_FSGSBASE_GDT) || !defined(CONFIG_FSGSBASE_MSR)

static inline void x86_write_fs_base(word_t base)
{
    gdt_entry_gdt_data_ptr_set_base_low(ARCH_NODE_STATE(x86KSgdt) + GDT_IPCBUF, base);
    gdt_entry_gdt_data_ptr_set_base_mid(ARCH_NODE_STATE(x86KSgdt) + GDT_IPCBUF,  (base >> 16) & 0xFF);
    gdt_entry_gdt_data_ptr_set_base_high(ARCH_NODE_STATE(x86KSgdt) + GDT_IPCBUF, (base >> 24) & 0xFF);
}

static inline void x86_write_gs_base(word_t base)
{
    gdt_entry_gdt_data_ptr_set_base_low(ARCH_NODE_STATE(x86KSgdt) + GDT_TLS, base);
    gdt_entry_gdt_data_ptr_set_base_mid(ARCH_NODE_STATE(x86KSgdt) + GDT_TLS,  (base >> 16) & 0xFF);
    gdt_entry_gdt_data_ptr_set_base_high(ARCH_NODE_STATE(x86KSgdt) + GDT_TLS, (base >> 24) & 0xFF);
}

#endif

void ia32_load_fs(word_t selector);
void ia32_load_gs(word_t selector);

static inline void init_syscall_msrs(void)
{
    fail("syscall not supported on ia32");
}

#endif
