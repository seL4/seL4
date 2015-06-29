/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_64_H
#define __ARCH_MACHINE_64_H

#define wordRadix 6
#define wordBits (1 << wordRadix)

/* Address space control */
static inline cr3_t getCurrentCR3(void)
{
    return x64CurrentCR3;
}

static inline paddr_t getCurrentVSpaceRoot(void)
{
    return cr3_get_pml4_base_address(getCurrentCR3());
}

static inline void setCurrentCR3(cr3_t cr3)
{
    x64CurrentCR3 = cr3;
    write_cr3(cr3.words[0]);
}

static inline void setCurrentVSpaceRoot(paddr_t addr, word_t pcid)
{
    setCurrentCR3(cr3_new(addr, pcid));
}

/* GDT installation */
void x64_install_gdt(gdt_idt_ptr_t* gdt_idt_ptr);

/* IDT installation */
void x64_install_idt(gdt_idt_ptr_t* gdt_idt_ptr);

/* LDT installation */
void x64_install_ldt(uint32_t ldt_sel);

/* TSS installation */
void x64_install_tss(uint32_t tss_sel);

/* Get current stack pointer */
static inline void* get_current_esp(void)
{
    word_t stack;
    void *result;
    asm volatile("movq %[stack_address], %[result]" : [result] "=r"(result) : [stack_address] "r"(&stack));
    return result;
}

typedef struct invpcid_desc {
    uint64_t    asid;
    uint64_t    addr;
} invpcid_desc_t;

#define INVPCID_TYPE_ADDR           0
#define INVPCID_TYPE_SINGLE         1
#define INVPCID_TYPE_ALL_GLOBAL     2   /* also invalidate global */
#define INVPCID_TYPE_ALL            3

static inline void invalidatePCID(word_t type, void *vaddr, asid_t asid)
{
    invpcid_desc_t desc;
    desc.asid = asid & 0xfff; 
    desc.addr = (uint64_t)vaddr;
    asm volatile ("invpcid %1, %0" :: "r"(type), "m"(desc));
}

#endif
