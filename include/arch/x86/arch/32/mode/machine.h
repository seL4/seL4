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

#include <arch/model/statedata.h>
#include <arch/machine/cpu_registers.h>

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

#endif
