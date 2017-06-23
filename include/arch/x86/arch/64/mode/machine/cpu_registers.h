/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODE_MACHINE_CPU_REGISTERS_H_
#define __ARCH_MODE_MACHINE_CPU_REGISTERS_H_

static inline unsigned long read_cr3(void)
{
    word_t cr3;
    asm volatile ("movq %%cr3, %0" : "=r"(cr3), "=m"(control_reg_order));
    return cr3;
}

static inline void write_cr3(unsigned long val)
{
    asm volatile("movq %0, %%cr3" :: "r"(val), "m"(control_reg_order));
}

static inline unsigned long read_cr0(void)
{
    unsigned long val;
    asm volatile("movq %%cr0, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline void write_cr0(unsigned long val)
{
    asm volatile("movq %0, %%cr0" :: "r"(val), "m"(control_reg_order));
}

static inline unsigned long read_cr2(void)
{
    unsigned long val;
    asm volatile("movq %%cr2, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline unsigned long read_cr4(void)
{
    unsigned long val;
    asm volatile("movq %%cr4, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline void write_cr4(unsigned long val)
{
    asm volatile("movq %0, %%cr4" :: "r"(val), "m"(control_reg_order));
}

#endif /* __ARCH_MODE_MACHINE_CPU_REGISTERS_H_ */
