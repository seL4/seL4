/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MODE_MACHINE_CPU_REGISTERS_H
#define __MODE_MACHINE_CPU_REGISTERS_H

static inline unsigned long read_cr3(void)
{
    unsigned long val;
    asm volatile("movl %%cr3, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline void write_cr3(unsigned long val)
{
    asm volatile("movl %0, %%cr3" :: "r"(val), "m"(control_reg_order));
}

static inline unsigned long read_cr0(void)
{
    unsigned long val;
    asm volatile("movl %%cr0, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline void write_cr0(unsigned long val)
{
    asm volatile("movl %0, %%cr0" :: "r"(val), "m"(control_reg_order));
}

static inline unsigned long read_cr2(void)
{
    unsigned long val;
    asm volatile("movl %%cr2, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline unsigned long read_cr4(void)
{
    unsigned long val;
    asm volatile("movl %%cr4, %0" : "=r"(val), "=m"(control_reg_order));
    return val;
}

static inline void write_cr4(unsigned long value)
{
    asm volatile("movl %0, %%cr4" :: "r"(value), "m"(control_reg_order));
}

#endif
