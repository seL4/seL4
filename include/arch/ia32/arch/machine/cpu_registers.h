/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_CPU_REGISTERS_H
#define __ARCH_MACHINE_CPU_REGISTERS_H

#define CR0_MONITOR_COPROC  BIT(1)  /* Trap on FPU "WAIT" commands. */
#define CR0_EMULATION       BIT(2)  /* Enable OS emulation of FPU. */
#define CR0_TASK_SWITCH     BIT(3)  /* Trap on any FPU usage, for lazy FPU. */
#define CR0_NUMERIC_ERROR   BIT(5)  /* Internally handle FPU problems. */
#define CR4_OSFXSR          BIT(9)  /* Enable SSE et. al. features. */
#define CR4_OSXMMEXCPT      BIT(10) /* Enable SSE exceptions. */

/* We use a dummy variable to synchronize reads and writes to the control registers.
 * this allows us to write inline asm blocks that do not have enforced memory
 * clobbers for ordering. */
static uint32_t __control_reg_order;

static inline uint32_t read_cr3(void)
{
    uint32_t val;
    asm volatile("movl %%cr3, %0" : "=r"(val), "=m"(__control_reg_order));
    return val;
}

static inline void write_cr3(uint32_t val)
{
    asm volatile("movl %0, %%cr3" :: "r"(val), "m"(__control_reg_order));
}

static inline uint32_t read_cr0(void)
{
    uint32_t val;
    asm volatile("movl %%cr0, %0" : "=r"(val), "=m"(__control_reg_order));
    return val;
}

static inline void write_cr0(uint32_t val)
{
    asm volatile("movl %0, %%cr0" :: "r"(val), "m"(__control_reg_order));
}

uint32_t read_cr4(void);
void write_cr4(uint32_t value);

#endif
