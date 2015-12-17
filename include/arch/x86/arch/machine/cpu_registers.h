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
#define CR4_VMXE            BIT(13) /* Enable VMX mode. */

/* We use a dummy variable to synchronize reads and writes to the control registers.
 * this allows us to write inline asm blocks that do not have enforced memory
 * clobbers for ordering. */
static unsigned long __control_reg_order;

#include <mode/machine/cpu_registers.h>

#endif
