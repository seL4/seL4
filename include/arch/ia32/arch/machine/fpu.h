/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_FPU_H
#define __ARCH_KERNEL_FPU_H

#include <arch/machine.h>
#include <api/failures.h>
#include <types.h>

/* Initialise the FPU. */
void Arch_initFpu(void);

/* Initialise the FPU state of the given user context. */
void Arch_initFpuContext(user_context_t *context);

/* Perform any actions required for the deletion of the given thread. */
void Arch_fpuThreadDelete(tcb_t *thread);

/* Handle an FPU exception. */
exception_t handleUnimplementedDevice(void);

/* Store state in the FPU registers into memory. */
void saveFpuState(user_fpu_state_t *dest);

/* Load FPU state from memory into the FPU registers. */
void loadFpuState(user_fpu_state_t *src);

/* Reset the FPU registers into their initial blank state. */
void resetFpu(void);

/*
 * Enable the FPU to be used without faulting.
 * Required even if the kernel attempts to use the FPU.
 */
static inline void enableFpu(void)
{
    asm volatile("clts" :: "m" (__control_reg_order));
}

/*
 * Disable the FPU so that usage of it causes a fault
 */
static inline void disableFpu(void)
{
    write_cr0(read_cr0() | CR0_TASK_SWITCH);
}

#endif
