/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_ARM_TRAPS_H
#define __KERNEL_ARM_TRAPS_H

#include <config.h>
#include <machine.h>
#include <util.h>

static inline void arch_c_entry_hook(void)
{
    arm_save_thread_id(NODE_STATE(ksCurThread));
}

static inline void arch_c_exit_hook(void)
{
    arm_load_thread_id(NODE_STATE(ksCurThread));
}

void VISIBLE NORETURN restore_user_context(void);

#ifdef CONFIG_KERNEL_MCS
void c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall, word_t reply)
#else
void c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
#endif
VISIBLE SECTION(".vectors.text");

void c_handle_interrupt(void)
VISIBLE SECTION(".vectors.text");

void c_handle_undefined_instruction(void)
VISIBLE SECTION(".vectors.text");

void c_handle_data_fault(void)
VISIBLE SECTION(".vectors.text");

void c_handle_instruction_fault(void)
VISIBLE SECTION(".vectors.text");

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
void c_handle_vcpu_fault(word_t hsr)
VISIBLE SECTION(".vectors.text");
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#ifdef CONFIG_HAVE_FPU
void c_handle_enfp(void)
VISIBLE SECTION(".vectors.text");
#endif /* CONFIG_HAVE_FPU */

#endif /* __KERNEL_ARM_TRAPS_H */
