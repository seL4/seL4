/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <util.h>

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <machine/assembler.h>

.code 32
.section .vectors, "ax"

BEGIN_FUNC(arm_vector_table)
    ldr pc, =arm_reset_exception
    ldr pc, =arm_undefined_inst_exception
    ldr pc, =arm_swi_syscall
    ldr pc, =arm_prefetch_abort_exception
    ldr pc, =arm_data_abort_exception
    ldr pc, =arm_reset_exception
    ldr pc, =arm_irq_exception
    ldr pc, =arm_fiq_exception

.ltorg
END_FUNC(arm_vector_table)

.section .vectors.text, "ax"

#include <arch/api/syscall.h>
#include <arch/machine/hardware.h>

#include <arch/machine/registerset.h>
#include <sel4/sel4_arch/constants.h>

BEGIN_FUNC(arm_undefined_inst_exception)
    /* Full save/restore, documented in arm_swi_syscall */
    srsia #PMODE_SUPERVISOR
    cps #PMODE_SUPERVISOR
    stmdb sp, {r0-lr}^
    ldr r8, [sp]
    sub r8, r8, #4
    str r8, [sp, #(PT_FaultIP - PT_NextIP)]
    mrc p15, 0, sp, c13, c0, 4
    b c_handle_undefined_instruction
END_FUNC(arm_undefined_inst_exception)

BEGIN_FUNC(arm_swi_syscall)
    /* Store CPSR and NextIP on supervisor stack, which currently points
       at the end of the current thread's user context */
    srsia #PMODE_SUPERVISOR

    /* Set the FaultIP address, which in ARM mode is the NextIP - 4.
     * NOTE: This is completely wrong and broken in thumb mode.
     */
    sub lr, lr, #4

    /* Store FaultIP */
    str lr, [sp, #(PT_FaultIP - PT_NextIP)]

    /* Stack all user registers */
    stmdb sp, {r0-lr}^

    /* Load the kernel's real stack pointer */
    mrc p15, 0, sp, c13, c0, 4

    /* Load system call number as a c_handle_syscall argument. r0 and r1 are passed
     * unmodified (cptr and msgInfo) respectively.  On MCS configurations we also
     * pass the reply cptr in r2 for fastpath_reply_recv.
     */

#ifdef CONFIG_FASTPATH
    cmp r7, #SYSCALL_CALL
    beq c_handle_fastpath_call
    cmp r7, #SYSCALL_REPLY_RECV
#ifdef CONFIG_KERNEL_MCS
    moveq r2, r6
#endif
    beq c_handle_fastpath_reply_recv
#endif

    mov r2, r7
    b c_handle_syscall

END_FUNC(arm_swi_syscall)

BEGIN_FUNC(arm_prefetch_abort_exception)
    /* Full save/restore, documented in arm_swi_syscall */
    srsia #PMODE_SUPERVISOR
    cps #PMODE_SUPERVISOR
    stmdb sp, {r0-lr}^

    /* Load PC and SPSR saved by the "srs" instruction above. */
    ldmia   sp, {r8,r9}

    /* Ensure the bottom 4 bits of SPSR are zero, indicating we came from
     * userspace. If not, something has gone amiss in the kernel. */
    tst     r9, #0xf

    /* Compute the faulting address. */
    sub r8, r8, #4

    bne     kernel_prefetch_fault

    /* Store faulting address in TCB and call handleVMFaultEvent. */
    str r8, [sp, #(PT_FaultIP - PT_NextIP)]

    mrc p15, 0, sp, c13, c0, 4

    b c_handle_instruction_fault

kernel_prefetch_fault:
#ifdef CONFIG_DEBUG_BUILD
    mov r0, r8
    mrc p15, 0, sp, c13, c0, 4
    blx kernelPrefetchAbort
    /* Fallthrough to infinite loop should we foolishly return. */
#endif
    /* To aid finding faults in non-debug mode, catch kernel faults here.
     * - r8 will contain the faulting address.
     * - r9 will contain the IFSR register.
     * - lr might contain something useful too if we followed a function
     *   call.
     * - the original values of r8 and r9 will be obliterated.
     */
    mrc p15, 0, r9, c5, c0, 1    /* Get ISFR. */
1:  b 1b /* Infinite loop. You'd better have a watchdog. */
END_FUNC(arm_prefetch_abort_exception)

BEGIN_FUNC(arm_data_abort_exception)
    /* Full save/restore, documented in arm_swi_syscall */
    srsia #PMODE_SUPERVISOR
    cps #PMODE_SUPERVISOR
    stmdb sp, {r0-lr}^

    /* Load PC and SPSR saved by the "srs" instruction above. */
    ldmia   sp, {r8,r9}

    /* Ensure the bottom 4 bits of SPSR are zero, indicating we came from
     * userspace. If not, something has gone amiss in the kernel. */
    tst     r9, #0xf

    /* Compute the faulting address.
     * For a Data abort, LR_abt points at PC+8. */
    sub r8, r8, #8

    bne     kernel_data_fault

    /* Store faulting address in TCB and call handleVMFaultEvent. */
    str r8, [sp, #(PT_FaultIP - PT_NextIP)]
    mrc p15, 0, sp, c13, c0, 4

    b c_handle_data_fault


kernel_data_fault:
#ifdef CONFIG_DEBUG_BUILD
    mov r0, r8
    mrc p15, 0, sp, c13, c0, 4
    blx kernelDataAbort
    /* Fallthrough to infinite loop should we foolishly return. */
#endif
    /* To aid finding faults in non-debug mode, catch kernel faults here.
     * - r8 will contain the faulting instruction.
     * - r9 will contain the memory address that faulted.
     * - r10 will contain the fault status register (DFSR).
     * - the original values of r8, r9 and r10 will be obliterated.
     */
    mrc p15, 0, r9, c5, c0, 0    /* Get data fault status register. */
    mrc p15, 0, r10, c6, c0, 0   /* Get fault address register. */
1:  b 1b /* Infinite loop. You'd better have a watchdog. */
END_FUNC(arm_data_abort_exception)

BEGIN_FUNC(arm_irq_exception)
    /* Full save/restore, documented in arm_swi_syscall */
    srsia #PMODE_SUPERVISOR
    cps #PMODE_SUPERVISOR
    stmdb sp, {r0-lr}^
    ldr r8, [sp]
    sub r8, r8, #4
    str r8, [sp]
    str r8, [sp, #(PT_FaultIP - PT_NextIP)]
    mrc p15, 0, sp, c13, c0, 4
    b c_handle_interrupt
END_FUNC(arm_irq_exception)

BEGIN_FUNC(arm_reset_exception)
    blx halt
END_FUNC(arm_reset_exception)

BEGIN_FUNC(arm_fiq_exception)
    blx halt
END_FUNC(arm_fiq_exception)

#endif /* !CONFIG_ARM_HYP */
