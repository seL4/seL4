/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
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

#define VM_EVENT_DATA_ABORT 0
#define VM_EVENT_PREFETCH_ABORT 1

/* Helper to return to userland. */
.macro RET_TO_USER, cur_thread_reg
    /* cur_thread_reg should contain the address of ksCurThread. */
    /* Set stack pointer to point at the LR_svc of the user context. */
    ldr sp, [\cur_thread_reg]
    add sp, sp, #PT_LR_svc

    /* Unstack user registers */
    ldmdb sp, {r0-lr}^

    /* Jump to user NextPC and restore user CPSR */
    rfeia sp
.endm

BEGIN_FUNC(arm_undefined_inst_exception)
    /* Full save/restore, documented in arm_swi_syscall */
    srsia #PMODE_SUPERVISOR
    cps #PMODE_SUPERVISOR
#if defined(CONFIG_ARCH_ARM_V6) && defined(CONFIG_DANGEROUS_CODE_INJECTION_ON_UNDEF_INSTR)
    /* Call whatever's in r8. See Kconfig for the purpose of this. */
    blx r8
    rfeia sp
#else
    stmdb sp, {r0-lr}^
    ldr r8, [sp]
    sub r8, r8, #4
    str r8, [sp, #(PT_FaultInstruction - PT_LR_svc)]
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    ldr r7, =ksCurThread

    /* There's only one user-level fault on ARM, and the code is (0,0) */
    mov r0, #0
    mov r1, #0
    blx handleUserLevelFault

    RET_TO_USER r7
#endif
END_FUNC(arm_undefined_inst_exception)

BEGIN_FUNC(arm_swi_syscall)
    /* Store CPSR and LR_svc on supervisor stack, which currently points
       at the end of the current thread's user context */
    srsia #PMODE_SUPERVISOR

    /* Set the FaultInstruction address, which in ARM mode is the LR_svc - 4.
     * NOTE: This is completely wrong and broken in thumb mode.
     */
    sub lr, lr, #4

    /* Store FaultInstruction */
    str lr, [sp, #(PT_FaultInstruction - PT_LR_svc)]

#ifdef FASTPATH
    cmp r7, #SYSCALL_REPLY_WAIT
#endif

    /* Stack all user registers */
    stmdb sp, {r0-lr}^

    /* Load the kernel's real stack pointer */
    mov sp, #(PPTR_KERNEL_STACK_TOP)

#ifdef FASTPATH
    /*
     * Call is -1 == 0xffffffff.
     * We compared against -2 = 0xfffffffe above.
     * Performing an unsigned higher than, there is only one unsigned number
     * greater than -2.
     */
    bhi fastpath_call
    beq fastpath_reply_wait
#endif

    /* Load system call number for handleSyscall and handleUnknownSyscall() */
    mov r0, r7

    /*
     * RET_TO_USER needs ksCurThread. We can issue the load here where we have
     * some spare cycles, and the ARM ABI will preserve it across function
     * calls.
     */
    ldr r8, =ksCurThread

    /* Check that syscall number is in range */
    sub r2, r0, #SYSCALL_MIN
    cmp r2, #(SYSCALL_MAX - SYSCALL_MIN + 1)
    bhs arm_swi_undefined_syscall

    blx handleSyscall

    /* Return to user. */
    RET_TO_USER r8
END_FUNC(arm_swi_syscall)

BEGIN_FUNC(arm_swi_undefined_syscall)
    blx handleUnknownSyscall
    RET_TO_USER r8
END_FUNC(arm_swi_undefined_syscall)

#ifdef FASTPATH
BEGIN_FUNC(slowpath)
    /*
     * We enter here only via the fastpath.
     * r0 contains the system call number, and all other registers are
     * trashed.
     */
    ldr r7, =ksCurThread
    mov sp, #(PPTR_KERNEL_STACK_TOP)
    blx handleSyscall
    RET_TO_USER r7
END_FUNC(slowpath)

BEGIN_FUNC(fastpath_restore)
    /* Duplicate of above except r2 contains ksCurThread,
       r0 and r1 should be preserved */
    mov sp, r2
    add sp, sp, #PT_LR_svc

    ldmdb sp, {r2-lr}^

    rfeia sp
END_FUNC(fastpath_restore)
#endif /* FASTPATH */

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
    str r8, [sp, #(PT_FaultInstruction - PT_LR_svc)]
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    ldr r7, =ksCurThread
    mov r0, #VM_EVENT_PREFETCH_ABORT
    blx handleVMFaultEvent

    RET_TO_USER r7

kernel_prefetch_fault:
#ifdef DEBUG
    mov r0, r8
    mov sp, #(PPTR_KERNEL_STACK_TOP)
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
    str r8, [sp, #(PT_FaultInstruction - PT_LR_svc)]
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    ldr r7, =ksCurThread
    mov r0, #VM_EVENT_DATA_ABORT
    blx handleVMFaultEvent

    RET_TO_USER r7

kernel_data_fault:
#ifdef DEBUG
    mov r0, r8
    mov sp, #(PPTR_KERNEL_STACK_TOP)
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
    str r8, [sp, #(PT_FaultInstruction - PT_LR_svc)]

    ldr r7, =ksCurThread
    mov sp, #(PPTR_KERNEL_STACK_TOP)
    blx handleInterruptEntry

    RET_TO_USER r7
END_FUNC(arm_irq_exception)

BEGIN_FUNC(arm_reset_exception)
    blx halt
END_FUNC(arm_reset_exception)

BEGIN_FUNC(arm_fiq_exception)
    blx halt
END_FUNC(arm_fiq_exception)
