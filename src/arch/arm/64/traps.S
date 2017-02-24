/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <config.h>
#include <machine/assembler.h>
#include <arch/api/syscall.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>

#define VM_EVENT_DATA_ABORT 0
#define VM_EVENT_PREFETCH_ABORT 1
 
.macro lsp_i _tmp
    mrs     \_tmp, tpidr_el1
    mov     sp, \_tmp
.endm

.macro ventry label
.align 7
    b       \label
.endm

.section .vectors

BEGIN_FUNC(arm_vector_table)
    ventry  invalid_vector_entry           // Synchronous EL1t
    ventry  invalid_vector_entry           // IRQ EL1t
    ventry  invalid_vector_entry           // FIQ EL1t
    ventry  invalid_vector_entry           // SError EL1t

    ventry  el1_sync                       // Synchronous EL1h
    ventry  el1_irq                        // IRQ EL1h
    ventry  invalid_vector_entry           // FIQ EL1h
    ventry  invalid_vector_entry           // SError EL1h

    ventry  el0_sync                       // Synchronous 64-bit EL0
    ventry  el0_irq                        // IRQ 64-bit EL0
    ventry  invalid_vector_entry           // FIQ 64-bit EL0
    ventry  invalid_vector_entry           // SError 64-bit EL0

    ventry  invalid_vector_entry           // Synchronous 32-bit EL0
    ventry  invalid_vector_entry           // IRQ 32-bit EL0
    ventry  invalid_vector_entry           // FIQ 32-bit EL0
    ventry  invalid_vector_entry           // SError 32-bit EL0
END_FUNC(arm_vector_table)

.section .vectors.text

.macro kernel_enter
    /* Storing thread's stack frame */
    stp     x0,  x1,  [sp, #16 * 0]
    stp     x2,  x3,  [sp, #16 * 1]
    stp     x4,  x5,  [sp, #16 * 2]
    stp     x6,  x7,  [sp, #16 * 3]
    stp     x8,  x9,  [sp, #16 * 4]
    stp     x10, x11, [sp, #16 * 5]
    stp     x12, x13, [sp, #16 * 6]
    stp     x14, x15, [sp, #16 * 7]
    stp     x16, x17, [sp, #16 * 8]
    stp     x18, x19, [sp, #16 * 9]
    stp     x20, x21, [sp, #16 * 10]
    stp     x22, x23, [sp, #16 * 11]
    stp     x24, x25, [sp, #16 * 12]
    stp     x26, x27, [sp, #16 * 13]
    stp     x28, x29, [sp, #16 * 14]

    /* Store thread's SPSR, LR, and SP */
    mrs     x21, sp_el0
    mrs     x22, elr_el1
    mrs     x23, spsr_el1
    stp     x30, x21, [sp, #PT_LR]
    stp     x22, x23, [sp, #PT_ELR_EL1]
.endm

BEGIN_FUNC(invalid_vector_entry)
    b       halt
END_FUNC(invalid_vector_entry)

BEGIN_FUNC(el1_sync)
    /* Read esr_el1 and branch to respective labels*/
    mrs     x25, esr_el1
    lsr     x24, x25, #ESR_EL1_EC_SHIFT
    cmp     x24, #ESR_EL1_EC_DABT_EL1
    b.eq    el1_da
    cmp     x24, #ESR_EL1_EC_IABT_EL1
    b.eq    el1_ia
    b       el1_inv

el1_da:
#ifdef CONFIG_DEBUG_BUILD
    mrs     x0, elr_el1
    lsp_i   x19
    bl      kernelDataAbort
#endif /* CONFIG_DEBUG_BUILD */
    b       halt

el1_ia:
#ifdef CONFIG_DEBUG_BUILD
    mrs     x0, elr_el1
    lsp_i   x19
    bl      kernelPrefetchAbort
#endif /* CONFIG_DEBUG_BUILD */
    b       halt

el1_inv:
    b       invalid_vector_entry
END_FUNC(el1_sync)

/*
 * This is only called if ksCurThread is idle thread.
 *
 * No need to store the state of idle thread and simply call c_handle_interrupt to
 * activate ksCurThread when returning from interrupt as long as idle thread is stateless.
 */
BEGIN_FUNC(el1_irq)
    lsp_i   x19
    b       c_handle_interrupt
END_FUNC(el1_irq)

BEGIN_FUNC(el0_sync)
    kernel_enter

    /* Read esr_el1 and branch to respective labels*/
    mrs     x25, esr_el1
    lsr     x24, x25, #ESR_EL1_EC_SHIFT
    cmp     x24, #ESR_EL1_EC_DABT_EL0
    b.eq    el0_da
    cmp     x24, #ESR_EL1_EC_IABT_EL0
    b.eq    el0_ia
    cmp     x24, #ESR_EL1_EC_SVC64
    b.eq    el0_svc
    cmp     x24, #ESR_EL1_EC_ENFP
    b.eq    el0_enfp
    b       el0_user

el0_da:
    mrs     x20, elr_el1
    str     x20, [sp, #PT_FaultInstruction]

    lsp_i   x19
    b       c_handle_data_fault

el0_ia:
    mrs     x20, elr_el1
    str     x20, [sp, #PT_FaultInstruction]

    lsp_i   x19
    b       c_handle_instruction_fault

el0_svc:
    mrs     x20, elr_el1
    sub     x20, x20, #4
    str     x20, [sp, #PT_FaultInstruction]

    lsp_i   x19
    mov     x2, x7
    b       c_handle_syscall

el0_enfp:
#ifdef CONFIG_HAVE_FPU
    lsp_i   x19
    b       c_handle_enfp
#endif /* CONFIG_HAVE_FPU */

el0_user:
    mrs     x20, elr_el1
    str     x20, [sp, #PT_FaultInstruction]

    lsp_i   x19
    b       c_handle_undefined_instruction
END_FUNC(el0_sync)

BEGIN_FUNC(el0_irq)
    kernel_enter

    mrs     x20, elr_el1
    str     x20, [sp, #PT_FaultInstruction]

    lsp_i   x19
    b       c_handle_interrupt
END_FUNC(el0_irq)
