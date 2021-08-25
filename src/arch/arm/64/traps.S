/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>
#include <arch/api/syscall.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>

#define VM_EVENT_DATA_ABORT 0
#define VM_EVENT_PREFETCH_ABORT 1

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#define ELR     elr_el2
#define ESR     esr_el2
#define SPSR    spsr_el2
#define TPIDR   tpidr_el2

#else

#define ELR     elr_el1
#define ESR     esr_el1
#define SPSR    spsr_el1
#define TPIDR   tpidr_el1

#endif


.macro lsp_i _tmp
    mrs     \_tmp, TPIDR
#if CONFIG_MAX_NUM_NODES > 1
    bic     \_tmp, \_tmp, #0xfff
#endif
    mov     sp, \_tmp
.endm

.macro ventry label
.align 7
    b       \label
.endm

.section .vectors

BEGIN_FUNC(arm_vector_table)
    ventry  invalid_vector_entry           // Synchronous EL1t/EL2t
    ventry  invalid_vector_entry           // IRQ EL1t/EL2t
    ventry  invalid_vector_entry           // FIQ EL1t/EL2t
    ventry  invalid_vector_entry           // SError EL1t/EL2t

    ventry  cur_el_sync                    // Current EL Synchronous (EL1/2)
    ventry  cur_el_irq                     // IRQ
    ventry  invalid_vector_entry           // FIQ
    ventry  cur_el_serr                    // SError

    ventry  lower_el_sync                  // Synchronous 64-bit EL0/EL1
    ventry  lower_el_irq                   // IRQ 64-bit EL0/EL1
    ventry  invalid_vector_entry           // FIQ 64-bit EL0/EL1
    ventry  lower_el_serr                  // SError 64-bit EL0/EL1

    ventry  invalid_vector_entry           // Synchronous 32-bit EL0/EL1
    ventry  invalid_vector_entry           // IRQ 32-bit EL0/EL1
    ventry  invalid_vector_entry           // FIQ 32-bit EL0/EL1
    ventry  invalid_vector_entry           // SError 32-bit EL0/EL1
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
    mrs     x22, ELR
    mrs     x23, SPSR
    stp     x30, x21, [sp, #PT_LR]
    stp     x22, x23, [sp, #PT_ELR_EL1]
.endm

BEGIN_FUNC(invalid_vector_entry)
    lsp_i   x19
    b       halt
END_FUNC(invalid_vector_entry)

BEGIN_FUNC(cur_el_sync)
    lsp_i   x19
    /* Read esr and branch to respective labels */
    mrs     x25, ESR
    lsr     x24, x25, #ESR_EC_SHIFT
    cmp     x24, #ESR_EC_CEL_DABT
    b.eq    cur_el_da
    cmp     x24, #ESR_EC_CEL_IABT
    b.eq    cur_el_ia
    b       cur_el_inv

cur_el_da:
#ifdef CONFIG_DEBUG_BUILD
    mrs     x0, ELR
    bl      kernelDataAbort
#endif /* CONFIG_DEBUG_BUILD */
    b       halt

cur_el_ia:
#ifdef CONFIG_DEBUG_BUILD
    mrs     x0, ELR
    bl      kernelPrefetchAbort
#endif /* CONFIG_DEBUG_BUILD */
    b       halt

cur_el_inv:
    b       invalid_vector_entry
END_FUNC(cur_el_sync)

/*
 * This is only called if ksCurThread is idle thread.
 *
 * No need to store the state of idle thread and simply call c_handle_interrupt to
 * activate ksCurThread when returning from interrupt as long as idle thread is stateless.
 */
BEGIN_FUNC(cur_el_irq)
    lsp_i   x19
    b       c_handle_interrupt
END_FUNC(cur_el_irq)

BEGIN_FUNC(cur_el_serr)
#ifdef CONFIG_AARCH64_SERROR_IGNORE
    eret
#else
    b       invalid_vector_entry
#endif
END_FUNC(cur_el_serr)

BEGIN_FUNC(lower_el_sync)
    kernel_enter

    /* Read esr and branch to respective labels */
    mrs     x25, ESR
    lsr     x24, x25, #ESR_EC_SHIFT
    cmp     x24, #ESR_EC_LEL_DABT
    b.eq    lel_da
    cmp     x24, #ESR_EC_LEL_IABT
    b.eq    lel_ia
    cmp     x24, #ESR_EC_LEL_SVC64
    b.eq    lel_syscall
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    cmp     x24, #ESR_EC_LEL_HVC64
    b.eq    lel_syscall
    mrs     x20, ELR
    str     x20, [sp, #PT_FaultIP]

    lsp_i   x19
    /* move the ESR as the input */
    mov     x0, x25
    b       c_handle_vcpu_fault
#else
    cmp     x24, #ESR_EL1_EC_ENFP
    b.eq    el0_enfp
    b       el0_user
#endif

lel_da:
    mrs     x20, ELR
    str     x20, [sp, #PT_FaultIP]

    lsp_i   x19
    b       c_handle_data_fault

lel_ia:
    mrs     x20, ELR
    str     x20, [sp, #PT_FaultIP]

    lsp_i   x19
    b       c_handle_instruction_fault

lel_syscall:
    mrs     x20, ELR
    sub     x20, x20, #4
    str     x20, [sp, #PT_FaultIP]

    lsp_i   x19

#ifdef CONFIG_FASTPATH
    cmp     x7, #SYSCALL_CALL
    b.eq    c_handle_fastpath_call
    cmp     x7, #SYSCALL_REPLY_RECV
#ifdef CONFIG_KERNEL_MCS
    mov     x2, x6
#endif
    b.eq    c_handle_fastpath_reply_recv
#endif

    mov     x2, x7
    b       c_handle_syscall

el0_enfp:
#ifdef CONFIG_HAVE_FPU
    lsp_i   x19
    b       c_handle_enfp
#endif /* CONFIG_HAVE_FPU */

el0_user:
    mrs     x20, ELR
    str     x20, [sp, #PT_FaultIP]

    lsp_i   x19
    b       c_handle_undefined_instruction
END_FUNC(lower_el_sync)

BEGIN_FUNC(lower_el_irq)
    kernel_enter
    mrs     x20, ELR
    str     x20, [sp, #PT_FaultIP]

    lsp_i   x19
    b       c_handle_interrupt
END_FUNC(lower_el_irq)

BEGIN_FUNC(lower_el_serr)
#ifdef CONFIG_AARCH64_SERROR_IGNORE
    eret
#else
    b       invalid_vector_entry
#endif
END_FUNC(lower_el_serr)
