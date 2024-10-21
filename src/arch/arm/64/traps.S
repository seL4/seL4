/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
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

#define ESR     esr_el2
#define SPSR    spsr_el2
#define TPIDR   REGN(tpidr_el2)
#define ELR     REGN(elr_el2)

#else

#define ESR     esr_el1
#define SPSR    spsr_el1
#define TPIDR   REGN(tpidr_el1)
#define ELR     REGN(elr_el1)

#endif

.macro lsp_i _tmp
    mrs     \_tmp, TPIDR
#ifdef CONFIG_ENABLE_SMP_SUPPORT
    bic     \_tmp, \_tmp, #0xfff
#endif
    mov     REGN(sp), \_tmp
.endm

.macro ventry label
.align 7
    b       \label
.endm

.section .vectors, "ax"

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

.section .vectors.text, "ax"

.macro kernel_enter
    /* Storing thread's stack frame */
    stp     REG(0),  REG(1),  [REGN(sp), #(REGSIZE * 2) * 0]
    stp     REG(2),  REG(3),  [REGN(sp), #(REGSIZE * 2) * 1]
    stp     REG(4),  REG(5),  [REGN(sp), #(REGSIZE * 2) * 2]
    stp     REG(6),  REG(7),  [REGN(sp), #(REGSIZE * 2) * 3]
    stp     REG(8),  REG(9),  [REGN(sp), #(REGSIZE * 2) * 4]
    stp     REG(10), REG(11), [REGN(sp), #(REGSIZE * 2) * 5]
    stp     REG(12), REG(13), [REGN(sp), #(REGSIZE * 2) * 6]
    stp     REG(14), REG(15), [REGN(sp), #(REGSIZE * 2) * 7]
    stp     REG(16), REG(17), [REGN(sp), #(REGSIZE * 2) * 8]
    stp     REG(18), REG(19), [REGN(sp), #(REGSIZE * 2) * 9]
    stp     REG(20), REG(21), [REGN(sp), #(REGSIZE * 2) * 10]
    stp     REG(22), REG(23), [REGN(sp), #(REGSIZE * 2) * 11]
    stp     REG(24), REG(25), [REGN(sp), #(REGSIZE * 2) * 12]
    stp     REG(26), REG(27), [REGN(sp), #(REGSIZE * 2) * 13]
    stp     REG(28), REG(29), [REGN(sp), #(REGSIZE * 2) * 14]

    /* Store thread's SPSR, LR, and SP */
    mrs     REG(21), REGN(sp_el0)
    mrs     REG(22), ELR
    mrs     x23, SPSR
    stp     REG(30), REG(21), [REGN(sp), #PT_LR]
    stp     REG(22), REG(23), [REGN(sp), #PT_ELR_EL1]
.endm

BEGIN_FUNC(invalid_vector_entry)
    lsp_i   REG(19)
    b       halt
END_FUNC(invalid_vector_entry)

BEGIN_FUNC(cur_el_sync)
    lsp_i   REG(19)
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
    mrs     REG(0), ELR
    bl      kernelDataAbort
#endif /* CONFIG_DEBUG_BUILD */
    b       halt

cur_el_ia:
#ifdef CONFIG_DEBUG_BUILD
    mrs     REG(0), ELR
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
    lsp_i   REG(19)
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
    mrs     REG(20), ELR
    str     REG(20), [REGN(sp), #PT_FaultIP]

    lsp_i   REG(19)
    /* move the ESR as the input */
    mov     x0, x25
    b       c_handle_vcpu_fault
#else
    cmp     x24, #ESR_EL1_EC_ENFP
    b.eq    el0_enfp
    b       el0_user
#endif

lel_da:
    mrs     REG(20), ELR
    str     REG(20), [REGN(sp), #PT_FaultIP]

    lsp_i   REG(19)
    b       c_handle_data_fault

lel_ia:
    mrs     REG(20), ELR
    str     REG(20), [REGN(sp), #PT_FaultIP]

    lsp_i   REG(19)
    b       c_handle_instruction_fault

lel_syscall:
    mrs     REG(20), ELR
    sub     REG(20), REG(20), #4
    str     REG(20), [REGN(sp), #PT_FaultIP]

    lsp_i   REG(19)

#ifdef CONFIG_FASTPATH
    cmp     x7, #SYSCALL_CALL
    b.eq    c_handle_fastpath_call
#ifdef CONFIG_SIGNAL_FASTPATH
    cmp     x7, #SYSCALL_SEND
    b.eq    c_handle_fastpath_signal
#endif /* CONFIG_SIGNAL_FASTPATH */
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
    lsp_i   REG(19)
    b       c_handle_enfp
#endif /* CONFIG_HAVE_FPU */

el0_user:
    mrs     REG(20), ELR
    str     REG(20), [REGN(sp), #PT_FaultIP]

    lsp_i   REG(19)
    b       c_handle_undefined_instruction
END_FUNC(lower_el_sync)

BEGIN_FUNC(lower_el_irq)
    kernel_enter
    mrs     REG(20), ELR
    str     REG(20), [REGN(sp), #PT_FaultIP]

    lsp_i   REG(19)
    b       c_handle_interrupt
END_FUNC(lower_el_irq)

BEGIN_FUNC(lower_el_serr)
#ifdef CONFIG_AARCH64_SERROR_IGNORE
    eret
#else
    b       invalid_vector_entry
#endif
END_FUNC(lower_el_serr)
