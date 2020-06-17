/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <machine/assembler.h>
#include <sel4/sel4_arch/constants.h>

/*** Hypervisor coprocessor registers ***/
#define   HVBAR(reg)    p15, 4, reg, c12, c0, 0
#define     HCR(reg)    p15, 4, reg, c1 , c1, 0
#define  HSCTLR(reg)    p15, 4, reg, c1 , c0, 0
#define  HACTLR(reg)    p15, 4, reg, c1 , c0, 1
#define    HDCR(reg)    p15, 4, reg, c1 , c1, 1
#define   HCPTR(reg)    p15, 4, reg, c1 , c1, 2
#define    HSTR(reg)    p15, 4, reg, c1 , c1, 3
#define    HACR(reg)    p15, 4, reg, c1 , c1, 7
#define    HTCR(reg)    p15, 4, reg, c2 , c0, 2
#define  HADFSR(reg)    p15, 4, reg, c5 , c1, 0
#define  HAIFSR(reg)    p15, 4, reg, c5 , c1, 1
#define     HSR(reg)    p15, 4, reg, c5 , c2, 0
#define   HDFAR(reg)    p15, 4, reg, c6 , c0, 0
#define   HIFAR(reg)    p15, 4, reg, c6 , c0, 2
#define   HPFAR(reg)    p15, 4, reg, c6 , c0, 4
#define  HMAIR0(reg)    p15, 4, reg, c10, c2, 0
#define  HMAIR1(reg)    p15, 4, reg, c10, c2, 1
#define HAMAIR0(reg)    p15, 4, reg, c10, c3, 0
#define HAMAIR1(reg)    p15, 4, reg, c10, c3, 1
#define  HTPIDR(reg)    p15, 4, reg, c13, c0, 2
#define   HTTBR(rh,rl)  p15, 4, rl, rh, c2

/*** VMM coprocessor registers ***/
#define    VTCR(reg)    p15, 4, reg, c2 , c1, 2
#define   VTTBR(rh,rl)  p15, 6, rl, rh, c2


#define  DTLBIALL(reg) p15, 0, reg, c8, c6, 0
#define   TLBIALL(reg) p15, 0, reg, c8, c7, 0
#define DTLBIASID(reg) p15, 0, reg, c8, c6, 2
#define  TLBIASID(reg) p15, 0, reg, c8, c7, 2

/*** Hyp mode TLB maintenance ***/
/* Invalidate entire Hyp unified TLB Inner Shareable */
#define TLBIALLHIS()     p15, 4,  r0, c8, c7, 0
/* Invalidate Hyp unified TLB entry by MVA Inner Shareable MVA */
#define TLBIMVAHIS(mva)  p15, 4, mva, c8, c7, 0
/* Invalidate entire Non-secure Non-Hyp unified TLB Inner Shareable */
#define TLBIALLNSNHIS()  p15, 4,  r0, c8, c7, 0
/* Invalidate entire Hyp unified TLB */
#define TLBIALLH()       p15, 4,  r0, c8, c7, 0
/* Invalidate Hyp unified TLB entry by MVA */
#define TLBIMVAH(mva)    p15, 4, mva, c8, c7, 0
/* Invalidate entire Non-secure Non-Hyp unified TLB  */
#define TLBIALLNSNH()    p15, 4,  r0, c8, c7, 0

.code 32
.section .vectors, "ax"
BEGIN_FUNC(arm_vector_table)
    ldr pc, =arm_hyp_reset_exception
    ldr pc, =arm_hyp_undefined_inst_exception
    ldr pc, =arm_hyp_syscall
    ldr pc, =arm_hyp_prefetch_abort_exception
    ldr pc, =arm_hyp_data_abort_exception
    ldr pc, =arm_hyp_trap
    ldr pc, =arm_hyp_irq_exception
    ldr pc, =arm_hyp_fiq_exception
END_FUNC(arm_vector_table)

.ltorg

.section .vectors.text, "ax"

#include <arch/api/syscall.h>
#include <arch/machine/hardware.h>

#include <arch/machine/registerset.h>

#define HSREC_SHIFT          26
#define HSREC_MASK           (0x3f << HSREC_SHIFT)
#define HSREC_UNKNOWN        0x00
#define HSREC_WFI            0x01
#define HSREC_SVC            0x11
#define HSREC_HVC            0x12
#define HSREC_SMC            0x13
#define HSREC_PREFETCH_ABORT 0x20
#define HSREC_DATA_ABORT     0x24
#define HSRIL32              (1 << 25)

.macro EX_ENTRY
    /* Create some scratch space */
    push {lr}
    /* Store ELR */
    mrs lr, elr_hyp
    str lr, [sp, #4]
    /* Store SPSR */
    mrs lr, spsr_hyp
    str lr, [sp, #8]
    /* Store SP_usr */
    mrs lr, sp_usr
    push {lr}
.endm

/* Prepare to enter the kernel */
.macro PUSH_STACK
    /* store FaultIP */
    str lr, [sp, #(PT_FaultIP - PT_SP)]
    /* Stack all user registers */
    push {r0-r12}
    /* Load the kernel's real stack pointer */
    /* the hyp mode uses HTPIDR */
    mrc p15, 4, sp, c13, c0, 2
.endm

/********************************
 ***  Traps taken to HYP mode ***
 ********************************/

BEGIN_FUNC(arm_hyp_trap)
    EX_ENTRY

    /* ARM_ARM B3.13.6 */
    mrc HSR(lr)
    and lr, lr, #(HSREC_MASK)
    cmp lr, #(HSREC_SVC << HSREC_SHIFT)
    beq arm_syscall
    cmp lr, #(HSREC_HVC << HSREC_SHIFT)
    beq arm_syscall
    cmp lr, #(HSREC_PREFETCH_ABORT << HSREC_SHIFT)
    beq arm_prefetch_abort
    cmp lr, #(HSREC_DATA_ABORT << HSREC_SHIFT)
    beq arm_data_abort
    cmp lr, #(HSREC_UNKNOWN << HSREC_SHIFT)
    beq arm_undefined_inst

    /** Everything else is assumed to be a VCPU trap **/
    mrs lr, elr_hyp
    PUSH_STACK
    mrc HSR(r0)
    b c_handle_vcpu_fault
END_FUNC(arm_hyp_trap)


BEGIN_FUNC(arm_undefined_inst)
    mrs lr, elr_hyp
    PUSH_STACK
    b c_handle_undefined_instruction
END_FUNC(arm_undefined_inst)

BEGIN_FUNC(arm_prefetch_abort)
    mrs lr, elr_hyp
    PUSH_STACK
    b c_handle_instruction_fault
END_FUNC(arm_prefetch_abort)

BEGIN_FUNC(arm_data_abort)
    mrs lr, elr_hyp
    PUSH_STACK
    b c_handle_data_fault
END_FUNC(arm_data_abort)

BEGIN_FUNC(arm_syscall)
    mrs lr, elr_hyp
    sub lr, lr, #4
    PUSH_STACK
    /* r0: cptr, r1: msgInfo, r2: syscallNo */
    mov r2, r7
    b c_handle_syscall
END_FUNC(arm_syscall)

/*********************************
 *** Traps taken from HYP mode ***
 *********************************/

BEGIN_FUNC(arm_hyp_prefetch_abort_exception)
#ifdef DEBUG
    mrc p15, 4, sp, c13, c0, 2
    mrs r0, elr_hyp
    blx kernelPrefetchAbort
#endif
    mrc HSR(r9)    /* Get Hype Syndrome Register. */
    mrc HIFAR(r10) /* Get fault address register. */
1: b 1b
END_FUNC(arm_hyp_prefetch_abort_exception)

BEGIN_FUNC(arm_hyp_data_abort_exception)
#ifdef DEBUG
    mrc p15, 4, sp, c13, c0, 2
    mrs r0, elr_hyp
    blx kernelDataAbort
#endif
    mrc HSR(r9)    /* Get Hype Syndrome Register. */
    mrc HDFAR(r10) /* Get fault address register. */
1: b 1b
END_FUNC(arm_hyp_data_abort_exception)

BEGIN_FUNC(arm_hyp_undefined_inst_exception)
#ifdef DEBUG
    mrc p15, 4, sp, c13, c0, 2
    mrs r0, elr_hyp
    blx kernelUndefinedInstruction
#endif
    mrc HSR(r9)    /* Get Hype Syndrome Register. */
    mrc HIFAR(r10) /* Get fault address register. */
1: b 1b
END_FUNC(arm_hyp_undefined_inst_exception)

BEGIN_FUNC(arm_hyp_syscall)
    b arm_hyp_undefined_inst_exception
END_FUNC(arm_hyp_syscall)

/************************
 *** Other exceptions ***
 ************************/

BEGIN_FUNC(arm_hyp_irq_exception)
    EX_ENTRY
    mrs lr, elr_hyp
    PUSH_STACK
    b c_handle_interrupt
END_FUNC(arm_hyp_irq_exception)

BEGIN_FUNC(arm_hyp_reset_exception)
    blx halt
END_FUNC(arm_hyp_reset_exception)

BEGIN_FUNC(arm_hyp_fiq_exception)
    blx halt
END_FUNC(arm_hyp_fiq_exception)

#endif /* CONFIG_ARM_HYP */
