# @LICENSE(OKL_CORE)

#include <config.h>
#include <machine/assembler.h>

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

#define VM_EVENT_DATA_ABORT 0
#define VM_EVENT_PREFETCH_ABORT 1

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

/* Helper to return to userland. */
.macro RET_TO_USER, cur_thread_reg
    /* cur_thread_reg should contain the address of ksCurThread. */
    /* Set stack pointer to point at the r0 of the user context. */
    ldr sp, [\cur_thread_reg]
    /* Pop user registers */
    pop {r0-r12}
    /* Retore the user stack pointer */
    pop {lr}
    msr sp_usr, lr

    /* prepare the eception return lr */
    ldr lr, [sp, #4]
    msr elr_hyp, lr
    /* prepare the user status register */
    ldr lr, [sp, #8]
    msr spsr_hyp, lr

    /* Finally, pop our LR */
    pop {lr}
    /* Return to user */
    eret
.endm

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

    /* Store FaultInstruction */
    mrs lr, elr_hyp
    str lr, [sp, #(PT_FaultInstruction - PT_SP)]
    /* Stack all user registers */
    push {r0-r12}
    /* Load the kernel's real stack pointer */
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    /* Call out to the user */
    ldr r7, =ksCurThread
    mrc HSR(r0)
    blx handleVCPUFault

    /* Exception return */
    RET_TO_USER r7
END_FUNC(arm_hyp_trap)


BEGIN_FUNC(arm_undefined_inst)
    /* Store FaultInstruction */
    mrs lr, elr_hyp
    str lr, [sp, #(PT_FaultInstruction - PT_SP)]
    /* Stack all user registers */
    push {r0-r12}
    /* Load the kernel's real stack pointer */
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    /* Call out to the user */
    ldr r7, =ksCurThread
    mov r0, #0
    mov r1, #0
    blx handleUserLevelFault

    /* Exception return */
    RET_TO_USER r7
END_FUNC(arm_undefined_inst)

BEGIN_FUNC(arm_prefetch_abort)
    /* Store FaultInstruction */
    mrs lr, elr_hyp
    str lr, [sp, #(PT_FaultInstruction - PT_SP)]
    /* Stack all user registers */
    push {r0-r12}
    /* Load the kernel's real stack pointer */
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    /* Call out to the user */
    ldr r7, =ksCurThread
    mov r0, #VM_EVENT_PREFETCH_ABORT
    blx handleVMFaultEvent

    /* Exception return */
    RET_TO_USER r7
END_FUNC(arm_prefetch_abort)

BEGIN_FUNC(arm_data_abort)
    /* Store FaultInstruction */
    mrs lr, elr_hyp
    str lr, [sp, #(PT_FaultInstruction - PT_SP)]
    /* Stack all user registers */
    push {r0-r12}
    /* Load the kernel's real stack pointer */
    mov sp, #(PPTR_KERNEL_STACK_TOP)

    /* Call out to the user */
    ldr r7, =ksCurThread
    mov r0, #VM_EVENT_DATA_ABORT
    blx handleVMFaultEvent

    /* Exception return */
    RET_TO_USER r7
END_FUNC(arm_data_abort)

BEGIN_FUNC(arm_syscall)
    /* Store FaultInstruction */
    mrs lr, elr_hyp
    sub lr, lr, #4
    str lr, [sp, #(PT_FaultInstruction - PT_SP)]

#ifdef FASTPATH
    cmp r7, #SYSCALL_REPLY_RECV
#endif

    /* Stack all user registers */
    push {r0-r12}

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
    beq fastpath_reply_recv
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
    bhs arm_hyp_undefined_syscall
    blx handleSyscall

    /* Return to user. */
    RET_TO_USER r8
END_FUNC(arm_syscall)

BEGIN_FUNC(arm_hyp_undefined_syscall)
    blx handleUnknownSyscall
    RET_TO_USER r8
END_FUNC(arm_hyp_undefined_syscall)

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

    /* Pop user registers, preserving r0 and r1 */
    add sp, sp, #8
    pop {r2-r12}
    /* Retore the user stack pointer */
    pop {lr}
    msr sp_usr, lr
    /* prepare the exception return lr */
    ldr lr, [sp, #4]
    msr elr_hyp, lr
    /* prepare the user status register */
    ldr lr, [sp, #8]
    msr spsr_hyp, lr
    /* Finally, pop our LR */
    pop {lr}
    /* Return to user */
    eret
END_FUNC(fastpath_restore)
#endif /* FASTPATH */

/*********************************
 *** Traps taken from HYP mode ***
 *********************************/

BEGIN_FUNC(arm_hyp_prefetch_abort_exception)
#ifdef DEBUG
    mov sp, #(PPTR_KERNEL_STACK_TOP)
    mrs r0, elr_hyp
    blx kernelPrefetchAbort
#endif
    mrc HSR(r9)    /* Get Hype Syndrome Register. */
    mrc HIFAR(r10) /* Get fault address register. */
1: b 1b
END_FUNC(arm_hyp_prefetch_abort_exception)

BEGIN_FUNC(arm_hyp_data_abort_exception)
#ifdef DEBUG
    mov sp, #(PPTR_KERNEL_STACK_TOP)
    mrs r0, elr_hyp
    blx kernelDataAbort
#endif
    mrc HSR(r9)    /* Get Hype Syndrome Register. */
    mrc HDFAR(r10) /* Get fault address register. */
1: b 1b
END_FUNC(arm_hyp_data_abort_exception)

BEGIN_FUNC(arm_hyp_undefined_inst_exception)
#ifdef DEBUG
    mov sp, #(PPTR_KERNEL_STACK_TOP)
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

    /* Store FaultInstruction */
    mrs lr, elr_hyp
    str lr, [sp, #(PT_FaultInstruction - PT_SP)]
    /* Stack all user registers */
    push {r0-r12}

    /* Handle the exception */
    ldr r7, =ksCurThread
    mov sp, #(PPTR_KERNEL_STACK_TOP)
    blx handleInterruptEntry

    /* Return to the user */
    RET_TO_USER r7
END_FUNC(arm_hyp_irq_exception)

BEGIN_FUNC(arm_hyp_reset_exception)
    blx halt
END_FUNC(arm_hyp_reset_exception)

BEGIN_FUNC(arm_hyp_fiq_exception)
    blx halt
END_FUNC(arm_hyp_fiq_exception)

