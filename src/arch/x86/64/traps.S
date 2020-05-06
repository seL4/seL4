/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/assembler.h>
#include <config.h>
#include <hardware.h>

/*
 * The exception in 64-bit mode:
 * All interrupt handlers pointed by the IDT are in 64-bit code. (this does not apply to SMI handler)
 * The size of interrupt-stack pushes is fixed at 64 bits; and the processor uses 8-byte, zero extended
 * stores.
 * The stack pointer (SS:RSP) is pushed unconditionally on interrupts.
 * The new SS is set to NULL if there is a change in CPL.
 * Only 64-bit interrupt and trap gates can be referenced in x86-64 mode.
 * No 32-bit interrupt or trap gate type exists in x86-64 mode.
 * The RSP is aligned to a 16-byte boundary before pushing the stack frame.
 * In x86-64 mode, when stacks are switched as part of a 64-bit mode privilege-level
 * change, a new SS descriptor is not loaded. x86-64 mode loads only an inner-level
 * RSP from the TSS. The new SS selector is forced to NULL and the SS selctor's RPL
 * field is set to the new CPL. The old SS and RSP are saved on the new stack.
 *
 * Stack Usage with Privilege-Level Change
 *
 *          SS                      +40
 *         RSP                      +32
 *      RFLAGS                      +24
 *          CS                      +16
 *         RIP                      +8
 *     ErrCode                      0  - RSP
 *
 * About Segment in x86-64 mode
 * ES, DS and SS segment registers are not used in 64-bit mode, their
 * fields (base, limit and attribute) in segment descriptor registers
 * are ignored. Some forms of segment load instructions are also invalid.
 * Address caculations that reference the DS, ES or SS segments are treated
 * as if the segment base is zero. Mode change does not change the contents
 * of the segment registers or associated descriptor register. These registers
 * are also not changed during 64-bit mode exectuion, unless explicit
 * segment loads are performed.
 *
 * In order to setup compability mode for an application, segment-load
 * instructions (mov to Sreg, pop Sreg) work normally in 64-bit mode. An
 * entry is read from the system descriptor table (GDT or LDT) and is loaded
 * in the hidden portion of the segment descriptor. The descriptor-register
 * base, limit and attribute fields are all loaded. However, the contents
 * of the data and stack segment selector and the descriptor registers are ignored
 */

#ifdef ENABLE_SMP_SUPPORT
/* If using multicore our gs base is set to point to a nodeInfo_t structure.
 * Inside that is the 'currentThreadUserContext' that points to the first
 * register we want to push in the case of the fastsyscall trap. See the
 * comment in the nodeInfo struct for more details
 */
#define MAYBE_SWAPGS swapgs
#define LOAD_USER_CONTEXT movq %gs:16, %rsp
/* The user context under SMP is always set to Error + 1 for the current
 * thread */
#define LOAD_USER_CONTEXT_OFFSET(x) LOAD_USER_CONTEXT; addq $((x) * 8), %rsp
#define LOAD_KERNEL_STACK movq %gs:0, %rsp
#define LOAD_IRQ_STACK(x) movq %gs:8, %x
#else
/* The location in the user context we want is in ksCurThread after the
 * fpu state (CONFIG_XSAVE_SIZE) and then the end of the user context is
 * after 22 words (22 == n_immContextRegisters). By default (in the case
 * of a fast syscall trap) we skip 6 registers (SS, CS, RCX, R11,
 * FaultIP, and RSP) and are ready to push Error.
 */
#define MAYBE_SWAPGS
#define LOAD_USER_CONTEXT_OFFSET(x) movq (ksCurThread), %rsp; addq $(CONFIG_XSAVE_SIZE + 22*8 - 6*8 + (x)*8), %rsp
#define LOAD_USER_CONTEXT LOAD_USER_CONTEXT_OFFSET(0)
#define LOAD_KERNEL_STACK leaq    kernel_stack_alloc + (1 << CONFIG_KERNEL_STACK_BITS), %rsp
#define LOAD_IRQ_STACK(x)  leaq    x64KSIRQStack, %x
#endif

#ifdef CONFIG_KERNEL_SKIM_WINDOW
/* If using PCIDs then our final value is not an address with a valid 32-bit relocation for
 * the linker, but rather requires a full 64-bit representation. To work around this we
 * use movabs to generate a full 64-bit immediate if using PCID, but lea if not. We prefer
 * lea where possible as it has a more efficient instruction representation
 */
#ifdef CONFIG_SUPPORT_PCID
#define LOAD_KERNEL_AS(reg) \
    movabs $x64KSKernelPML4 - KERNEL_ELF_BASE_OFFSET + (1 << 63), %reg; \
    movq %reg, %cr3;
#else /* !CONFIG_SUPPORT_PCID */
#define LOAD_KERNEL_AS(reg) \
    lea x64KSKernelPML4 - KERNEL_ELF_BASE_OFFSET, %reg; \
    movq %reg, %cr3;
#endif /* CONFIG_SUPPORT_PCID */
#else /* !CONFIG_KERNEL_SKIM_WINDOW */

#define LOAD_KERNEL_AS(reg)

#endif /* CONFIG_KERNEL_SKIM_WINDOW */

/* Registers to be pushed after an interrupt
   %rcx must be pushed beforehand */
#define INT_SAVE_STATE                              \
    push    %r11;                                   \
    /* skip FaultIP, RSP, Error, NextIP, RFLAGS */  \
    subq    $(5 * 8), %rsp;                         \
    push    %r15;                                   \
    push    %r9;                                    \
    push    %r8;                                    \
    push    %r10;                                   \
    push    %rdx;                                   \
    push    %r14;                                   \
    push    %r13;                                   \
    push    %r12;                                   \
    push    %rbp;                                   \
    push    %rbx;                                   \
    push    %rax;                                   \
    push    %rsi;                                   \
    push    %rdi

/* Kernel exception handler if interrupt vector < 32. */
#define EXPAND_EXCEPT_1(number)                     \
    /* use saved stack */                           \
    movq    32(%rsp), %rsp;                         \
    push    %rcx;                                   \
    movq    $0x##number, %rcx;                      \
    jmp     kernel_exception

/* Kernel exception handler if interrupt vector >= 32.
   Either a normal interrupt from the idle thread
   or a nested interrupt. */
#define EXPAND_EXCEPT_0(number)                     \
    /* Check the saved esp, if its 0 we came from */\
    /* the idle thread and have a normal interrupt*/\
    addq    $48, %rsp;                              \
    cmpq    $0, -16(%rsp);                          \
    je      2b;                                     \
    /* nested interrupt, use saved stack */         \
    movq    -16(%rsp), %rsp;                        \
    /* skip 128 bytes as we need to respect the */  \
    /* red zone */                                  \
    subq    $128, %rsp;                             \
    push    %rcx;                                   \
    movq    $0x##number, %rcx;                      \
    jmp     nested_interrupt

#define INT_HANDLE_COMMON_EXCEPT(number,except) _expand_except_(except)(number)
#define _expand_except_(except) EXPAND_EXCEPT_##except

#define INT_HANDLER_COMMON(number,error_code,except) \
.global int_##number;                               \
.type   int_##number, %function;                    \
int_##number:                                       \
    error_code;                                     \
    /* Check CPL */                                 \
    testq   $3, 16(%rsp);                           \
    jz      1f;                                     \
2:                                                  \
    LOAD_KERNEL_AS(rsp)                             \
    /* we need to not skip RSP, FaultIP, R11 and RCX for now */ \
    MAYBE_SWAPGS;                                   \
    LOAD_USER_CONTEXT_OFFSET(4);                    \
    push    %rcx;                                   \
    movq    $0x##number, %rcx;                      \
    jmp     handle_interrupt;                       \
1:                                                  \
    INT_HANDLE_COMMON_EXCEPT(number,except);        \
.size   int_##number, . - int_##number;

#define INT_HANDLER_WITH_ERR_CODE(number,except) INT_HANDLER_COMMON(number,,except)
#define INT_HANDLER_WITHOUT_ERR_CODE(number,except) INT_HANDLER_COMMON(number,pushq $0x0,except)

.section .text
.code64

INT_HANDLER_WITHOUT_ERR_CODE(00,1)
INT_HANDLER_WITHOUT_ERR_CODE(01,1)
INT_HANDLER_WITHOUT_ERR_CODE(02,1)
INT_HANDLER_WITHOUT_ERR_CODE(03,1)
INT_HANDLER_WITHOUT_ERR_CODE(04,1)
INT_HANDLER_WITHOUT_ERR_CODE(05,1)
INT_HANDLER_WITHOUT_ERR_CODE(06,1)
INT_HANDLER_WITHOUT_ERR_CODE(07,1)
INT_HANDLER_WITH_ERR_CODE(08,1)
INT_HANDLER_WITHOUT_ERR_CODE(09,1)
INT_HANDLER_WITH_ERR_CODE(0a,1)
INT_HANDLER_WITH_ERR_CODE(0b,1)
INT_HANDLER_WITH_ERR_CODE(0c,1)
INT_HANDLER_WITH_ERR_CODE(0d,1)
INT_HANDLER_WITH_ERR_CODE(0e,1)
INT_HANDLER_WITHOUT_ERR_CODE(0f,1)

INT_HANDLER_WITHOUT_ERR_CODE(10,1)
INT_HANDLER_WITH_ERR_CODE(11,1)
INT_HANDLER_WITHOUT_ERR_CODE(12,1)
INT_HANDLER_WITHOUT_ERR_CODE(13,1)
INT_HANDLER_WITHOUT_ERR_CODE(14,1)
INT_HANDLER_WITHOUT_ERR_CODE(15,1)
INT_HANDLER_WITHOUT_ERR_CODE(16,1)
INT_HANDLER_WITHOUT_ERR_CODE(17,1)
INT_HANDLER_WITHOUT_ERR_CODE(18,1)
INT_HANDLER_WITHOUT_ERR_CODE(19,1)
INT_HANDLER_WITHOUT_ERR_CODE(1a,1)
INT_HANDLER_WITHOUT_ERR_CODE(1b,1)
INT_HANDLER_WITHOUT_ERR_CODE(1c,1)
INT_HANDLER_WITHOUT_ERR_CODE(1d,1)
INT_HANDLER_WITHOUT_ERR_CODE(1e,1)
INT_HANDLER_WITHOUT_ERR_CODE(1f,1)

INT_HANDLER_WITHOUT_ERR_CODE(20,0)
INT_HANDLER_WITHOUT_ERR_CODE(21,0)
INT_HANDLER_WITHOUT_ERR_CODE(22,0)
INT_HANDLER_WITHOUT_ERR_CODE(23,0)
INT_HANDLER_WITHOUT_ERR_CODE(24,0)
INT_HANDLER_WITHOUT_ERR_CODE(25,0)
INT_HANDLER_WITHOUT_ERR_CODE(26,0)
INT_HANDLER_WITHOUT_ERR_CODE(27,0)
INT_HANDLER_WITHOUT_ERR_CODE(28,0)
INT_HANDLER_WITHOUT_ERR_CODE(29,0)
INT_HANDLER_WITHOUT_ERR_CODE(2a,0)
INT_HANDLER_WITHOUT_ERR_CODE(2b,0)
INT_HANDLER_WITHOUT_ERR_CODE(2c,0)
INT_HANDLER_WITHOUT_ERR_CODE(2d,0)
INT_HANDLER_WITHOUT_ERR_CODE(2e,0)
INT_HANDLER_WITHOUT_ERR_CODE(2f,0)

INT_HANDLER_WITHOUT_ERR_CODE(30,0)
INT_HANDLER_WITHOUT_ERR_CODE(31,0)
INT_HANDLER_WITHOUT_ERR_CODE(32,0)
INT_HANDLER_WITHOUT_ERR_CODE(33,0)
INT_HANDLER_WITHOUT_ERR_CODE(34,0)
INT_HANDLER_WITHOUT_ERR_CODE(35,0)
INT_HANDLER_WITHOUT_ERR_CODE(36,0)
INT_HANDLER_WITHOUT_ERR_CODE(37,0)
INT_HANDLER_WITHOUT_ERR_CODE(38,0)
INT_HANDLER_WITHOUT_ERR_CODE(39,0)
INT_HANDLER_WITHOUT_ERR_CODE(3a,0)
INT_HANDLER_WITHOUT_ERR_CODE(3b,0)
INT_HANDLER_WITHOUT_ERR_CODE(3c,0)
INT_HANDLER_WITHOUT_ERR_CODE(3d,0)
INT_HANDLER_WITHOUT_ERR_CODE(3e,0)
INT_HANDLER_WITHOUT_ERR_CODE(3f,0)

INT_HANDLER_WITHOUT_ERR_CODE(40,0)
INT_HANDLER_WITHOUT_ERR_CODE(41,0)
INT_HANDLER_WITHOUT_ERR_CODE(42,0)
INT_HANDLER_WITHOUT_ERR_CODE(43,0)
INT_HANDLER_WITHOUT_ERR_CODE(44,0)
INT_HANDLER_WITHOUT_ERR_CODE(45,0)
INT_HANDLER_WITHOUT_ERR_CODE(46,0)
INT_HANDLER_WITHOUT_ERR_CODE(47,0)
INT_HANDLER_WITHOUT_ERR_CODE(48,0)
INT_HANDLER_WITHOUT_ERR_CODE(49,0)
INT_HANDLER_WITHOUT_ERR_CODE(4a,0)
INT_HANDLER_WITHOUT_ERR_CODE(4b,0)
INT_HANDLER_WITHOUT_ERR_CODE(4c,0)
INT_HANDLER_WITHOUT_ERR_CODE(4d,0)
INT_HANDLER_WITHOUT_ERR_CODE(4e,0)
INT_HANDLER_WITHOUT_ERR_CODE(4f,0)

INT_HANDLER_WITHOUT_ERR_CODE(50,0)
INT_HANDLER_WITHOUT_ERR_CODE(51,0)
INT_HANDLER_WITHOUT_ERR_CODE(52,0)
INT_HANDLER_WITHOUT_ERR_CODE(53,0)
INT_HANDLER_WITHOUT_ERR_CODE(54,0)
INT_HANDLER_WITHOUT_ERR_CODE(55,0)
INT_HANDLER_WITHOUT_ERR_CODE(56,0)
INT_HANDLER_WITHOUT_ERR_CODE(57,0)
INT_HANDLER_WITHOUT_ERR_CODE(58,0)
INT_HANDLER_WITHOUT_ERR_CODE(59,0)
INT_HANDLER_WITHOUT_ERR_CODE(5a,0)
INT_HANDLER_WITHOUT_ERR_CODE(5b,0)
INT_HANDLER_WITHOUT_ERR_CODE(5c,0)
INT_HANDLER_WITHOUT_ERR_CODE(5d,0)
INT_HANDLER_WITHOUT_ERR_CODE(5e,0)
INT_HANDLER_WITHOUT_ERR_CODE(5f,0)

INT_HANDLER_WITHOUT_ERR_CODE(60,0)
INT_HANDLER_WITHOUT_ERR_CODE(61,0)
INT_HANDLER_WITHOUT_ERR_CODE(62,0)
INT_HANDLER_WITHOUT_ERR_CODE(63,0)
INT_HANDLER_WITHOUT_ERR_CODE(64,0)
INT_HANDLER_WITHOUT_ERR_CODE(65,0)
INT_HANDLER_WITHOUT_ERR_CODE(66,0)
INT_HANDLER_WITHOUT_ERR_CODE(67,0)
INT_HANDLER_WITHOUT_ERR_CODE(68,0)
INT_HANDLER_WITHOUT_ERR_CODE(69,0)
INT_HANDLER_WITHOUT_ERR_CODE(6a,0)
INT_HANDLER_WITHOUT_ERR_CODE(6b,0)
INT_HANDLER_WITHOUT_ERR_CODE(6c,0)
INT_HANDLER_WITHOUT_ERR_CODE(6d,0)
INT_HANDLER_WITHOUT_ERR_CODE(6e,0)
INT_HANDLER_WITHOUT_ERR_CODE(6f,0)

INT_HANDLER_WITHOUT_ERR_CODE(70,0)
INT_HANDLER_WITHOUT_ERR_CODE(71,0)
INT_HANDLER_WITHOUT_ERR_CODE(72,0)
INT_HANDLER_WITHOUT_ERR_CODE(73,0)
INT_HANDLER_WITHOUT_ERR_CODE(74,0)
INT_HANDLER_WITHOUT_ERR_CODE(75,0)
INT_HANDLER_WITHOUT_ERR_CODE(76,0)
INT_HANDLER_WITHOUT_ERR_CODE(77,0)
INT_HANDLER_WITHOUT_ERR_CODE(78,0)
INT_HANDLER_WITHOUT_ERR_CODE(79,0)
INT_HANDLER_WITHOUT_ERR_CODE(7a,0)
INT_HANDLER_WITHOUT_ERR_CODE(7b,0)
INT_HANDLER_WITHOUT_ERR_CODE(7c,0)
INT_HANDLER_WITHOUT_ERR_CODE(7d,0)
INT_HANDLER_WITHOUT_ERR_CODE(7e,0)
INT_HANDLER_WITHOUT_ERR_CODE(7f,0)

INT_HANDLER_WITHOUT_ERR_CODE(80,0)
INT_HANDLER_WITHOUT_ERR_CODE(81,0)
INT_HANDLER_WITHOUT_ERR_CODE(82,0)
INT_HANDLER_WITHOUT_ERR_CODE(83,0)
INT_HANDLER_WITHOUT_ERR_CODE(84,0)
INT_HANDLER_WITHOUT_ERR_CODE(85,0)
INT_HANDLER_WITHOUT_ERR_CODE(86,0)
INT_HANDLER_WITHOUT_ERR_CODE(87,0)
INT_HANDLER_WITHOUT_ERR_CODE(88,0)
INT_HANDLER_WITHOUT_ERR_CODE(89,0)
INT_HANDLER_WITHOUT_ERR_CODE(8a,0)
INT_HANDLER_WITHOUT_ERR_CODE(8b,0)
INT_HANDLER_WITHOUT_ERR_CODE(8c,0)
INT_HANDLER_WITHOUT_ERR_CODE(8d,0)
INT_HANDLER_WITHOUT_ERR_CODE(8e,0)
INT_HANDLER_WITHOUT_ERR_CODE(8f,0)

INT_HANDLER_WITHOUT_ERR_CODE(90,0)
INT_HANDLER_WITHOUT_ERR_CODE(91,0)
INT_HANDLER_WITHOUT_ERR_CODE(92,0)
INT_HANDLER_WITHOUT_ERR_CODE(93,0)
INT_HANDLER_WITHOUT_ERR_CODE(94,0)
INT_HANDLER_WITHOUT_ERR_CODE(95,0)
INT_HANDLER_WITHOUT_ERR_CODE(96,0)
INT_HANDLER_WITHOUT_ERR_CODE(97,0)
INT_HANDLER_WITHOUT_ERR_CODE(98,0)
INT_HANDLER_WITHOUT_ERR_CODE(99,0)
INT_HANDLER_WITHOUT_ERR_CODE(9a,0)
INT_HANDLER_WITHOUT_ERR_CODE(9b,0)
INT_HANDLER_WITHOUT_ERR_CODE(9c,0)
INT_HANDLER_WITHOUT_ERR_CODE(9d,0)
INT_HANDLER_WITHOUT_ERR_CODE(9e,0)
INT_HANDLER_WITHOUT_ERR_CODE(9f,0)

INT_HANDLER_WITHOUT_ERR_CODE(a0,0)
INT_HANDLER_WITHOUT_ERR_CODE(a1,0)
INT_HANDLER_WITHOUT_ERR_CODE(a2,0)
INT_HANDLER_WITHOUT_ERR_CODE(a3,0)
INT_HANDLER_WITHOUT_ERR_CODE(a4,0)
INT_HANDLER_WITHOUT_ERR_CODE(a5,0)
INT_HANDLER_WITHOUT_ERR_CODE(a6,0)
INT_HANDLER_WITHOUT_ERR_CODE(a7,0)
INT_HANDLER_WITHOUT_ERR_CODE(a8,0)
INT_HANDLER_WITHOUT_ERR_CODE(a9,0)
INT_HANDLER_WITHOUT_ERR_CODE(aa,0)
INT_HANDLER_WITHOUT_ERR_CODE(ab,0)
INT_HANDLER_WITHOUT_ERR_CODE(ac,0)
INT_HANDLER_WITHOUT_ERR_CODE(ad,0)
INT_HANDLER_WITHOUT_ERR_CODE(ae,0)
INT_HANDLER_WITHOUT_ERR_CODE(af,0)

INT_HANDLER_WITHOUT_ERR_CODE(b0,0)
INT_HANDLER_WITHOUT_ERR_CODE(b1,0)
INT_HANDLER_WITHOUT_ERR_CODE(b2,0)
INT_HANDLER_WITHOUT_ERR_CODE(b3,0)
INT_HANDLER_WITHOUT_ERR_CODE(b4,0)
INT_HANDLER_WITHOUT_ERR_CODE(b5,0)
INT_HANDLER_WITHOUT_ERR_CODE(b6,0)
INT_HANDLER_WITHOUT_ERR_CODE(b7,0)
INT_HANDLER_WITHOUT_ERR_CODE(b8,0)
INT_HANDLER_WITHOUT_ERR_CODE(b9,0)
INT_HANDLER_WITHOUT_ERR_CODE(ba,0)
INT_HANDLER_WITHOUT_ERR_CODE(bb,0)
INT_HANDLER_WITHOUT_ERR_CODE(bc,0)
INT_HANDLER_WITHOUT_ERR_CODE(bd,0)
INT_HANDLER_WITHOUT_ERR_CODE(be,0)
INT_HANDLER_WITHOUT_ERR_CODE(bf,0)

INT_HANDLER_WITHOUT_ERR_CODE(c0,0)
INT_HANDLER_WITHOUT_ERR_CODE(c1,0)
INT_HANDLER_WITHOUT_ERR_CODE(c2,0)
INT_HANDLER_WITHOUT_ERR_CODE(c3,0)
INT_HANDLER_WITHOUT_ERR_CODE(c4,0)
INT_HANDLER_WITHOUT_ERR_CODE(c5,0)
INT_HANDLER_WITHOUT_ERR_CODE(c6,0)
INT_HANDLER_WITHOUT_ERR_CODE(c7,0)
INT_HANDLER_WITHOUT_ERR_CODE(c8,0)
INT_HANDLER_WITHOUT_ERR_CODE(c9,0)
INT_HANDLER_WITHOUT_ERR_CODE(ca,0)
INT_HANDLER_WITHOUT_ERR_CODE(cb,0)
INT_HANDLER_WITHOUT_ERR_CODE(cc,0)
INT_HANDLER_WITHOUT_ERR_CODE(cd,0)
INT_HANDLER_WITHOUT_ERR_CODE(ce,0)
INT_HANDLER_WITHOUT_ERR_CODE(cf,0)

INT_HANDLER_WITHOUT_ERR_CODE(d0,0)
INT_HANDLER_WITHOUT_ERR_CODE(d1,0)
INT_HANDLER_WITHOUT_ERR_CODE(d2,0)
INT_HANDLER_WITHOUT_ERR_CODE(d3,0)
INT_HANDLER_WITHOUT_ERR_CODE(d4,0)
INT_HANDLER_WITHOUT_ERR_CODE(d5,0)
INT_HANDLER_WITHOUT_ERR_CODE(d6,0)
INT_HANDLER_WITHOUT_ERR_CODE(d7,0)
INT_HANDLER_WITHOUT_ERR_CODE(d8,0)
INT_HANDLER_WITHOUT_ERR_CODE(d9,0)
INT_HANDLER_WITHOUT_ERR_CODE(da,0)
INT_HANDLER_WITHOUT_ERR_CODE(db,0)
INT_HANDLER_WITHOUT_ERR_CODE(dc,0)
INT_HANDLER_WITHOUT_ERR_CODE(dd,0)
INT_HANDLER_WITHOUT_ERR_CODE(de,0)
INT_HANDLER_WITHOUT_ERR_CODE(df,0)

INT_HANDLER_WITHOUT_ERR_CODE(e0,0)
INT_HANDLER_WITHOUT_ERR_CODE(e1,0)
INT_HANDLER_WITHOUT_ERR_CODE(e2,0)
INT_HANDLER_WITHOUT_ERR_CODE(e3,0)
INT_HANDLER_WITHOUT_ERR_CODE(e4,0)
INT_HANDLER_WITHOUT_ERR_CODE(e5,0)
INT_HANDLER_WITHOUT_ERR_CODE(e6,0)
INT_HANDLER_WITHOUT_ERR_CODE(e7,0)
INT_HANDLER_WITHOUT_ERR_CODE(e8,0)
INT_HANDLER_WITHOUT_ERR_CODE(e9,0)
INT_HANDLER_WITHOUT_ERR_CODE(ea,0)
INT_HANDLER_WITHOUT_ERR_CODE(eb,0)
INT_HANDLER_WITHOUT_ERR_CODE(ec,0)
INT_HANDLER_WITHOUT_ERR_CODE(ed,0)
INT_HANDLER_WITHOUT_ERR_CODE(ee,0)
INT_HANDLER_WITHOUT_ERR_CODE(ef,0)

INT_HANDLER_WITHOUT_ERR_CODE(f0,0)
INT_HANDLER_WITHOUT_ERR_CODE(f1,0)
INT_HANDLER_WITHOUT_ERR_CODE(f2,0)
INT_HANDLER_WITHOUT_ERR_CODE(f3,0)
INT_HANDLER_WITHOUT_ERR_CODE(f4,0)
INT_HANDLER_WITHOUT_ERR_CODE(f5,0)
INT_HANDLER_WITHOUT_ERR_CODE(f6,0)
INT_HANDLER_WITHOUT_ERR_CODE(f7,0)
INT_HANDLER_WITHOUT_ERR_CODE(f8,0)
INT_HANDLER_WITHOUT_ERR_CODE(f9,0)
INT_HANDLER_WITHOUT_ERR_CODE(fa,0)
INT_HANDLER_WITHOUT_ERR_CODE(fb,0)
INT_HANDLER_WITHOUT_ERR_CODE(fc,0)
INT_HANDLER_WITHOUT_ERR_CODE(fd,0)
INT_HANDLER_WITHOUT_ERR_CODE(fe,0)
INT_HANDLER_WITHOUT_ERR_CODE(ff,0)

BEGIN_FUNC(handle_interrupt)
    # push the rest of the registers
    INT_SAVE_STATE

    # switch to kernel stack
    LOAD_KERNEL_STACK

    # Set the arguments for c_x64_handle_interrupt
    movq    %rcx, %rdi
    movq    %rax, %rsi

    # gtfo to C land, we will not return
    call    c_x64_handle_interrupt
END_FUNC(handle_interrupt)

BEGIN_FUNC(nested_interrupt)
    # we got an interrupt from the kernel, call into c to save the irq number,
    # then return back to where we were
    INT_SAVE_STATE
    movq    %rcx, %rdi
    call    c_nested_interrupt
    # disable the interrupt flag so we don't take any more interrupts
    LOAD_IRQ_STACK(rbx)
    andq    $~0x200, 24(%rbx)
    # return
interrupt_return:
    popq %rdi
    popq %rsi
    popq %rax
    popq %rbx
    popq %rbp
    popq %r12
    popq %r13
    popq %r14
    popq %rdx
    popq %r10
    popq %r8
    popq %r9
    popq %r15
    /* skip RFLAGS, Error, NextIP, RSP, FaultIP */
    addq $(5 * 8), %rsp
    popq %r11
    popq %rcx
    LOAD_IRQ_STACK(rsp)
    addq $8, %rsp
    iretq
END_FUNC(nested_interrupt)

BEGIN_FUNC(kernel_exception)
    # push registers
    INT_SAVE_STATE
#if defined(CONFIG_HARDWARE_DEBUG_API)
    /* Before giving up and panicking, we need to test for the extra case that
     * this might be a kernel exception that is the result of EFLAGS.TF being
     * set when SYSENTER was called.
     *
     * Since EFLAGS.TF is not disabled by SYSENTER, single-stepping continues
     * into the kernel, and so causes a debug-exception in kernel code, since
     * the CPU is trying to single-step the kernel code.
     *
     * So we test for EFLAGS.TF, and if it's set, we unset it, and let the
     * exception continue. The debug exception handler will notice that it was
     * kernel exception, and handle it appropriately -- that really just means
     * setting EFLAGS.TF before SYSEXIT so that single-stepping resumes in the
     * userspace thread.
     */
    LOAD_IRQ_STACK(rdx)
    movq    24(%rdx), %rax
    movq    $(1<<8), %rbx
    testq   %rbx, %rax
    je      .not_eflags_tf

    /* Else it was EFLAGS.TF that caused the kernel exception on SYSENTER.
     * So, unset the EFLAGS.TF on the stack and this causes the syscall that we
     * will return to, to be able to execute properly.
     *
     * It will then be the debug exception handler's responsibility to re-set
     * EFLAGS.TF for the userspace thread before it returns.
     *
     * So at this point we want to just unset EFLAGS.TF and IRET immediately.
     */
    andq    $~(1<<8), %rax
    movq    %rax, 24(%rdx)

    /* Begin popping registers to IRET now. We don't need to consider any
     * unexpected side effects because we are just immediately returning after
     * entering.
     */
    popq %rdi
    popq %rsi
    popq %rax
    popq %rbx
    popq %rbp
    popq %r12
    popq %r13
    popq %r14
    popq %rdx
    popq %r10
    popq %r8
    popq %r9
    popq %r15
    /* skip RFLAGS, NextIP, Error, RSP, FaultIP */
    addq $(5 * 8), %rsp
    popq %r11
    popq %rcx
    LOAD_IRQ_STACK(rsp)
    addq $8, %rsp
    iretq

.not_eflags_tf:
#endif /* CONFIG_HARDWARE_DEBUG_API */

    movq    %rcx, %rdi
    LOAD_IRQ_STACK(rsi)
    movq    0(%rsi), %rsi    # error code
    LOAD_IRQ_STACK(rdx)
    movq    8(%rdx), %rdx    # RIP of the exception
    LOAD_IRQ_STACK(rcx)
    movq    32(%rcx), %rcx   # RSP of the exception
    LOAD_IRQ_STACK(r8)
    movq    24(%r8), %r8     # RFLAGS
    # handleKernelException(vector, errorcode, RIP, RSP, RFLAGS, CR0, CR2, CR3, CR4)
    movq    %cr0, %r9
    movq    %cr4, %r11
    push    %r11
    movq    %cr3, %r11
    push    %r11
    movq    %cr2, %r11
    push    %r11
    call    handleKernelException
    addq    $24, %rsp
    # Set RIP in the saved register context to the new IP returned from handleKernelException
    LOAD_IRQ_STACK(r8)
    movq    %rax, 8(%r8)
    jmp     interrupt_return
END_FUNC(kernel_exception)

# For a fast syscall the RFLAGS have been placed in
# r11, the instruction *AFTER* the syscall is in
# rcx. The current CS and SS have been loaded from
# IA32_LSTAR (along with this code location). Additionally
# the current RFLAGS (after saving) have been masked
# with IA32_FMASK.
BEGIN_FUNC(handle_fastsyscall)
    LOAD_KERNEL_AS(rsp)
    MAYBE_SWAPGS
    LOAD_USER_CONTEXT
    pushq   $-1             # set Error -1 to mean entry via syscall
    push    %rcx            # save NextIP
    push    %r11            # save RFLAGS
    push    %r15            # save R15 (message register)
    push    %r9             # save R9 (message register)
    push    %r8             # save R8 (message register)
    push    %r10            # save R10 (message register)
    push    %rdx            # save RDX (syscall number)
    push    %r14
    push    %r13
    push    %r12
    push    %rbp
    push    %rbx
    push    %rax
    push    %rsi            # save RSI (msgInfo register)
    push    %rdi            # save RDI (capRegister)

    # switch to kernel stack
    LOAD_KERNEL_STACK

    # RSI, RDI and RDX are already correct for calling c_handle_syscall
    # gtfo to C land, we will not return
#ifdef CONFIG_KERNEL_MCS
    # mov reply to correct register for calling c_handle_syscall
    movq   %r12, %rcx
#endif
    jmp    c_handle_syscall
END_FUNC(handle_fastsyscall)

# Handle Syscall (coming via sysenter)
# Assume following register contents when called:
#   RAX : syscall number
#   RCX : user ESP
#   RDX : user EIP (pointing to the sysenter instruction)
#   RSP : NULL
BEGIN_FUNC(handle_syscall)
    /* We need to save r11, rdx TLS_BASE and RSP */
    LOAD_KERNEL_AS(rsp)
    MAYBE_SWAPGS
    LOAD_USER_CONTEXT_OFFSET(3)
    push    %r11
    push    %rdx            # save FaultIP
    push    %rcx            # save RSP
    push    $-1             # set Error -1 to mean entry via syscall
    push    %rdx            # save FaultIP (which will need to be updated later)
    pushf                   # save RFLAGS
    orq     $0x200, (%rsp)  # set interrupt bit in save RFLAGS
    push    %r15            # save R15 (message register)
    push    %r9             # save R9 (message register)
    push    %r8             # save R8 (message register)
    push    %r10            # save R10 (message register)
    subq    $8, %rsp        # skip RDX
    push    %r14
    push    %r13
    push    %r12
    push    %rbp
    push    %rbx
    push    %rax            # save RAX (syscall number)
    push    %rsi            # save RSI (msgInfo register)
    push    %rdi            # save RDI (capRegister)

    # switch to kernel stack
    LOAD_KERNEL_STACK

    # RSI, RDI are already correct for calling c_handle_syscall
    movq %rax, %rdx
    # gtfo to C land, we will not return
#ifdef CONFIG_KERNEL_MCS
    # mov reply to correct register for calling c_handle_syscall
    movq   %r12, %rcx
#endif
    call    c_handle_syscall
END_FUNC(handle_syscall)

# Handle vmexit
# RSP points to the end of the VCPUs general purpose register array
#ifdef CONFIG_VTX
BEGIN_FUNC(handle_vmexit)
    MAYBE_SWAPGS
    push    %rbp
    push    %rdi
    push    %rsi
    push    %rdx
    push    %rcx
    push    %rbx
    push    %rax

    # switch to kernel stack
    LOAD_KERNEL_STACK
    # Handle the vmexit, we will not return
    call    c_handle_vmexit
END_FUNC(handle_vmexit)
#endif /* CONFIG_VTX */
