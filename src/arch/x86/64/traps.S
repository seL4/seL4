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

#include <machine/assembler.h>
#include <config.h>

/*
 * The exception in 64-bit mode:
 * All interrupt handlers pointed by the IDT are in 64-bit code. (this does not apply to SMI handler)
 * The size of interrupt-stack pushes is fixed at 64 bits; and the processor uses 8-byte, zero extended
 * stores.
 * The stack pointer (SS:RSP) is pushed unconditionally on interrupts.
 * The new SS is set to NULL if there is a change in CPL.
 * Only 64-bit interrupt and trap gates can be referenced in x86-64 mode.
 * No 32-bit interrupt or trap gate type exists in x86-64 mode.
 * The RSP is aligned to a 16-bypte boundary before pushing the stack frame.
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

#if CONFIG_MAX_NUM_NODES > 1
/* If using multicore our gs base is set to point to a nodeInfo_t structure.
 * Inside that is the 'currentThreadUserContext' that points to the first
 * register we want to push in the case of the fastsyscall trap. See the
 * comment in the nodeInfo struct for more details
 */
#define MAYBE_SWAPGS swapgs
#define LOAD_USER_CONTEXT movq %gs:16, %rsp
#define LOAD_USER_CONTEXT_OFFSET(x) LOAD_USER_CONTEXT; addq $((x) * 8), %rsp
#define LOAD_KERNEL_STACK movq %gs:0, %rsp
#define LOAD_IRQ_STACK(x) movq %gs:8, %x
#else
/* The location in the user context we want is in ksCurThread after the fpu
 * state (CONFIG_XSAVE_SIZE) and then the end of the user context is after
 * 23 words (23 == n_contextRegisters). By default (in the case of a fast
 * syscall trap) we skip 7 registers (SS, CS, RCX, R11, FaultIP, TLS_BASE
 * and RSP) and are ready to push Error.
 */
#define MAYBE_SWAPGS
#define LOAD_USER_CONTEXT_OFFSET(x) movq (ksCurThread), %rsp; addq $(CONFIG_XSAVE_SIZE + 23*8 - 7*8 + (x)*8), %rsp
#define LOAD_USER_CONTEXT LOAD_USER_CONTEXT_OFFSET(0)
#define LOAD_KERNEL_STACK leaq    kernel_stack_alloc + 4096, %rsp
#define LOAD_IRQ_STACK(x)  leaq    x64KSIRQStack, %x
#endif

#define INT_HANDLER_COMMON(number, error_code)       \
.global int_##number;                               \
.type   int_##number, %function;                    \
int_##number:                                       \
    error_code;                                     \
    /* we need to not skip RSP, TLS_BASE, FaultIP, R11 and RCX for now */ \
    MAYBE_SWAPGS;                                   \
    LOAD_USER_CONTEXT_OFFSET(5);                    \
    push    %rcx;                                   \
    push    %r11;                                   \
    /* skip FaultIP TLS_BASE, RSP, NextIP, Error, RFLAGS */ \
    subq    $(6 * 8), %rsp;                         \
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
    push    %rdi;                                   \
    movq    $0x##number, %rcx;                      \
    jmp     handle_interrupt;                       \
.size   int_##number, . - int_##number;

#define INT_HANDLER_WITH_ERR_CODE(number) INT_HANDLER_COMMON(number,)
#define INT_HANDLER_WITHOUT_ERR_CODE(number) INT_HANDLER_COMMON(number, pushq $0x0)

.section .text
.code64

INT_HANDLER_WITHOUT_ERR_CODE(00)
INT_HANDLER_WITHOUT_ERR_CODE(01)
INT_HANDLER_WITHOUT_ERR_CODE(02)
INT_HANDLER_WITHOUT_ERR_CODE(03)
INT_HANDLER_WITHOUT_ERR_CODE(04)
INT_HANDLER_WITHOUT_ERR_CODE(05)
INT_HANDLER_WITHOUT_ERR_CODE(06)
INT_HANDLER_WITHOUT_ERR_CODE(07)
INT_HANDLER_WITH_ERR_CODE(08)
INT_HANDLER_WITHOUT_ERR_CODE(09)
INT_HANDLER_WITH_ERR_CODE(0a)
INT_HANDLER_WITH_ERR_CODE(0b)
INT_HANDLER_WITH_ERR_CODE(0c)
INT_HANDLER_WITH_ERR_CODE(0d)
INT_HANDLER_WITH_ERR_CODE(0e)
INT_HANDLER_WITHOUT_ERR_CODE(0f)

INT_HANDLER_WITHOUT_ERR_CODE(10)
INT_HANDLER_WITH_ERR_CODE(11)
INT_HANDLER_WITHOUT_ERR_CODE(12)
INT_HANDLER_WITHOUT_ERR_CODE(13)
INT_HANDLER_WITHOUT_ERR_CODE(14)
INT_HANDLER_WITHOUT_ERR_CODE(15)
INT_HANDLER_WITHOUT_ERR_CODE(16)
INT_HANDLER_WITHOUT_ERR_CODE(17)
INT_HANDLER_WITHOUT_ERR_CODE(18)
INT_HANDLER_WITHOUT_ERR_CODE(19)
INT_HANDLER_WITHOUT_ERR_CODE(1a)
INT_HANDLER_WITHOUT_ERR_CODE(1b)
INT_HANDLER_WITHOUT_ERR_CODE(1c)
INT_HANDLER_WITHOUT_ERR_CODE(1d)
INT_HANDLER_WITHOUT_ERR_CODE(1e)
INT_HANDLER_WITHOUT_ERR_CODE(1f)

INT_HANDLER_WITHOUT_ERR_CODE(20)
INT_HANDLER_WITHOUT_ERR_CODE(21)
INT_HANDLER_WITHOUT_ERR_CODE(22)
INT_HANDLER_WITHOUT_ERR_CODE(23)
INT_HANDLER_WITHOUT_ERR_CODE(24)
INT_HANDLER_WITHOUT_ERR_CODE(25)
INT_HANDLER_WITHOUT_ERR_CODE(26)
INT_HANDLER_WITHOUT_ERR_CODE(27)
INT_HANDLER_WITHOUT_ERR_CODE(28)
INT_HANDLER_WITHOUT_ERR_CODE(29)
INT_HANDLER_WITHOUT_ERR_CODE(2a)
INT_HANDLER_WITHOUT_ERR_CODE(2b)
INT_HANDLER_WITHOUT_ERR_CODE(2c)
INT_HANDLER_WITHOUT_ERR_CODE(2d)
INT_HANDLER_WITHOUT_ERR_CODE(2e)
INT_HANDLER_WITHOUT_ERR_CODE(2f)

INT_HANDLER_WITHOUT_ERR_CODE(30)
INT_HANDLER_WITHOUT_ERR_CODE(31)
INT_HANDLER_WITHOUT_ERR_CODE(32)
INT_HANDLER_WITHOUT_ERR_CODE(33)
INT_HANDLER_WITHOUT_ERR_CODE(34)
INT_HANDLER_WITHOUT_ERR_CODE(35)
INT_HANDLER_WITHOUT_ERR_CODE(36)
INT_HANDLER_WITHOUT_ERR_CODE(37)
INT_HANDLER_WITHOUT_ERR_CODE(38)
INT_HANDLER_WITHOUT_ERR_CODE(39)
INT_HANDLER_WITHOUT_ERR_CODE(3a)
INT_HANDLER_WITHOUT_ERR_CODE(3b)
INT_HANDLER_WITHOUT_ERR_CODE(3c)
INT_HANDLER_WITHOUT_ERR_CODE(3d)
INT_HANDLER_WITHOUT_ERR_CODE(3e)
INT_HANDLER_WITHOUT_ERR_CODE(3f)

INT_HANDLER_WITHOUT_ERR_CODE(40)
INT_HANDLER_WITHOUT_ERR_CODE(41)
INT_HANDLER_WITHOUT_ERR_CODE(42)
INT_HANDLER_WITHOUT_ERR_CODE(43)
INT_HANDLER_WITHOUT_ERR_CODE(44)
INT_HANDLER_WITHOUT_ERR_CODE(45)
INT_HANDLER_WITHOUT_ERR_CODE(46)
INT_HANDLER_WITHOUT_ERR_CODE(47)
INT_HANDLER_WITHOUT_ERR_CODE(48)
INT_HANDLER_WITHOUT_ERR_CODE(49)
INT_HANDLER_WITHOUT_ERR_CODE(4a)
INT_HANDLER_WITHOUT_ERR_CODE(4b)
INT_HANDLER_WITHOUT_ERR_CODE(4c)
INT_HANDLER_WITHOUT_ERR_CODE(4d)
INT_HANDLER_WITHOUT_ERR_CODE(4e)
INT_HANDLER_WITHOUT_ERR_CODE(4f)

INT_HANDLER_WITHOUT_ERR_CODE(50)
INT_HANDLER_WITHOUT_ERR_CODE(51)
INT_HANDLER_WITHOUT_ERR_CODE(52)
INT_HANDLER_WITHOUT_ERR_CODE(53)
INT_HANDLER_WITHOUT_ERR_CODE(54)
INT_HANDLER_WITHOUT_ERR_CODE(55)
INT_HANDLER_WITHOUT_ERR_CODE(56)
INT_HANDLER_WITHOUT_ERR_CODE(57)
INT_HANDLER_WITHOUT_ERR_CODE(58)
INT_HANDLER_WITHOUT_ERR_CODE(59)
INT_HANDLER_WITHOUT_ERR_CODE(5a)
INT_HANDLER_WITHOUT_ERR_CODE(5b)
INT_HANDLER_WITHOUT_ERR_CODE(5c)
INT_HANDLER_WITHOUT_ERR_CODE(5d)
INT_HANDLER_WITHOUT_ERR_CODE(5e)
INT_HANDLER_WITHOUT_ERR_CODE(5f)

INT_HANDLER_WITHOUT_ERR_CODE(60)
INT_HANDLER_WITHOUT_ERR_CODE(61)
INT_HANDLER_WITHOUT_ERR_CODE(62)
INT_HANDLER_WITHOUT_ERR_CODE(63)
INT_HANDLER_WITHOUT_ERR_CODE(64)
INT_HANDLER_WITHOUT_ERR_CODE(65)
INT_HANDLER_WITHOUT_ERR_CODE(66)
INT_HANDLER_WITHOUT_ERR_CODE(67)
INT_HANDLER_WITHOUT_ERR_CODE(68)
INT_HANDLER_WITHOUT_ERR_CODE(69)
INT_HANDLER_WITHOUT_ERR_CODE(6a)
INT_HANDLER_WITHOUT_ERR_CODE(6b)
INT_HANDLER_WITHOUT_ERR_CODE(6c)
INT_HANDLER_WITHOUT_ERR_CODE(6d)
INT_HANDLER_WITHOUT_ERR_CODE(6e)
INT_HANDLER_WITHOUT_ERR_CODE(6f)

INT_HANDLER_WITHOUT_ERR_CODE(70)
INT_HANDLER_WITHOUT_ERR_CODE(71)
INT_HANDLER_WITHOUT_ERR_CODE(72)
INT_HANDLER_WITHOUT_ERR_CODE(73)
INT_HANDLER_WITHOUT_ERR_CODE(74)
INT_HANDLER_WITHOUT_ERR_CODE(75)
INT_HANDLER_WITHOUT_ERR_CODE(76)
INT_HANDLER_WITHOUT_ERR_CODE(77)
INT_HANDLER_WITHOUT_ERR_CODE(78)
INT_HANDLER_WITHOUT_ERR_CODE(79)
INT_HANDLER_WITHOUT_ERR_CODE(7a)
INT_HANDLER_WITHOUT_ERR_CODE(7b)
INT_HANDLER_WITHOUT_ERR_CODE(7c)
INT_HANDLER_WITHOUT_ERR_CODE(7d)
INT_HANDLER_WITHOUT_ERR_CODE(7e)
INT_HANDLER_WITHOUT_ERR_CODE(7f)

INT_HANDLER_WITHOUT_ERR_CODE(80)
INT_HANDLER_WITHOUT_ERR_CODE(81)
INT_HANDLER_WITHOUT_ERR_CODE(82)
INT_HANDLER_WITHOUT_ERR_CODE(83)
INT_HANDLER_WITHOUT_ERR_CODE(84)
INT_HANDLER_WITHOUT_ERR_CODE(85)
INT_HANDLER_WITHOUT_ERR_CODE(86)
INT_HANDLER_WITHOUT_ERR_CODE(87)
INT_HANDLER_WITHOUT_ERR_CODE(88)
INT_HANDLER_WITHOUT_ERR_CODE(89)
INT_HANDLER_WITHOUT_ERR_CODE(8a)
INT_HANDLER_WITHOUT_ERR_CODE(8b)
INT_HANDLER_WITHOUT_ERR_CODE(8c)
INT_HANDLER_WITHOUT_ERR_CODE(8d)
INT_HANDLER_WITHOUT_ERR_CODE(8e)
INT_HANDLER_WITHOUT_ERR_CODE(8f)

INT_HANDLER_WITHOUT_ERR_CODE(90)
INT_HANDLER_WITHOUT_ERR_CODE(91)
INT_HANDLER_WITHOUT_ERR_CODE(92)
INT_HANDLER_WITHOUT_ERR_CODE(93)
INT_HANDLER_WITHOUT_ERR_CODE(94)
INT_HANDLER_WITHOUT_ERR_CODE(95)
INT_HANDLER_WITHOUT_ERR_CODE(96)
INT_HANDLER_WITHOUT_ERR_CODE(97)
INT_HANDLER_WITHOUT_ERR_CODE(98)
INT_HANDLER_WITHOUT_ERR_CODE(99)
INT_HANDLER_WITHOUT_ERR_CODE(9a)
INT_HANDLER_WITHOUT_ERR_CODE(9b)
INT_HANDLER_WITHOUT_ERR_CODE(9c)
INT_HANDLER_WITHOUT_ERR_CODE(9d)
INT_HANDLER_WITHOUT_ERR_CODE(9e)
INT_HANDLER_WITHOUT_ERR_CODE(9f)

INT_HANDLER_WITHOUT_ERR_CODE(a0)
INT_HANDLER_WITHOUT_ERR_CODE(a1)
INT_HANDLER_WITHOUT_ERR_CODE(a2)
INT_HANDLER_WITHOUT_ERR_CODE(a3)
INT_HANDLER_WITHOUT_ERR_CODE(a4)
INT_HANDLER_WITHOUT_ERR_CODE(a5)
INT_HANDLER_WITHOUT_ERR_CODE(a6)
INT_HANDLER_WITHOUT_ERR_CODE(a7)
INT_HANDLER_WITHOUT_ERR_CODE(a8)
INT_HANDLER_WITHOUT_ERR_CODE(a9)
INT_HANDLER_WITHOUT_ERR_CODE(aa)
INT_HANDLER_WITHOUT_ERR_CODE(ab)
INT_HANDLER_WITHOUT_ERR_CODE(ac)
INT_HANDLER_WITHOUT_ERR_CODE(ad)
INT_HANDLER_WITHOUT_ERR_CODE(ae)
INT_HANDLER_WITHOUT_ERR_CODE(af)

INT_HANDLER_WITHOUT_ERR_CODE(b0)
INT_HANDLER_WITHOUT_ERR_CODE(b1)
INT_HANDLER_WITHOUT_ERR_CODE(b2)
INT_HANDLER_WITHOUT_ERR_CODE(b3)
INT_HANDLER_WITHOUT_ERR_CODE(b4)
INT_HANDLER_WITHOUT_ERR_CODE(b5)
INT_HANDLER_WITHOUT_ERR_CODE(b6)
INT_HANDLER_WITHOUT_ERR_CODE(b7)
INT_HANDLER_WITHOUT_ERR_CODE(b8)
INT_HANDLER_WITHOUT_ERR_CODE(b9)
INT_HANDLER_WITHOUT_ERR_CODE(ba)
INT_HANDLER_WITHOUT_ERR_CODE(bb)
INT_HANDLER_WITHOUT_ERR_CODE(bc)
INT_HANDLER_WITHOUT_ERR_CODE(bd)
INT_HANDLER_WITHOUT_ERR_CODE(be)
INT_HANDLER_WITHOUT_ERR_CODE(bf)

INT_HANDLER_WITHOUT_ERR_CODE(c0)
INT_HANDLER_WITHOUT_ERR_CODE(c1)
INT_HANDLER_WITHOUT_ERR_CODE(c2)
INT_HANDLER_WITHOUT_ERR_CODE(c3)
INT_HANDLER_WITHOUT_ERR_CODE(c4)
INT_HANDLER_WITHOUT_ERR_CODE(c5)
INT_HANDLER_WITHOUT_ERR_CODE(c6)
INT_HANDLER_WITHOUT_ERR_CODE(c7)
INT_HANDLER_WITHOUT_ERR_CODE(c8)
INT_HANDLER_WITHOUT_ERR_CODE(c9)
INT_HANDLER_WITHOUT_ERR_CODE(ca)
INT_HANDLER_WITHOUT_ERR_CODE(cb)
INT_HANDLER_WITHOUT_ERR_CODE(cc)
INT_HANDLER_WITHOUT_ERR_CODE(cd)
INT_HANDLER_WITHOUT_ERR_CODE(ce)
INT_HANDLER_WITHOUT_ERR_CODE(cf)

INT_HANDLER_WITHOUT_ERR_CODE(d0)
INT_HANDLER_WITHOUT_ERR_CODE(d1)
INT_HANDLER_WITHOUT_ERR_CODE(d2)
INT_HANDLER_WITHOUT_ERR_CODE(d3)
INT_HANDLER_WITHOUT_ERR_CODE(d4)
INT_HANDLER_WITHOUT_ERR_CODE(d5)
INT_HANDLER_WITHOUT_ERR_CODE(d6)
INT_HANDLER_WITHOUT_ERR_CODE(d7)
INT_HANDLER_WITHOUT_ERR_CODE(d8)
INT_HANDLER_WITHOUT_ERR_CODE(d9)
INT_HANDLER_WITHOUT_ERR_CODE(da)
INT_HANDLER_WITHOUT_ERR_CODE(db)
INT_HANDLER_WITHOUT_ERR_CODE(dc)
INT_HANDLER_WITHOUT_ERR_CODE(dd)
INT_HANDLER_WITHOUT_ERR_CODE(de)
INT_HANDLER_WITHOUT_ERR_CODE(df)

INT_HANDLER_WITHOUT_ERR_CODE(e0)
INT_HANDLER_WITHOUT_ERR_CODE(e1)
INT_HANDLER_WITHOUT_ERR_CODE(e2)
INT_HANDLER_WITHOUT_ERR_CODE(e3)
INT_HANDLER_WITHOUT_ERR_CODE(e4)
INT_HANDLER_WITHOUT_ERR_CODE(e5)
INT_HANDLER_WITHOUT_ERR_CODE(e6)
INT_HANDLER_WITHOUT_ERR_CODE(e7)
INT_HANDLER_WITHOUT_ERR_CODE(e8)
INT_HANDLER_WITHOUT_ERR_CODE(e9)
INT_HANDLER_WITHOUT_ERR_CODE(ea)
INT_HANDLER_WITHOUT_ERR_CODE(eb)
INT_HANDLER_WITHOUT_ERR_CODE(ec)
INT_HANDLER_WITHOUT_ERR_CODE(ed)
INT_HANDLER_WITHOUT_ERR_CODE(ee)
INT_HANDLER_WITHOUT_ERR_CODE(ef)

INT_HANDLER_WITHOUT_ERR_CODE(f0)
INT_HANDLER_WITHOUT_ERR_CODE(f1)
INT_HANDLER_WITHOUT_ERR_CODE(f2)
INT_HANDLER_WITHOUT_ERR_CODE(f3)
INT_HANDLER_WITHOUT_ERR_CODE(f4)
INT_HANDLER_WITHOUT_ERR_CODE(f5)
INT_HANDLER_WITHOUT_ERR_CODE(f6)
INT_HANDLER_WITHOUT_ERR_CODE(f7)
INT_HANDLER_WITHOUT_ERR_CODE(f8)
INT_HANDLER_WITHOUT_ERR_CODE(f9)
INT_HANDLER_WITHOUT_ERR_CODE(fa)
INT_HANDLER_WITHOUT_ERR_CODE(fb)
INT_HANDLER_WITHOUT_ERR_CODE(fc)
INT_HANDLER_WITHOUT_ERR_CODE(fd)
INT_HANDLER_WITHOUT_ERR_CODE(fe)
INT_HANDLER_WITHOUT_ERR_CODE(ff)

BEGIN_FUNC(handle_interrupt)
    # determine if we have a kernel exception
    LOAD_IRQ_STACK(rdx)
    movq    16(%rdx), %rdx   # get CS (code segment selector) at time of interrupt
    andq    $3, %rdx         # extract CPL (current privilege level)
    setz    %dl              # DL is now 1 if kernel was interrupted, 0 in case of userland
    cmpq    $0x20, %rcx      # if interrupt vector is below 0x20, we have an exception
    setl    %bl              # BL is now 1 if we have an exception (opposed to IRQ or trap)
    andb    %bl, %dl         # we have a kernel exception if both BL and DL are 1
    jnz     kernel_exception

    # switch to kernel stack
    LOAD_KERNEL_STACK

    # Set the arguments for c_x64_handle_interrupt
    movq    %rcx, %rdi
    movq    %rax, %rsi

    # gtfo to C land, we will not return
    call    c_x64_handle_interrupt
END_FUNC(handle_interrupt)

BEGIN_FUNC(kernel_exception)
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
    /* skip RFLAGS, Error NextIP RSP, TLS_BASE, FaultIP */
    addq $48, %rsp
    popq %r11
    popq %rcx
    LOAD_IRQ_STACK(rsp)
    addq $8, %rsp
    MAYBE_SWAPGS
    iretq

.not_eflags_tf:
#endif /* CONFIG_HARDWARE_DEBUG_API */

#ifdef DEBUG
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
#endif
    jmp halt
END_FUNC(kernel_exception)

# For a fast syscall the RFLAGS have been placed in
# r11, the instruction *AFTER* the syscall is in
# rcx. The current CS and SS have been loaded from
# IA32_LSTAR (along with this code location). Additionally
# the current RFLAGS (after saving) have been masked
# with IA32_FMASK.
BEGIN_FUNC(handle_fastsyscall)
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
    MAYBE_SWAPGS
    LOAD_USER_CONTEXT_OFFSET(4)
    push    %r11
    push    %rdx            # save FaultIP
    subq    $8, %rsp        # skip TLS_BASE
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
