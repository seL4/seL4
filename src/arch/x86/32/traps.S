/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>

/* From <arch/mode/machine/registerset.h> */
#define Error              9
#define NextIP             10
#define CS                 11
#define FLAGS              12
#define ESP                13
#define n_contextRegisters 17

# On kernel entry, ESP points to the end of the thread's registers array.
# Hardware pushes onto the stack SS, ESP, EFLAGS, CS, NextIP and Error,
# leaving the stack pointer pointing to TLS_BASE. The kernel pushes the rest.

#define INT_HANDLER_COMMON(number, error_code) \
.global int_##number;                        \
.type int_##number, %function;               \
int_##number:                                \
    error_code;                              \
    pushl 4(%esp); /* FaultIP := NextIP */   \
    pushl %ebp;                              \
    pushl %edi;                              \
    pushl %esi;                              \
    pushl %edx;                              \
    pushl %ecx;                              \
    pushl %ebx;                              \
    pushl %eax;                              \
    pushl $0;                                \
    movl  $0x##number, %ecx;                 \
    jmp   handle_interrupt;                  \
    .size int_##number, .-int_##number;

#define INT_HANDLER_WITH_ERR_CODE(number) INT_HANDLER_COMMON(number,)
#define INT_HANDLER_WITHOUT_ERR_CODE(number) INT_HANDLER_COMMON(number,pushl $0x0)

#ifdef ENABLE_SMP_SUPPORT
#define SET_KERNEL_STACK_FROM(x)            \
    movl    (x)(%esp), %esp
#else
#define SET_KERNEL_STACK_FROM(x)            \
    leal kernel_stack_alloc + (1 << CONFIG_KERNEL_STACK_BITS) - 4, %esp
#endif

#define SET_KERNEL_STACK SET_KERNEL_STACK_FROM(4 * n_contextRegisters)

# switch to the user data segments as the rest of our entry/exit code assumes that's
# what they will be

#define SET_SELECTOR(selector, value, tmp) \
    movl %selector, %tmp; \
    cmpl $value, %tmp; \
    je 1f; \
    movl $value, %tmp; \
    movl %tmp, %selector; \
    1:

/* user-level selectors from </arch/mode/object/structures.h> */
#define GDT_DS_0    2
#define GDT_DS_3    4
#define GDT_FS      6
#define GDT_GS      7

/* user-level selectors from </arch/object/structures.h> */
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_FS      ((GDT_FS << 3) | 3)
#define SEL_GS      ((GDT_GS << 3) | 3)

#define RESET_SELECTORS(tmp) \
    SET_SELECTOR(ds, SEL_DS_3, tmp) \
    SET_SELECTOR(es, SEL_DS_3, tmp) \
    SET_SELECTOR(fs, SEL_FS, tmp) \
    SET_SELECTOR(gs, SEL_GS, tmp)

.section .text

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

handle_interrupt:
    # Reset segment selectors
    RESET_SELECTORS(edx)

    # determine if we have a kernel exception
    testl   $3, (4 * CS)(%esp)     # extract CPL (current privilege level)
    jz kernel_interrupt

user_interrupt:
    # switch to kernel stack
    SET_KERNEL_STACK

    # Place the arguments on the stack
    pushl   %eax
    pushl   %ecx

    # gtfo to C land, we will not return
    call    c_handle_interrupt

kernel_interrupt:
    cmpl    $0x20, %ecx      # if interrupt vector is below 0x20, we have an exception
    jl kernel_exception
    # Check the current ESP. If this is within the bounds of __idle_thread_start and
    # __idle_thread_end then this is the idle thread. Interrupts from the idle thread
    # are handled like normal interrupts.
    cmpl $__idle_thread_start, %esp
    jb not_idle_thread
    cmpl $__idle_thread_end, %esp
    jae not_idle_thread
    jmp user_interrupt
not_idle_thread:
    # We got an interrupt from the kernel. Call into C to save
    # the IRQ number, then return back to where we were
    pushl %ecx
    call c_nested_interrupt
    addl $4, %esp
    # Disable the interrupt flag so that we do not take any additional interrupts
    andl $~0x200, (4 * FLAGS)(%esp)
    # Return
kernel_int_return:
    addl    $4, %esp
    popl    %eax
    popl    %ebx
    popl    %ecx
    popl    %edx
    popl    %esi
    popl    %edi
    popl    %ebp
    /* Skip FaultIP and error-code. */
    addl    $8, %esp
    iretl

# Handle a kernel exception

BEGIN_FUNC(kernel_exception)
#ifdef CONFIG_HARDWARE_DEBUG_API
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
    movl    (4 * FLAGS)(%esp), %eax
    movl    $(1<<8), %ebx
    testl   %ebx, %eax
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
    andl    $~(1<<8), %eax
    movl    %eax, (4 * FLAGS)(%esp)

    /* Begin popping registers to IRET now. We don't need to consider any
     * unexpected side effects because we are just immediately returning after
     * entering.
     */
    addl    $4, %esp
    popl    %eax
    popl    %ebx
    popl    %ecx
    popl    %edx
    popl    %esi
    popl    %edi
    popl    %ebp
    /* Skip FaultIP and error-code. */
    addl    $8, %esp
    iretl

.not_eflags_tf:
#endif /* CONFIG_HARDWARE_DEBUG_API */

    # prepare debug info
    movl    (4 * Error)(%esp), %eax # EAX contains Error Code
    movl    (4 * NextIP)(%esp), %ebx # EBX contains EIP of the exception generating instruction
    movl    (4 * FLAGS)(%esp), %edx # EDX contains EFLAGS
    leal    (4 * ESP)(%esp), %edi # EDI contains ESP when exception happened

    # call handleKernelException(vector, errcode, EIP, ESP, EFLAGS, CR0, CR2, CR3, CR4)
    movl    %cr4, %esi
    pushl   %esi
    movl    %cr3, %esi
    pushl   %esi
    movl    %cr2, %esi
    pushl   %esi
    movl    %cr0, %esi
    pushl   %esi
    pushl   %edx
    pushl   %edi
    pushl   %ebx
    pushl   %eax
    pushl   %ecx
    call    handleKernelException
    addl    $36, %esp
    # Set EIP in the saved register context to the new IP returned from handleKernelException
    movl    %eax, (4 * NextIP)(%esp)
    jmp     kernel_int_return
END_FUNC(kernel_exception)

# Handle vmexit
# ESP points to the end of an array that just holds the general purpose registers
#ifdef CONFIG_VTX
BEGIN_FUNC(handle_vmexit)
    pushl   %ebp
    pushl   %edi
    pushl   %esi
    pushl   %edx
    pushl   %ecx
    pushl   %ebx
    pushl   %eax

    # Reset segment selectors
    RESET_SELECTORS(ecx)

    # switch to kernel stack
    SET_KERNEL_STACK_FROM(7 * 4)
    # Handle the vmexit, we will not return
    call    c_handle_vmexit
END_FUNC(handle_vmexit)
#endif

# Handle Syscall (coming via sysenter)
# Assume following register contents when called:
#   EAX : syscall number
#   ECX : user ESP
#   EDX : user EIP (pointing to the sysenter instruction)
#   ESP : points to tss.esp0 which points to the end of the thread's registers array

BEGIN_FUNC(handle_syscall)
#ifndef CONFIG_HARDWARE_DEBUG_API
    movl    (%esp), %esp    # ESP := tss.esp0
#endif

    subl    $4, %esp        # skip SS
    pushl   %ecx            # save ESP (passed in ECX)
    pushfl                  # save EFLAGS
    orl     $0x200, (%esp)  # set interrupt bit in saved EFLAGS
    subl    $4, %esp        # skip CS
    pushl   %edx            # NextIP := EDX
    pushl   $-1             # save Error (-1 means we entered via syscall)
    pushl   %edx            # save FaultIP (passed in EDX)
    pushl   %ebp            # save EBP (reply register)
    pushl   %edi            # save EDI (message register)
    pushl   %esi            # save ESI (msgInfo register)
    pushl   %edx            # save EDX (contains FaultIP)
    pushl   %ecx            # save ECX (contains ESP)
    pushl   %ebx            # save EBX (cap/badge register)
    pushl   %eax            # save EAX (syscall number)
    pushl   $0

    # Reset segment selectors
    RESET_SELECTORS(ecx)

    # switch to kernel stack
    SET_KERNEL_STACK

    # Push all the arguments for c_handle_syscall
#ifdef CONFIG_KERNEL_MCS
    pushl   %ebp # reply
#endif
    pushl   %eax # syscall number
    pushl   %esi # msgInfo
    pushl   %ebx # cptr

    # gtfo to C land, we will not return
    call    c_handle_syscall

END_FUNC(handle_syscall)
