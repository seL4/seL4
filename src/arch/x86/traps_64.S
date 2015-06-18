/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <machine/assembler.h>

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

#define INT_HANDLER_WITH_ERR_CODE(number)            \
.global int_##number;                                \
.type   int_##number, %function;                     \
int_##number:                                        \
    push    $0x0;                                    \
    subq    $8, %rsp;                                \
    push    24(%rsp);                                \
    push    %gs;                                     \
    push    %fs;                                     \
    push    $0x0;                                    \
    push    $0x0;                                    \
    push    %r15;                                    \
    push    %r14;                                    \
    push    %r13;                                    \
    push    %r12;                                    \
    push    %r11;                                    \
    push    %r10;                                    \
    push    %r9;                                     \
    push    %r8;                                     \
    push    %rbp;                                    \
    push    %rdi;                                    \
    push    %rsi;                                    \
    push    %rdx;                                    \
    push    %rcx;                                    \
    push    %rbx;                                    \
    push    %rax;                                    \
    movq    $0x##number, %rcx;                       \
    jmp     handle_interrupt;                        \
.size   int_##number, . - int_##number;

#define INT_HANDLER_WITHOUT_ERR_CODE(number)        \
.global int_##number;                               \
.type   int_##number, %function;                    \
int_##number:                                       \
    push    $0x0;                                   \
    push    $0x0;                                   \
    subq    $8, %rsp;                               \
    push    24(%rsp);                               \
    push    %gs;                                    \
    push    %fs;                                    \
    push    $0x0;                                   \
    push    $0x0;                                   \
    push    %r15;                                   \
    push    %r14;                                   \
    push    %r13;                                   \
    push    %r12;                                   \
    push    %r11;                                   \
    push    %r10;                                   \
    push    %r9;                                    \
    push    %r8;                                    \
    push    %rbp;                                   \
    push    %rdi;                                   \
    push    %rsi;                                   \
    push    %rdx;                                   \
    push    %rcx;                                   \
    push    %rbx;                                   \
    push    %rax;                                   \
    movq    $0x##number, %rcx;                      \
    jmp     handle_interrupt;                       \
.size   int_##number, . - int_##number;

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
    movq    0xc0(%rsp), %rdx   # get CS (code segment selector) at time of interrupt
    andq    $3, %rdx         # extract CPL (current privilege level)
    setz    %dl              # DL is now 1 if kernel was interrupted, 0 in case of userland
    cmpq    $0x20, %rcx      # if interrupt vector is below 0x20, we have an exception
    setl    %bl              # BL is now 1 if we have an exception (opposed to IRQ or trap)
    andb    %bl, %dl         # we have a kernel exception if both BL and DL are 1
    jnz     kernel_exception

    # switch to kernel stack
    leaq    kernel_stack_alloc + 4096, %rsp

    # Set the arguments for c_handle_interrupt
    movq    %rcx, %rdi
    movq    %rax, %rsi

    # gtfo to C land, we will not return
    call    c_handle_interrupt
END_FUNC(handle_interrupt)

BEGIN_FUNC(kernel_exception)
#ifdef DEBUG
    movq    %rcx, %rdi
    movq    0xb0(%rsp), %rsi    # error code
    movq    0xb8(%rsp), %rdx    # RIP of the exception 
    movq    %rsp, %rcx
    addq    $0xe0, %rcx         # RSP when exception happed
    movq    0xc8(%rsp), %r8     # RFLAGS
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

BEGIN_FUNC(handle_syscall)
    movq    (%rsp), %rsp    # rsp = tss.rsp0
    subq    $8, %rsp        # skip SS
    push    %rcx            # save rsp
    pushf                   # save RFLAGS
    orq     $0x200, (%rsp)  # set interrupt bit in save RFLAGS
    subq    $8, %rsp        # skip CS
    push    %rdx            # NextRIP
    push    $-1             # save Error (-1 means we entered via syscall)
    push    $0              # padding
    subq    $8, %rsp        # skip TLS_BASE
    push    %rdx            # save FaultRIP
    push    %gs             # save GS
    push    %fs             # save FS
    subq    $16, %rsp       # skip ES, DS
    push    %r15            # save r15 - r8
    push    %r14
    push    %r13
    push    %r12
    push    %r11
    push    %r10
    push    %r9
    push    %r8
    push    %rbp            # save RBP (message register)
    push    %rdi            # save RDI (message register)
    push    %rsi            # save RSI (msgInfo register)
    push    %rdx            # save RDX (FaultRIP)
    push    %rcx            # save RCX (contains RSP)
    push    %rbx            # save RBX (cap/badge register)
    push    %rax            # save RAX (syscall number)

    movl    $0x10, %r10d
    movl    %r10d, %ds

    # switch to kernel stack
    leaq    kernel_stack_alloc + 4096, %rsp

    movq    %rax, %rdi
    movq    %rsi, %rdx
    movq    %rbx, %rsi
    # gtfo to C land, we will not return
    call    c_handle_syscall
END_FUNC(handle_syscall)
