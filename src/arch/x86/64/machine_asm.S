/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/assembler.h>

/*
 * Calling convention for x86-64
 * For integer type, the next available register of the sequence %rdi,
 * %rsi, %rdx, %rcx, %r8 and %r9 are used for passing values. %rax and
 * %rdx are used for returning integer values. Pointers are considered
 * as integer type according to the ABI.
 */

.section .boot.text

.section .text

BEGIN_FUNC(out8)                        # port, value
    movq    %rdi, %rdx
    movq    %rsi, %rax
    outb    %al, %dx
    ret
END_FUNC(out8)

BEGIN_FUNC(out16)
    movq    %rdi, %rdx
    movq    %rsi, %rax
    outw    %ax, %dx
    ret
END_FUNC(out16)

BEGIN_FUNC(out32)
    movq    %rdi, %rdx
    movq    %rsi, %rax
    outl    %eax, %dx
    ret
END_FUNC(out32)

BEGIN_FUNC(in8)
    movq    $0, %rax
    movq    %rdi, %rdx
    inb     %dx, %al
    ret
END_FUNC(in8)

BEGIN_FUNC(in16)
    movq    $0, %rax
    movq    %rdi, %rdx
    inw     %dx, %ax
    ret
END_FUNC(in16)

BEGIN_FUNC(in32)
    movq    $0, %rax
    movq    %rdi, %rdx
    inl     %dx, %eax
    ret
END_FUNC(in32)

BEGIN_FUNC(getCacheLineSize)
    push    %rbx
    movl    $1, %eax
    cpuid
    movl    %ebx, %eax
    shrl    $8, %eax
    andl    $0xff, %eax
    shll    $3, %eax
    pop     %rbx
    ret
END_FUNC(getCacheLineSize)

BEGIN_FUNC(x64_install_gdt)
    lgdt    (%rdi)          # load gdtr with gdt pointer
    movw    $0x10, %ax      # load register ax with seg selector
    movw    %ax, %ds
    movw    %ax, %es
    movw    %ax, %ss
    movw    $0x0, %ax
    movw    %ax, %fs
    movw    %ax, %gs
    ret
END_FUNC(x64_install_gdt)

BEGIN_FUNC(x64_install_idt)
    lidt    (%rdi)
    ret
END_FUNC(x64_install_idt)

BEGIN_FUNC(x64_install_ldt)
    lldt    %di
    ret
END_FUNC(x64_install_ldt)

BEGIN_FUNC(x64_install_tss)
    ltr     %di
    ret
END_FUNC(x64_install_tss)
