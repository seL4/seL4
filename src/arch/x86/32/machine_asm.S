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

.section .boot.text

BEGIN_FUNC(ia32_install_gdt)
    movl 4(%esp), %eax
    lgdt (%eax)             # load gdtr register with gdt pointer
    movw $0x10, %ax         # load register ax with seg selector for kernel DS
    movw %ax,   %ds
    movw %ax,   %es
    movw %ax,   %ss
    movw $0x0,  %ax
    movw %ax,   %fs
    movw %ax,   %gs
    ljmp $0x08, $1f         # reload kernel CS with a far jump
1:  ret
END_FUNC(ia32_install_gdt)

BEGIN_FUNC(ia32_install_idt)
    movl 4(%esp), %eax
    lidt (%eax)
    ret
END_FUNC(ia32_install_idt)

BEGIN_FUNC(ia32_install_ldt)
    lldt 4(%esp)
    ret
END_FUNC(ia32_install_ldt)

BEGIN_FUNC(ia32_install_tss)
    ltr  4(%esp)
    ret
END_FUNC(ia32_install_tss)


BEGIN_FUNC(ia32_load_fs)
    movl 4(%esp), %eax
    movw %ax, %fs
    ret
END_FUNC(ia32_load_fs)

BEGIN_FUNC(ia32_load_gs)
    movl 4(%esp), %eax
    movw %ax, %gs
    ret
END_FUNC(ia32_load_gs)

BEGIN_FUNC(getCacheLineSize)
    pushl %ebx
    movl  $1,    %eax
    cpuid
    movl  %ebx,  %eax
    shrl  $8,    %eax
    andl  $0xff, %eax
    shll  $3,    %eax
    popl %ebx
    ret
END_FUNC(getCacheLineSize)

.section .text

BEGIN_FUNC(out8)
    movb 8(%esp), %al
    movw 4(%esp), %dx
    outb %al,     %dx
    ret
END_FUNC(out8)

BEGIN_FUNC(out16)
    movw 8(%esp), %ax
    movw 4(%esp), %dx
    outw  %ax,    %dx
    ret
END_FUNC(out16)

BEGIN_FUNC(out32)
    movl 8(%esp), %eax
    movw 4(%esp), %dx
    outl %eax,    %dx
    ret
END_FUNC(out32)

BEGIN_FUNC(in8)
    movw 4(%esp), %dx
    inb  %dx,     %al
    ret
END_FUNC(in8)

BEGIN_FUNC(in16)
    movw 4(%esp), %dx
    inw  %dx,     %ax
    ret
END_FUNC(in16)

BEGIN_FUNC(in32)
    movw 4(%esp), %dx
    inl  %dx,     %eax
    ret
END_FUNC(in32)
