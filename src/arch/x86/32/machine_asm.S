/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/assembler.h>

.section .boot.text

/* user-level selectors from </arch/mode/object/structures.h> */
#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_CS_3    3
#define GDT_DS_3    4
#define GDT_TSS     5
#define GDT_FS      6
#define GDT_GS      7
#define GDT_ENTRIES 8
/* user-level selectors from </arch/object/structures.h> */
#define SEL_DS_0    (GDT_DS_0 << 3)
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_FS      ((GDT_FS << 3) | 3)
#define SEL_GS      ((GDT_GS << 3) | 3)

BEGIN_FUNC(ia32_install_gdt)
    movl 4(%esp), %eax
    lgdt (%eax)             # load gdtr register with gdt pointer
    movw $SEL_DS_3, %ax     # load register ax with seg selector DS
    movw %ax,   %ds
    movw %ax,   %es
    movw $SEL_FS, %ax       # load register ax with seg selector FS
    movw %ax,   %fs
    movw $SEL_GS, %ax       # load register ax with seg selector GS
    movw %ax,   %gs
    movw $SEL_DS_0, %ax     # load register ax with seg selector for kernel DS
    movw %ax,   %ss
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
