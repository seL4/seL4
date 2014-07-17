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

.section .phys.text

BEGIN_FUNC(out8_phys)
    movb 8(%esp), %al
    movw 4(%esp), %dx
    outb %al,     %dx
    ret
END_FUNC(out8_phys)

BEGIN_FUNC(in8_phys)
    movw 4(%esp), %dx
    inb  %dx,     %al
    ret
END_FUNC(in8_phys)

.section .boot.text

BEGIN_FUNC(ia32_rdmsr_low)
    movl 4(%esp),  %ecx     # MSR register index
    rdmsr                   # 8 bytes output will be in EDX:EAX
    ret
END_FUNC(ia32_rdmsr_low)

BEGIN_FUNC(ia32_rdmsr_high)
    movl 4(%esp),  %ecx     # MSR register index
    rdmsr                   # 8 bytes output will be in EDX:EAX
    movl %edx,     %eax     # Move the high bytes to the return register
    ret
END_FUNC(ia32_rdmsr_high)

BEGIN_FUNC(ia32_wrmsr)
    movl 4(%esp),  %ecx     # MSR register index
    movl 8(%esp),  %edx     # 4 most significant bytes
    movl 12(%esp), %eax     # 4 least significant bytes
    wrmsr
    ret
END_FUNC(ia32_wrmsr)

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

BEGIN_FUNC(read_cr4)
    movl %cr4, %eax
    ret
END_FUNC(read_cr4)

BEGIN_FUNC(write_cr0)
    movl 4(%esp), %eax
    movl %eax,    %cr0
    ret
END_FUNC(write_cr0)

BEGIN_FUNC(write_cr4)
    movl 4(%esp), %eax
    movl %eax,    %cr4
    ret
END_FUNC(write_cr4)

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

BEGIN_FUNC(ia32_cpuid_edx)
    movl 4(%esp), %eax
    movl 8(%esp), %ecx
    pushl %ebx
    cpuid
    popl %ebx
    movl %edx, %eax
    ret
END_FUNC(ia32_cpuid_edx)

.section .text

BEGIN_FUNC(invalidateTLB)
    movl %cr3, %eax
    movl %eax, %cr3
    ret
END_FUNC(invalidateTLB)

BEGIN_FUNC(invalidateTLBentry)
    movl 4(%esp), %eax
    invlpg (%eax)
    ret
END_FUNC(invalidateTLBentry)

BEGIN_FUNC(invalidatePageStructureCache)
    // Force an invalidate by flushing an arbitrary entry
    // from the TLB
    movl $0, %eax
    invlpg (%eax)
    ret
END_FUNC(invalidatePageStructureCache)

BEGIN_FUNC(flushCacheLine)
    movl 4(%esp), %eax
    clflush (%eax)
    ret
END_FUNC(flushCacheLine)

BEGIN_FUNC(getFaultAddr)
    movl %cr2, %eax
    ret
END_FUNC(getFaultAddr)

BEGIN_FUNC(get_current_esp)
    movl %esp, %eax
    ret
END_FUNC(get_current_esp)

BEGIN_FUNC(ia32_wbinvd)
    wbinvd
    ret
END_FUNC(ia32_wbinvd)

BEGIN_FUNC(ia32_mfence)
    mfence
    ret
END_FUNC(ia32_mfence)

BEGIN_FUNC(saveFpuState)
    movl 4(%esp), %eax
    fxsave (%eax)
    ret
END_FUNC(saveFpuState)

BEGIN_FUNC(loadFpuState)
    movl 4(%esp), %eax
    fxrstor (%eax)
    ret
END_FUNC(loadFpuState)

BEGIN_FUNC(resetFpu)
    finit
    ret
END_FUNC(resetFpu)

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
