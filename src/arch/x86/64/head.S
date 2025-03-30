/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/* The kernel expects to be booted by a Multiboot compliant bootloader.
 * See Multiboot specifications:
 * www.gnu.org/software/grub/manual/multiboot
 * www.gnu.org/software/grub/manual/multiboot2
 *
 * The multiboot header's flags field is set to 3, indicating that we want
 * modules loaded on page boundaries, access to memory map information, and
 * information about the video mode table. Bit 16 of the multiboot header is
 * not set, indicating that the structure of the image should be taken from its
 * ELF headers.
 *
 * When the bootloader jumps to the entry point it is not in long mode and
 * 64-bit instructions are not accessible (Multiboot 1 does not have support
 * for this). While in protected mode, setup including the initialisation of
 * 64-bit paging structures is done before manually enabling long mode and
 * continuing. */

#include <config.h>
#include <machine/assembler.h>

#define IA32_EFER_MSR 0xC0000080
#define IA32_APIC_BASE_MSR 0x01B
#define APIC_ID_OFFSET 0x020

.section .phys.text

.code32

BEGIN_FUNC(print_string)
1:
    movw $0x3f8+5, %dx
2:
    inb  (%dx), %al
    test $0x20, %al
    jz   2b
    sub  $5, %dx
    movb (%ebx), %al
    outb %al, %dx
    incl %ebx
    decl %ecx
    jnz  1b
    ret
END_FUNC(print_string)

BEGIN_FUNC(hang)
1:
    hlt
    jmp  1b
END_FUNC(hang)

#ifdef CONFIG_HUGE_PAGE
BEGIN_FUNC(huge_page_check)
    movl $0x80000001, %eax
    cpuid
    andl $0x04000000, %edx
    jz   1f
    ret
1:
    movl $huge_page_error_string, %ebx
    movl $huge_page_error_size, %ecx
    call print_string
    call hang

huge_page_error_string:
    .string "Huge page not supported by the processor\r\n"
    .set huge_page_error_size, . - huge_page_error_string
END_FUNC(huge_page_check)
#endif /* CONFIG_HUGE_PAGE */

BEGIN_FUNC(setup_pml4)
#ifdef CONFIG_HUGE_PAGE
    call huge_page_check
#endif /* CONFIG_HUGE_PAGE */
    movl %cr0, %eax
    andl $0x7fffffff, %eax
    movl %eax, %cr0

    movl $boot_pml4, %edi
    movl $0x0, %edx
    movl $1024, %ecx
1:
    movl %edx, (%edi)
    addl $4, %edi
    loop 1b

    movl $boot_pdpt, %edi
    movl $1024, %ecx
1:
    movl %edx, (%edi)
    addl $4, %edi
    loop 1b

    movl $boot_pml4, %edi
    movl $boot_pdpt, %ecx
    orl  $0x7, %ecx
    movl %ecx, (%edi)
    movl %ecx, 0x800(%edi)
    movl %ecx, 4088(%edi)

    movl $_boot_pd, %ecx
    orl  $0x7, %ecx
    movl $boot_pdpt, %edi
    movl %ecx, (%edi)
    movl %ecx, 4080(%edi)
    addl $0x1000, %ecx
    movl %ecx, 8(%edi)
    addl $0x1000, %ecx
    movl %ecx, 16(%edi)
    addl $0x1000, %ecx
    movl %ecx, 24(%edi)

    /* Map first 4GiB into the _boot_pd. */
    movl $_boot_pd, %edi
    movl $2048, %ecx
    movl $0x87, %edx
2:
    movl %edx, (%edi)
    addl $0x200000, %edx
    addl $8, %edi
    loop 2b
    ret
END_FUNC(setup_pml4)

#ifdef CONFIG_SUPPORT_PCID
BEGIN_FUNC(pcid_check)
    movl $0x1, %eax
    xorl %ecx, %ecx
    cpuid
    andl $0x20000, %ecx
    jz   1f
    ret
1:
    movl $pcid_error_string, %ebx
    movl $pcid_error_size, %ecx
    call print_string
    call hang

pcid_error_string:
    .string "PCIDs not supported by the processor\r\n"
    .set pcid_error_size, . - pcid_error_string
END_FUNC(pcid_check)

BEGIN_FUNC(invpcid_check)
    movl $0x7, %eax
    xorl %ecx, %ecx
    cpuid
    andl $0x400, %ebx
    jz   1f
    ret
1:
    movl  $invpcid_error_string, %ebx
    movl  $invpcid_error_size, %ecx
    call  print_string
    call  hang

invpcid_error_string:
    .string "INVPCID instruction not supported by the processor\r\n"
    .set invpcid_error_size, . - invpcid_error_string
END_FUNC(invpcid_check)
#endif /* CONFIG_SUPPORT_PCID */

BEGIN_FUNC(syscall_check)
    movl $0x80000001, %eax
    xorl %ecx, %ecx
    cpuid
    andl $0x20000000, %edx
    jz   1f
    ret
1:
    movl  $syscall_error_string, %ebx
    movl  $syscall_error_size, %ecx
    call  print_string
    call  hang

syscall_error_string:
    .string "SYSCALL/SYSRET instruction not supported by the processor\r\n"
    .set syscall_error_size, . - syscall_error_string
END_FUNC(syscall_check)

#ifdef CONFIG_FSGSBASE_INST
BEGIN_FUNC(fsgsbase_enable)
    movl $0x7, %eax
    xorl %ecx, %ecx
    cpuid
    andl $1, %ebx
    jz   1f
    movl %cr4, %eax
    /* Enable the bit in cr4. */
    orl  $0x10000, %eax
    movl %eax, %cr4
    ret
1:
    movl $fsgsbase_error_string, %ebx
    movl $fsgsbase_error_size, %ecx
    call print_string
    call hang

fsgsbase_error_string:
    .string "fsgsbase instructions not supported by the processor\r\n"
    .set fsgsbase_error_size, . - fsgsbase_error_string
END_FUNC(fsgsbase_enable)
#endif /* CONFIG_FSGSBASE_INST */

#ifdef CONFIG_SYSCALL
BEGIN_FUNC(syscall_enable)
    call syscall_check
    /* Set SCE (bit 0) in the extended feature MSR. */
    movl $IA32_EFER_MSR, %ecx
    rdmsr
    orl $0x1, %eax
    wrmsr
    ret
END_FUNC(syscall_enable)
#endif /* CONFIG_SYSCALL */

BEGIN_FUNC(enable_x64_mode)
#ifdef CONFIG_SUPPORT_PCID
    call pcid_check
    call invpcid_check
#endif
    /* Put base pointer in cr3. */
    movl $boot_pml4, %eax
    movl %eax, %cr3
    /* Set PAE (bit 5), as this is required before switching to long mode. */
    movl %cr4, %eax
    orl $0x20, %eax
    movl %eax, %cr4
    /* Set LME (bit 8) in the extended feature MSR. */
    movl $IA32_EFER_MSR, %ecx
    rdmsr
    orl $0x100, %eax
    wrmsr
    /* Set PG (bit 31) of cr0 to enable paging. */
    movl %cr0, %eax
    orl $0x80000000, %eax
    movl %eax, %cr0
#ifdef CONFIG_SUPPORT_PCID
    /* Enable PCID (bit 17), must be done in long mode. */
    movl %cr4, %eax
    orl  $0x20000, %eax
    movl %eax, %cr4
#endif
    ret
END_FUNC(enable_x64_mode)

BEGIN_FUNC(common_init)
    /* Disable paging. */
    movl %cr0, %eax
    andl $0x7fffffff, %eax
    movl %eax, %cr0

#ifdef CONFIG_FSGSBASE_INST
    call fsgsbase_enable
#endif /* CONFIG_FSGSBASE_INST */

    /* Initialize boot PML4 and switch to long mode. */
    call setup_pml4
    call enable_x64_mode
    lgdt _gdt64_ptr

#ifdef CONFIG_SYSCALL
    call syscall_enable
#endif

    ret
END_FUNC(common_init)

BEGIN_FUNC(_start)
    /* Assume we are MultiBooted, e.g. by GRUB.
     * While not immediately checked, the magic number is checked prior to 
     * Multiboot dependent operations. */
    movl %eax, %edi /* multiboot_magic    */
    movl %ebx, %esi /* multiboot_info_ptr */

    /* Load kernel boot stack pointer. */
    leal boot_stack_top, %esp

    /* Reset EFLAGS register (also disables interrupts etc.). */
    pushl $0
    popf

    /* Already push parameters for calling boot_sys later. Push
     * them as 8 byte values so we can easily pop later. */
    pushl $0
    pushl %esi /* 2nd parameter: multiboot_info_ptr */
    pushl $0
    pushl %edi /* 1st parameter: multiboot_magic    */

    call common_init

    /* Reload CS with long bit to enable long mode. */
    ljmp $8, $_start64
END_FUNC(_start)

.code64
BEGIN_FUNC(_start64)
    /* Leave phys code behind and jump to the high kernel virtual address. */
    movabs $_entry_64, %rax
    jmp *%rax
END_FUNC(_start64)


.section .phys.data
.align 16
_gdt64:
    .quad 0x0000000000000000
    .word   0
    .word   0
    .byte   0
    .byte   0x98
    .byte   0x20
    .byte   0
    .word   0
    .word   0
    .byte   0
    .byte   0x90
    .byte   0
    .byte   0


_gdt64_ptr:
    .word (3 * 8) - 1
    .long _gdt64

.section .phys.bss
.align 4096
_boot_pd:
    .fill 16384

.section .boot.text

BEGIN_FUNC(_entry_64)
    /* Update our stack pointer. */
    movq $0xffffffff80000000, %rax
    addq %rax, %rsp
    addq %rax, %rbp

    /* Pop the multiboot parameters off. */
    pop %rdi
    pop %rsi

    /* Load our real kernel stack. */
    leaq kernel_stack_alloc + (1 << CONFIG_KERNEL_STACK_BITS), %rsp

    movabs $restore_user_context, %rax
    push %rax
    jmp boot_sys
END_FUNC(_entry_64)

.section .phys.text

#ifdef ENABLE_SMP_SUPPORT

BEGIN_FUNC(boot_cpu_start)
.code16
    /* Set DS equal to CS and load GDTR register with GDT pointer. */
    movw %cs, %ax
    movw %ax, %ds
    lgdt _boot_gdt_ptr - boot_cpu_start

    /* Enable protected mode. */
    movl %cr0, %eax
    orl  $1,   %eax
    movl %eax, %cr0

    /* Reload CS with a far jump. */
    ljmpl $0x08, $1f

.code32
1:
    /* Load DS/ES/SS with kernel data segment selector. */
    movw $0x10, %ax
    movw %ax,   %ds
    movw %ax,   %es
    movw %ax,   %ss

    /* Use temporary kernel boot stack pointer. */
    leal boot_stack_top, %esp

    /* Reset EFLAGS register (also disables interrupts etc.). */
    pushl $0
    popf

    call common_init

    /* Reload CS with long bit to enable long mode. */
    ljmp $8, $_start_ap64
    jmp 1b
END_FUNC(boot_cpu_start)

.code64
BEGIN_FUNC(_start_ap64)
    /* Leave phys code behind and jump to the high kernel virtual address. */
    movabs $_entry_ap64, %rax
    jmp *%rax
END_FUNC(_start_ap64)

_boot_gdt_ptr:
    .word   (3 * 8) - 1 /* Limit: 3 segments * 8 bytes - 1 byte */
    .long   _boot_gdt   /* Address of boot GDT */

/* GDT for getting us through 32-bit protected mode. */
    .align 16
_boot_gdt:
    .quad 0x0000000000000000 /* Null segment */
    .quad 0x00cf9b000000ffff /* 4GB kernel code segment */
    .quad 0x00cf93000000ffff /* 4GB kernel data segment */

.global boot_cpu_end
boot_cpu_end:

.section .boot.text

BEGIN_FUNC(_entry_ap64)
    /* Get the index of this cpu. */
    movq smp_aps_index, %rcx

    /* Switch to a real kernel stack. */
    leaq kernel_stack_alloc, %rsp
    inc %rcx
    shlq $CONFIG_KERNEL_STACK_BITS, %rcx
    addq %rcx, %rsp

    movabs $restore_user_context, %rax
    push %rax
    jmp boot_node
END_FUNC(_entry_ap64)

#endif /* ENABLE_SMP_SUPPORT */
