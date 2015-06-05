/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

/* Configuration for MultiBoot, see MultiBoot Specification:
   www.gnu.org/software/grub/manual/multiboot
   We use a flags field of 3, indicating that we want modules loaded on page
   boundaries and access to the memory map information. We do not set bit 16,
   indicating that the structure of the image should be taken from its ELF
   headers. */

#include <config.h>
#include <machine/assembler.h>

#define MULTIBOOT_HEADER_MAGIC 0x1BADB002
#define MULTIBOOT_HEADER_FLAGS 3

#define IA32_EFER_MSR 0xC0000080
#define IA32_APIC_BASE_MSR 0x01B
#define APIC_ID_OFFSET 0x020

.section .mbh
    /* MultiBoot header */
    .align  4
    .long   MULTIBOOT_HEADER_MAGIC;
    .long   MULTIBOOT_HEADER_FLAGS;
    .long   - MULTIBOOT_HEADER_FLAGS - MULTIBOOT_HEADER_MAGIC;

.section .phys.text

.code32

/* We need setup code to run in 32bits. Unfortunately it is difficult to
 * persuade other parts of the C kernel to be compiled as 32bit code
 * Therefore, whilst we would rather write this in C, we have to put
 * the PML4 initialization code here in assembly */

BEGIN_FUNC(print_string)
    movw $0x3f8, %dx
1:
    movb (%ebx), %al
    outb %al, %dx
    incl %ebx
    decl %ecx
    jnz  1b 
    ret
END_FUNC(print_string)

#ifdef CONFIG_HUGE_PAGE
BEGIN_FUNC(huge_page_check)
    movl $0x80000001, %eax
    cpuid
    andl $0x04000000, %edx
    jnz  2f
    movl $huge_page_error_string, %ebx
    movl $huge_page_error_size, %ecx
    call print_string
1:
    hlt
    jmp  1b 
2:
    ret

huge_page_error_string:
    .string "Huge page not supported by the processor"
    .set huge_page_error_size, . - huge_page_error_string 
END_FUNC(huge_page_check)

BEGIN_FUNC(setup_pml4)
    call huge_page_check
    /* Zero the boot pml4 */
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
    /* Zero the boot PDPT */
    movl $boot_pdpt, %edi
    movl $1024, %ecx
1:
    movl %edx, (%edi)
    addl $4, %edi
    loop 1b
    /* Set the first 4 entries in the PDPT to the first
     * 4gb of memory */
    movl $boot_pdpt, %edi
    movl $0x87, %edx
    movl $4, %ecx
    movl $0, %ebx
1:
    movl %edx, (%edi)
    movl %ebx, 4(%edi)
    addl $0x40000000, %edx
    jnc 2f
    /* Carry occured, need to increase the high part
     * of the address */
    incl %ebx
2:
    addl $8, %edi
    loop 1b
    /* Set the second highest entry in the PDPT to also map to the
     * first part of memory. This is our actual kernel window */
    movl $boot_pdpt, %edi
    movl $0x87, %edx
    movl %edx, 4080(%edi)
    /* Put the PDPTs into the PML4 twice
     * Once to create a 1-to-1 mapping, and once
     * to create the higher kernel window */
    movl $boot_pml4, %edi
    movl $boot_pdpt, %edx
    orl $0x7, %edx
    movl %edx, (%edi)
    movl %edx, 4088(%edi)
    /* done */
    ret
END_FUNC(setup_pml4)

#else

BEGIN_FUNC(setup_pml4)
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

    /* map first 4GiB into the _boot_pd */
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

#endif

BEGIN_FUNC(invpcid_check)
    movl $0x7, %eax
    xorl %ecx, %ecx
    cpuid
    andl $0x400, %ebx
    jnz   2f
    movl  $invpcid_error_string, %ebx
    movl  $invpcid_error_size, %ecx
    call  print_string
1:
    hlt
    jmp   1b
2:
    ret

invpcid_error_string:
    .string "INVPCID instruction not supported by the processor"
    .set invpcid_error_size, . - invpcid_error_string
END_FUNC(invpcid_check)

/* if fsgsbase instructions are supported, we enable them. */
BEGIN_FUNC(fsgsbase_enable)
#ifdef CONFIG_FSGSBASE_INST
    movl $0x7, %eax
    xorl %ecx, %ecx
    cpuid
    andl $1, %ebx
    jnz  2f
    movl $fsgsbase_error_string, %ebx
    movl $fsgsbase_error_size, %ecx
    call print_string
1:
    hlt
    jmp 1b
2:
    movl %cr4, %eax
    /* enable the bit in CR4 */
    orl  $0x10000, %eax
    movl %eax, %cr4
    ret

fsgsbase_error_string:
    .string "fsgsbase instructions not supported by the processor"
    .set fsgsbase_error_size, . - fsgsbase_error_string
#else
    ret
#endif
END_FUNC(fsgsbase_enable)

BEGIN_FUNC(enable_x64_mode)
    call invpcid_check 
    /* put base pointer in cr3 */
    movl $boot_pml4, %eax
    movl %eax, %cr3
    /* Set PAE (bit 5), as this is required before switching to long mode */
    movl %cr4, %eax
    orl $0x20, %eax
    movl %eax, %cr4
    /* Set LME (bit 8) in the extended feature MSR */
    movl $IA32_EFER_MSR, %ecx
    rdmsr
    orl $0x100, %eax
    wrmsr
    /* Set PG (bit 31) of cr0 to enable paging */
    movl %cr0, %eax
    orl $0x80000000, %eax
    movl %eax, %cr0
    /* enable PCID (bit 17), must be done in long mode */
    movl %cr4, %eax
    orl  $0x20000, %eax
    movl %eax, %cr4
    ret
END_FUNC(enable_x64_mode)

BEGIN_FUNC(_start)
    /* Assume we are MultiBooted, e.g. by GRUB.
       See MultiBoot Specification: www.gnu.org/software/grub/manual/multiboot
    */
    movl %eax, %edi /* multiboot_magic    */
    movl %ebx, %esi /* multiboot_info_ptr */

    /* Load kernel boot stack pointer */
    leal boot_stack_top, %esp

    /* Reset EFLAGS register (also disables interrupts etc.) */
    pushl $0
    popf

    /* Already push parameters for calling boot_sys later. Push
     * them as 8 byte values so we can easily pop later */
    pushl $0
    pushl %esi /* 2nd parameter: multiboot_info_ptr */
    pushl $0
    pushl %edi /* 1st parameter: multiboot_magic    */

    /* make sure paging (bit 31) is off */
    movl %cr0, %eax
    andl $0x7fffffff, %eax
    movl %eax, %cr0

    call fsgsbase_enable
    /* Initialize boot PML4 and switch to long mode */
    call setup_pml4
    call enable_x64_mode
    lgdt _gdt64_ptr
    /* reload CS with long bit to enable long mode */
    ljmp $8, $_start64
END_FUNC(_start)

_gdt64_ptr:
    .word (3 * 8) - 1
    .long _gdt64

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

#ifndef CONFIG_HUGE_PAGE

.align 4096
_boot_pd:
    .fill 16384

#endif

.code64
BEGIN_FUNC(_start64)
    /* Leave phys code behind and jump to the high kernel virtual address */
    movabs $_entry_64, %rax
    jmp *%rax
END_FUNC(_start64)

.section .boot.text

BEGIN_FUNC(_entry_64)
    /* Update our stack pointer */
    movq $0xffffffff80000000, %rax
    addq %rax, %rsp
    addq %rax, %rbp

    /* Pop the multiboot parameters off */
    pop %rdi
    pop %rsi
    movabs $restore_user_context, %rax
    push %rax
    jmp boot_sys
END_FUNC(_entry_64)

.section .phys.text

.code16
BEGIN_FUNC(boot_cpu_start)
1:
    jmp 1b
END_FUNC(boot_cpu_start)

.global boot_cpu_end
boot_cpu_end:
