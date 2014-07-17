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

#include <machine/assembler.h>

#define MULTIBOOT_HEADER_MAGIC 0x1BADB002
#define MULTIBOOT_HEADER_FLAGS 3

#define IA32_APIC_BASE_MSR 0x01B
#define APIC_ID_OFFSET 0x020

.section .mbh
    /* MultiBoot header */
    .align  4
    .long   MULTIBOOT_HEADER_MAGIC;
    .long   MULTIBOOT_HEADER_FLAGS;
    .long   - MULTIBOOT_HEADER_FLAGS - MULTIBOOT_HEADER_MAGIC;

.section .phys.text

BEGIN_FUNC(enable_paging)
    # Set PSE (bit 4) to enable 4M pages
    movl %cr4,  %eax
    orl  $0x10, %eax
    movl %eax,  %cr4

    # Load the boot PD address into CR3
    leal _boot_pd, %eax
    movl %eax,     %cr3

    # Enable caches by clearing bits 29 and 30 of CR0
    # Enable paging by setting bit 31 of CR0
    movl %cr0,        %eax
    andl $0x9fffffff, %eax
    orl  $0x80000000, %eax
    movl %eax,        %cr0

    # Now that paging is enabled we can enable global pages
    # Must be done in this sequence as per the intel manual
    # Set PGE (bit 7) to enable global pages
    movl %cr4,  %eax
    orl  $0x80, %eax
    movl %eax,  %cr4
    ret
END_FUNC(enable_paging)

/* ===== booting up first CPU ===== */

BEGIN_FUNC(_start)
    /* Assume we are MultiBooted, e.g. by GRUB.
       See MultiBoot Specification: www.gnu.org/software/grub/manual/multiboot
    */

    movl %eax, %edi /* multiboot_magic    */
    movl %ebx, %esi /* multiboot_info_ptr */

    /* Load kernel boot stack pointer */
    leal _boot_stack_top, %esp

    /* Reset EFLAGS register (also disables interrupts etc.) */
    pushl $0
    popf

    /* Load APIC address into EAX */
    movl $IA32_APIC_BASE_MSR, %ecx
    rdmsr
    andl $0xfffff000, %eax
    
    /* Measure APIC/bus frequency */
    pushl %eax
    call apic_measure_freq

    /* Already push parameters for calling boot_sys later */
    pushl %eax /* 3rd parameter: apic_khz */
    pushl %esi /* 2nd parameter: multiboot_info_ptr */
    pushl %edi /* 1st parameter: multiboot_magic    */

    /* Initialise boot PD and enable paging */
    call init_boot_pd
    call enable_paging

    /* Adjust ESP/EBP to their virtual address */
    addl $BASE_OFFSET, %esp
    addl $BASE_OFFSET, %ebp

    /* Call boot_sys() (takes 3 parameters) and set restore_user_context() as  */
    /* return EIP. This will start the roottask as soon as boot_sys() returns. */
    pushl $restore_user_context
    jmp   boot_sys
END_FUNC(_start)

/* ===== booting up another CPU ===== */

BEGIN_FUNC(_boot_cpu_start)

.code16
    /* Set DS equal to CS and load GDTR register with GDT pointer */
    movw %cs, %ax
    movw %ax, %ds
    lgdt _boot_gdt_ptr - _boot_cpu_start

    /* Enable Protected Mode */
    movl %cr0, %eax
    orl  $1,   %eax
    movl %eax, %cr0

    /* Reload CS with a far jump */
    ljmpl $0x08, $1f

.code32
    /* Load DS/ES/SS with kernel data segment selector */
1:  movw $0x10, %ax
    movw %ax,   %ds
    movw %ax,   %es
    movw %ax,   %ss

    /* Load APIC address into EAX */
    movl $IA32_APIC_BASE_MSR, %ecx
    rdmsr
    andl $0xfffff000, %eax
    
    /* Save APIC_ID (cpu_t) into ESP */
    movl APIC_ID_OFFSET(%eax), %esp
    shrl $24, %esp

    /* Get physical top address of 2K stack for this CPU */
    incl %esp
    shll $11, %esp

    /* Reset EFLAGS register (also disables interrupts etc.) */
    pushl $0
    popf

    /* Enable paging */
    call enable_paging

    /* Adjust ESP/EBP to their virtual address */
    addl $BASE_OFFSET, %esp
    addl $BASE_OFFSET, %ebp

    /* Call boot_node() and set restore_user_context() as return EIP. */
    /* This will start the roottask as soon as boot_node() returns.   */
    pushl $restore_user_context
    jmp   boot_node
END_FUNC(_boot_cpu_start)

_boot_gdt_ptr:
    .word   (3 * 8) - 1 /* Limit: 3 segments * 8 bytes - 1 byte */
    .long   _boot_gdt   /* Address of boot GDT */

    .align 16
_boot_gdt:
    .quad 0x0000000000000000 /* Null segment */
    .quad 0x00cf9b000000ffff /* 4GB kernel code segment */
    .quad 0x00cf93000000ffff /* 4GB kernel data segment */

.global _boot_cpu_end
_boot_cpu_end:
