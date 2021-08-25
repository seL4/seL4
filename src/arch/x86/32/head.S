/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/* Configuration for MultiBoot, see MultiBoot Specification:
   www.gnu.org/software/grub/manual/multiboot
   We use a flags field of 3, indicating that we want modules loaded on page
   boundaries and access to the memory map information. We do not set bit 16,
   indicating that the structure of the image should be taken from its ELF
   headers. */

#include <config.h>
#include <machine/assembler.h>

.section .phys.text

BEGIN_FUNC(enable_paging)
    # Set PSE (bit 4) to enable 4M pages
    movl %cr4,  %eax
    orl  $0x10, %eax
    movl %eax,  %cr4

    # Load the boot PD (or PDPT) address into CR3
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
    leal boot_stack_top, %esp

    /* Reset EFLAGS register (also disables interrupts etc.) */
    pushl $0
    popf

    /* Push parameters to preserve for calling boot_sys later */
    pushl %esi /* 2nd parameter: multiboot_info_ptr */
    pushl %edi /* 1st parameter: multiboot_magic    */

    /* Initialise boot PD and enable paging */
    call init_boot_pd
    call enable_paging

    popl %edi
    popl %esi

    /* Stop using shared boot stack and get a real stack and move to the top of the stack */
    leal kernel_stack_alloc + (1 << CONFIG_KERNEL_STACK_BITS) - 4, %esp

    pushl %esi /* 2nd parameter: multiboot_info_ptr */
    pushl %edi /* 1st parameter: multiboot_magic    */

    /* Call boot_sys() (takes 2 parameters) and set restore_user_context() as  */
    /* return EIP. This will start the roottask as soon as boot_sys() returns. */
    pushl $restore_user_context
    jmp   boot_sys
END_FUNC(_start)

/* ===== booting up another CPU ===== */
#ifdef ENABLE_SMP_SUPPORT
BEGIN_FUNC(boot_cpu_start)

.code16
    /* Set DS equal to CS and load GDTR register with GDT pointer */
    movw %cs, %ax
    movw %ax, %ds
    lgdt _boot_gdt_ptr - boot_cpu_start

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

    /* Use temporary kernel boot stack pointer */
    leal boot_stack_top, %esp

    /* Reset EFLAGS register (also disables interrupts etc.) */
    pushl $0
    popf

    /* Enable paging */
    call enable_paging

    /* Get index of this cpu, BSP always gets index of zero */
    movl smp_aps_index, %ecx

    /* Stop using shared boot stack and get a real stack and move to the top of the stack */
    leal kernel_stack_alloc, %esp
    inc %ecx
    shll $CONFIG_KERNEL_STACK_BITS, %ecx
    addl %ecx, %esp
    subl $4, %esp

    /* Call boot_node() and set restore_user_context() as return EIP. */
    pushl $restore_user_context
    jmp   boot_node
END_FUNC(boot_cpu_start)

_boot_gdt_ptr:
    .word   (3 * 8) - 1 /* Limit: 3 segments * 8 bytes - 1 byte */
    .long   _boot_gdt   /* Address of boot GDT */

    .align 16
_boot_gdt:
    .quad 0x0000000000000000 /* Null segment */
    .quad 0x00cf9b000000ffff /* 4GB kernel code segment */
    .quad 0x00cf93000000ffff /* 4GB kernel data segment */

.global boot_cpu_end
boot_cpu_end:

#endif /* ENABLE_SMP_SUPPORT */
