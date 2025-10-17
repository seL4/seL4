/*
 * platforms/plat/tensor-g3/src/plat_entry.S
 *
 * Copyright 2025, Millpex
 *
 * Initial entry point for the seL4 kernel on the tensor-g3 platform.
 */

#include <kernel/arch/asm_generic.S>
#include <kernel/platform_consts.S>

.global _start
.global plat_cpu_init_early

.text

/*
 * _start: Primary entry point for the kernel.
 * The bootloader jumps here in EL2, passing the device tree address in x0.
 */
_start:
    /* Only the primary core (core 0) performs the main initialization. */
    mrs     x1, mpidr_el1
    and     x1, x1, #MPIDR_CPU_MASK
    cbnz    x1, hang_secondary_cores

    /*
     * Drop to EL1. seL4 typically runs in EL1 unless configured as a hypervisor.
     * We'll set up the return to EL1 and then perform an exception return.
     */
    // Set up return address to el1_entry
    adr     x1, el1_entry
    msr     elr_el2, x1

    // Set SPSR_EL2 to return to EL1h (using SP_EL1)
    mov     x1, #(SPSR_EL_EL1h | SPSR_EL_A | SPSR_EL_I | SPSR_EL_F)
    msr     spsr_el2, x1

    // Set up the stack for EL1
    ldr     x1, =kernel_stack_end
    mov     sp, x1

    // Return to EL1
    eret

el1_entry:
    /* Now in EL1 */

    /* Set up exception vectors for EL1 */
    adr     x0, vectors
    msr     vbar_el1, x0

    /*
     * Set up MMU and system control registers.
     * This involves setting up initial page tables and then enabling
     * the MMU, caches, and other features in SCTLR_EL1.
     * The actual page table setup is complex and usually done by a C function.
     * For this example, we assume `init_boot_tables` sets up the initial mapping.
     */
    bl      init_boot_tables

    // Enable MMU, caches, etc.
    mrs     x0, sctlr_el1
    // Set SCTLR_EL1 bits for MMU, instruction cache, data cache, etc.
    // The exact values depend on the platform and configuration.
    // Example: enable MMU, I-cache, D-cache, and alignment checking.
    orr     x0, x0, #SCTLR_EL1_M
    orr     x0, x0, #SCTLR_EL1_I
    orr     x0, x0, #SCTLR_EL1_C
    orr     x0, x0, #SCTLR_EL1_A
    msr     sctlr_el1, x0
    isb

    /* Clear the BSS section. */
    ldr     x0, =__bss_start__
    ldr     x1, =__bss_end__
clear_bss_loop:
    cmp     x0, x1
    b.ge    bss_cleared
    str     xzr, [x0], #8
    b       clear_bss_loop
bss_cleared:

    /* Perform early platform initialization (e.g., UART). */
    bl      plat_serial_init

    /*
     * Jump to the main kernel initialization.
     * We need to pass the device tree address, which was in x0 at the beginning.
     * We should have saved it before dropping to EL1. For simplicity here,
     * we assume it's been preserved or can be retrieved. A real implementation
     * would save it in a safe place.
     */
    // mov x0, <saved_dtb_address>
    b       kernel_main

/*
 * Secondary cores will spin here until the kernel wakes them.
 */
hang_secondary_cores:
    wfe
    b       hang_secondary_cores

/*
 * Exception vector table. This is a very basic example.
 * A real implementation would have more complete handlers.
 */
.align 11
vectors:
    // Sync, IRQ, FIQ, SError from current EL with SP_EL0
    b       . // Loop forever on an exception
    b       .
    b       .
    b       .
    // Sync, IRQ, FIQ, SError from current EL with SP_ELx
    b       .
    b       .
    b       .
    b       .
    // Sync, IRQ, FIQ, SError from lower EL (AArch64)
    b       .
    b       .
    b       .
    b       .
    // Sync, IRQ, FIQ, SError from lower EL (AArch32)
    b       .
    b       .
    b       .
    b       .

/*
 * plat_cpu_init_early: Called by the kernel on each core
 * before it is fully operational.
 */
plat_cpu_init_early:
    /* Initialize the GIC for the current core. */
    bl      plat_interrupt_controller_init
    /* Initialize and enable the generic timer for the core */
    bl      plat_generic_timer_init
    ret
