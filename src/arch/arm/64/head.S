/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2021, HENSOLDT Cyber
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>
#include <util.h>

#ifndef ALLOW_UNALIGNED_ACCESS
#define ALLOW_UNALIGNED_ACCESS 1
#endif

#if ALLOW_UNALIGNED_ACCESS
#define CR_ALIGN_SET     0
#define CR_ALIGN_CLEAR   BIT(CONTROL_A)
#else
#define CR_ALIGN_SET     BIT(CONTROL_A)
#define CR_ALIGN_CLEAR   0
#endif

#if !defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined(CONFIG_AARCH64_USER_CACHE_ENABLE)
#define CR_USER_CACHE_OPS_SET (BIT(CONTROL_UCT) | BIT(CONTROL_UCI))
#define CR_USER_CACHE_OPS_CLEAR 0
#else
#define CR_USER_CACHE_OPS_SET 0
#define CR_USER_CACHE_OPS_CLEAR (BIT(CONTROL_UCT) | BIT(CONTROL_UCI))
#endif

#ifndef CONFIG_DEBUG_DISABLE_L1_ICACHE
    #define CR_L1_ICACHE_SET   BIT(CONTROL_I)
    #define CR_L1_ICACHE_CLEAR 0
#else
    #define CR_L1_ICACHE_SET   0
    #define CR_L1_ICACHE_CLEAR BIT(CONTROL_I)
#endif

#ifndef CONFIG_DEBUG_DISABLE_L1_DCACHE
    #define CR_L1_DCACHE_SET   BIT(CONTROL_C)
    #define CR_L1_DCACHE_CLEAR 0
#else
    #define CR_L1_DCACHE_SET   0
    #define CR_L1_DCACHE_CLEAR BIT(CONTROL_C)
#endif

#define CR_BITS_SET    (CR_ALIGN_SET | \
                        CR_L1_ICACHE_SET | \
                        CR_L1_DCACHE_SET | \
                        CR_USER_CACHE_OPS_SET | \
                        BIT(CONTROL_M))

#define CR_BITS_CLEAR  (CR_ALIGN_CLEAR | \
                        CR_L1_ICACHE_CLEAR | \
                        CR_L1_DCACHE_CLEAR | \
                        CR_USER_CACHE_OPS_CLEAR | \
                        BIT(CONTROL_SA0) | \
                        BIT(CONTROL_EE) | \
                        BIT(CONTROL_E0E))

/*
 * Entry point of the kernel ELF image.
 * X0-X5 contain parameters that are passed to init_kernel().
 *
 * Note that for SMP kernel, the tpidr_el1 is used to pass
 * the logical core ID.
 */

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define SCTLR   sctlr_el2
#else
#define SCTLR   sctlr_el1
#endif

#if defined(CONFIG_HAVE_CHERI)
#if defined(__CHERI_PURE_CAPABILITY__)
#define CPACR_CEN_MASK    (0x3 << 18)
#define  CPACR_CEN_TRAP_ALL1  (0x0 << 18) /* Traps from EL0 and EL1 */
#define  CPACR_CEN_TRAP_EL0 (0x1 << 18) /* Traps from EL0 */
#define  CPACR_CEN_TRAP_ALL2  (0x2 << 18) /* Traps from EL0 and EL1 */
#define  CPACR_CEN_TRAP_NONE  (0x3 << 18) /* No traps */
#else
#define CPACR_FPEN_MASK   (0x3 << 20)
#define  CPACR_FPEN_TRAP_ALL1 (0x0 << 20) /* Traps from EL0 and EL1 */
#define  CPACR_FPEN_TRAP_EL0  (0x1 << 20) /* Traps from EL0 */
#define  CPACR_FPEN_TRAP_ALL2 (0x2 << 20) /* Traps from EL0 and EL1 */
#define  CPACR_FPEN_TRAP_NONE (0x3 << 20) /* No traps */
#define CPACR_TTA   (0x1 << 28)
#endif

/* CCTLR_EL0 - Capability Control Register */
#define CCTLR_SBL_MASK    (0x1 << 7) /* Capability sealing by branch and link */
#define CCTLR_PERMVCT_MASK  (0x1 << 6) /* Permit access to CNTVCT w/o System */
#define CCTLR_ADRDPB_MASK (0x1 << 4) /* ADRPD base selection */
#define CCTLR_PCCBO_MASK  (0x1 << 3) /* PCC base offset enable */
#define CCTLR_DDCBO_MASK  (0x1 << 2) /* PCC base offset enable */
/*
 * CCTLR_EL1 - Capability Control Register
 * The rest of the fields mirror CCTLR_EL0
 */
#define CCTLR_EL1_C64E_MASK (0x1 << 5) /* Enable C64 mode upon exception */
#endif

/*
 * Helper to clear ddc and all other general-purpose capability registers,
 * except c0.
 */
#if defined(__CHERI_PURE_CAPABILITY__)
.extern _start_purecap
#endif

/*
 * Initialize morello on a cpu
 */
#if defined(CONFIG_HAVE_CHERI)
 /* This macros is copied from CheriBSD */
.macro morello_cpu_init
#ifdef __CHERI_PURE_CAPABILITY__
  .arch_extension a64c
#endif
  /* Enable Morello instructions at EL0 and EL1 */
  mrs x5, cpacr_el1
  bic x5, x5, CPACR_CEN_MASK
  orr x5, x5, CPACR_CEN_TRAP_NONE
  msr cpacr_el1, x5
  isb

  /*
   * Allow access to CNTVCT_EL0 without PCC System permission and enable
   * capability sealing for branch and link at EL0.
   */
  mrs x5, cctlr_el0
  orr x5, x5, #(CCTLR_PERMVCT_MASK | CCTLR_SBL_MASK)
  msr cctlr_el0, x5

#ifdef __CHERI_PURE_CAPABILITY__
  /*
   * Enable capablity sealing for branch and link at EL1
   * Use PCC/DDC address interpretation.
   * Use DDC as base for adrdp.
   */
  mrs x5, cctlr_el1
  bic x5, x5, #(CCTLR_PCCBO_MASK | CCTLR_DDCBO_MASK | CCTLR_ADRDPB_MASK)
  orr x5, x5, #(CCTLR_SBL_MASK)
  orr x5, x5, #(CCTLR_EL1_C64E_MASK)
  msr cctlr_el1, x5

  /* We assume that we enter here in a64 mode. */
  bx  #4

  .arch_extension c64
#endif
.endmacro
#endif

.section .boot.text, "ax"
BEGIN_FUNC(_start)
#ifdef __CHERI_PURE_CAPABILITY__
  .arch_extension noc64
  .arch_extension a64c
#endif
    /* Save x4 and x5 so we don't clobber it */
    mov     x7, x4
    mov     x8, x5

    /* Make sure interrupts are disabled */
    msr daifset, #DAIFSET_MASK

    /* Initialise sctlr_el1 or sctlr_el2 register */
    msr     spsel, #1
    mrs     x4, SCTLR
    ldr     x19, =CR_BITS_SET
    ldr     x20, =CR_BITS_CLEAR
    orr     x4, x4, x19
    bic     x4, x4, x20
    msr     SCTLR, x4

#ifdef ENABLE_SMP_SUPPORT
    /* tpidr_el1 has the logic ID of the core, starting from 0 */
    mrs     REG(6), tpidr_el1
    /* Set the sp for each core assuming linear indices */
    ldr     REG(5), =BIT(CONFIG_KERNEL_STACK_BITS)
    mul     REG(5), REG(5), REG(6)
    ldr     REG(4), =kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS)
    add     REG(4), REG(4), REG(5)
    mov     REGN(sp), REG(4)
    /* the kernel stack must be 4-KiB aligned since we use the
       lowest 12 bits to store the logical core ID. */
    orr     REG(6), REG(6), REG(4)
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    msr     tpidr_el2, REG(6)
#else
    msr     tpidr_el1, REG(6)
#endif
#else
    ldr    x4, =kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS)
    mov    sp, x4
#endif /* ENABLE_SMP_SUPPORT */

#ifdef __CHERI_PURE_CAPABILITY__
    /* Initialize morello capability ISA and switch to C64 if needed */
    morello_cpu_init

    mrs     c5, DDC
    ldr     x6, =kernel_stack_alloc
    scvalue c5, c5, x6
    ldr     x6, =BIT(CONFIG_KERNEL_STACK_BITS)
    scbnds  c5, c5, x6
    scvalue c5, c5, x4
    mov     csp, c5
#endif
    /* Attempt to workaround any known ARM errata. */
    stp     x0, x1, [REGN(sp), #-(REGSIZE * 2)]!
    stp     x2, x3, [REGN(sp), #-(REGSIZE * 2)]!
    stp     x7, x8, [REGN(sp), #-(REGSIZE * 2)]!


    bl arm_errata

#ifdef __CHERI_PURE_CAPABILITY__
    bl _start_purecap
#endif

    ldp     x4, x5, [REGN(sp)], #(REGSIZE * 2)
    ldp     x2, x3, [REGN(sp)], #(REGSIZE * 2)
    ldp     x0, x1, [REGN(sp)], #(REGSIZE * 2)

    /* Call bootstrapping implemented in C with parameters:
     *  x0: user image physical start address
     *  x1: user image physical end address
     *  x2: physical/virtual offset
     *  x3: user image virtual entry address
     *  x4: DTB physical address (0 if there is none)
     *  x5: DTB size (0 if there is none)
     */
    bl      init_kernel

    /* Restore the initial thread. Note that the function restore_user_context()
     * could technically also be called at the end of init_kernel() directly,
     * there is no need to return to the assembly code here at all. However, for
     * verification things are a lot easier when init_kernel() is a normal C
     * function that returns. The function restore_user_context() is not a
     * normal C function and thus handled specially in verification, it does
     * highly architecture specific things to exit to user mode.
     */
    b restore_user_context

END_FUNC(_start)
