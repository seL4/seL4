/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, HENSOLDT Cyber
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
#define CR_ALIGN_SET     BIT(CONTROL_U)
#define CR_ALIGN_CLEAR   BIT(CONTROL_A)
#else
#define CR_ALIGN_SET     BIT(CONTROL_A)
#define CR_ALIGN_CLEAR   BIT(CONTROL_U)
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

#ifndef CONFIG_DEBUG_DISABLE_BRANCH_PREDICTION
    #define CR_BRANCH_PREDICTION_SET   BIT(CONTROL_Z)
    #define CR_BRANCH_PREDICTION_CLEAR 0
#else
    #define CR_BRANCH_PREDICTION_SET   0
    #define CR_BRANCH_PREDICTION_CLEAR BIT(CONTROL_Z)
#endif

#define CR_BITS_SET    (CR_ALIGN_SET | \
                        CR_L1_ICACHE_SET | \
                        CR_L1_DCACHE_SET | \
                        BIT(CONTROL_M) | \
                        CR_BRANCH_PREDICTION_SET | \
                        BIT(CONTROL_V) | \
                        BIT(CONTROL_XP))

#define CR_BITS_CLEAR  (CR_ALIGN_CLEAR | \
                        CR_L1_ICACHE_CLEAR | \
                        CR_L1_DCACHE_CLEAR | \
                        CR_BRANCH_PREDICTION_CLEAR | \
                        BIT(CONTROL_B) | \
                        BIT(CONTROL_S) | \
                        BIT(CONTROL_R) | \
                        BIT(CONTROL_VE) | \
                        BIT(CONTROL_RR) | \
                        BIT(CONTROL_EE) | \
                        BIT(CONTROL_TRE) | \
                        BIT(CONTROL_AP))

/*
 * Entry point of the kernel ELF image.
 * R0-R3 contain parameters that are passed to init_kernel(),
 * and we put arguments 5 and 6 (DTB address/size) in r7 and r8.
 */

.code 32
.section .boot.text, "ax"
BEGIN_FUNC(_start)
    /*
     * Get the dtb and dtb size from the elfloader stack. Do this first because
     * sp might change when we switch to supervisor mode.
     */
    pop {r7, r8}

    /* Supervisor/hypervisor mode, interrupts disabled */
    ldr r5, =CPSR_KERNEL
    msr cpsr_fc, r5

    /* Initialise CP15 control register */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    mrc p15, 4, r4, c1, c0, 0
#else
    mrc p15, 0, r4, c1, c0, 0
#endif
    ldr r5, =CR_BITS_SET
    ldr r6, =CR_BITS_CLEAR
    orr r4, r4, r5
    bic r4, r4, r6
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    mcr p15, 4, r4, c1, c0, 0

    /* Initialise vector base */
    ldr r4, =PPTR_VECTOR_TABLE
    mcr p15, 4, r4, c12, c0, 0
#else
    mcr p15, 0, r4, c1, c0, 0
#endif

#if defined(CONFIG_ARM_CORTEX_A9) && defined(CONFIG_ENABLE_A9_PREFETCHER)
    /* Set bit 2 in the ACTLR, which on the cortex-a9 is the l1 prefetch enable
     * bit. See section 4.3.10 of the Cortex-A9 Technical Reference Manual */
    mrc p15, 0, r4, c1, c0, 1
    ldr r5, =BIT(2)
    orr r4, r4, r5
    mcr p15, 0, r4, c1, c0, 1
#endif

#if defined(CONFIG_PLAT_HIKEY)
    /* Prefetcher configuration */
   mrrc p15, 0, r4, r5, c15
   ldr r6, =PREFETCHER_MASK
   bic r4, r4, r6
   ldr r6, =PREFETCHER
   orr r4, r4, r6
   mcrr p15, 0, r4, r5, c15
#endif

     /* Load kernel stack pointer
      * On ARM SMP, kernel_stack_alloc is indexed by CPU ID
      * to get different stacks for each core
      */
    ldr sp, =kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS)

#ifdef ENABLE_SMP_SUPPORT
    /*
     * Read MPIDR in r4
     * See ARM Referce Manual (ARMv7-A and ARMv7-R edition), Section B4.1.106
     * for more details about MPIDR register.
     */
    mrc p15, 0, r4, c0, c0, 5
    and r4, r4, #0xff
    /* Set the sp for each core assuming linear indices */
    ldr     r5, =BIT(CONFIG_KERNEL_STACK_BITS)
    mul     r5, r4
    add     sp, sp, r5
#endif /* ENABLE_SMP_SUPPORT */

    /* Attempt to workaround any known ARM errata. */
    push {r0-r3,r7-r8}
    blx arm_errata
    pop {r0-r3,r7-r8}

    /* Hyp kernel always run in Hyp mode. */
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    /* Initialise ABORT stack pointer */
    cps #PMODE_ABORT
    ldr sp, =_breakpoint_stack_top
    cps #PMODE_SUPERVISOR
#endif

    /* Put the DTB address back on the new stack for init_kernel. */
    push {r7, r8}

    /* Call bootstrapping implemented in C with parameters:
     *   r0: user image physical start address
     *   r1: user image physical end address
     *   r2: physical/virtual offset
     *   r3: user image virtual entry address
     *   sp[0]: DTB physical address (0 if there is none)
     *   sp[1]: DTB size (0 if there is none)
     */
    blx init_kernel

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
