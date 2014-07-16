/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <machine/assembler.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>

#ifndef ALLOW_UNALIGNED_ACCESS
#define ALLOW_UNALIGNED_ACCESS 1
#endif

#define BIT(n) (1 << (n))

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
                        BIT(CONTROL_EE))

/*
 * Entry point of the kernel ELF image.
 * R0-R3 contain parameters that are passed to init_kernel().
 */

.code 32
.section .boot.text, "ax"

BEGIN_FUNC(_start)
    /* Supervisor mode, interrupts disabled */
    ldr r5, =CPSR_SUPERVISOR
    msr cpsr_fc, r5

    /* Initialise CP15 control register */
    mrc p15, 0, r4, c1, c0, 0
    ldr r5, =CR_BITS_SET
    ldr r6, =CR_BITS_CLEAR
    orr r4, r4, r5
    bic r4, r4, r6
    mcr p15, 0, r4, c1, c0, 0

    /*
     * Load kernel stack pointer. Note that in the traps pages, we
     * can load the remapped version of the stack page from the
     * address given by PPTR_KERNEL_STACK_TOP. However, at this
     * point in time, that page is not yet mapped, so we use the
     * original mapping of the page in the image.
     */
    ldr sp, =arm_kernel_stack
    add sp, sp, #(PPTR_KERNEL_STACK_TOP - PPTR_KERNEL_STACK)

    /* Attempt to workaround any known ARM errata. */
    push {r0-r3}
    blx arm_errata
    pop {r0-r3}

    /* Initialise ABORT stack pointer */
    cps #PMODE_ABORT
    ldr sp, =_breakpoint_stack_top
    cps #PMODE_SUPERVISOR

    /* Call bootstrapping implemented in C */
    blx init_kernel

    /* Restore the initial thread */
    ldr r7, =ksCurThread
    ldr sp, [r7]
    add sp, sp, #PT_LR_svc
    ldmdb sp, {r0-lr}^
    rfeia sp
END_FUNC(_start)
