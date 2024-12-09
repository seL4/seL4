/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <util.h>
#include <arch/machine/registerset.h>

#if defined(CONFIG_HAVE_CHERI)
#include <mode/cheri.h>
#endif

.section .boot.text, "ax"
.global _start
.extern init_kernel
.extern kernel_stack_alloc
.extern __global_pointer$
.extern restore_user_context

#if defined(__CHERI_PURE_CAPABILITY__)
# Currently we assume the kernel starts in hybrid mode, and the boot
# initialisation process has to be performed in hybrid.
# cheriTODO: change that if/when the elfloader (and/or OpenSBI) boot
# or jump to the kernel in purecap mode.
.option nocapmode
#endif
/*
 * When SMP is enabled, the elfloader passes the hart ID in a6
 * and logical core ID in a7.
 */
_start:
  fence.i
.option push
.option norelax
1:auipc gp, %pcrel_hi(__global_pointer$)
  addi  gp, gp, %pcrel_lo(1b)
.option pop
  la sp, (kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS))
  csrw sscratch, x0 /* zero sscratch for the init task */

#ifdef CONFIG_ENABLE_SMP_SUPPORT
/* setup the per-core stack */
  mv t0, a7
  slli t0, t0, CONFIG_KERNEL_STACK_BITS
  add  sp, sp, t0
  /* put the stack in sscratch */
  csrw sscratch, sp
#endif

#if defined(CONFIG_HAVE_CHERI)
    MOVE cs0, ca0
    MOVE cs1, ca1
    MOVE cs2, ca2
    MOVE cs3, ca3
    MOVE cs4, ca4
    MOVE cs5, ca5
    MOVE cs6, ca6
    MOVE cs7, ca7

#if defined(__CHERI_PURE_CAPABILITY__)
    # from now on, start purecap run-time mode
#if defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
    modesw
    call _start_purecap
#else
    la              t2, _start_purecap
    li              t1, 1
    CSRR            ct0, ddc
    CSETADDR        ct0, ct0, t2
    SETMODE         ct0, ct0, t1     # set purecap mode on
    jalr.cap        cra, ct0
#endif
.option capmode
#else
    call _start_hybrid
#endif

    MOVE ca0, cs0
    MOVE ca1, cs1
    MOVE ca2, cs2
    MOVE ca3, cs3
    MOVE ca4, cs4
    MOVE ca5, cs5
    MOVE ca6, cs6
    MOVE ca7, cs7
#endif

#if defined(__CHERI_PURE_CAPABILITY__)
    # Construct a bounded stack capability
    li              t2, BIT(CONFIG_KERNEL_STACK_BITS)
    add             sp, sp, t2
    CSRR            ct0, ddc
    CSETADDR        csp, ct0, sp
    CSETBNDS        csp, csp, t2
    CADD            csp, csp, t2
    li              t2, __CHERI_CAP_PERMISSION_PERMIT_STORE__            | \
                        __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ | \
                        __CHERI_CAP_PERMISSION_PERMIT_LOAD__             | \
                        __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__
#if defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
    ori             t2, t2, __CHERI_BW_CAP_PERMISSION_CAPABILITY__
#else
    ori             t2, t2, __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__
#endif
    ANDPERM         csp, csp, t2

    # Construct a valid cgp capability
    # cheriTODO: Bound cgp with proper permissions?
    CSRR            ct1, ddc
    CSETADDR        cgp, ct1, gp

#if defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
    call init_kernel
    tail            restore_user_context
#else
    # Jump to the C kernel
    CLGC            ct0, init_kernel
#ifdef __riscv_xcheri_mode_dependent_jumps
    jalr.cap        cra, ct0
    clgc            ct0, restore_user_context
    jr.cap          ct0
#else
    cjalr           cra, ct0
    clgc            ct0, restore_user_context
    cjr             ct0
#endif
#endif
#else

  /* Call bootstrapping implemented in C with parameters:
   *    a0/x10: user image physical start address
   *    a1/x11: user image physical end address
   *    a2/x12: physical/virtual offset
   *    a3/x13: user image virtual entry address
   *    a4/x14: DTB physical address (0 if there is none)
   *    a5/x15: DTB size (0 if there is none)
   *    a6/x16: hart ID (SMP only, unused on non-SMP)
   *    a7/x17: core ID (SMP only, unused on non-SMP)
   */
  jal init_kernel

  /* Restore the initial thread. Note that the function restore_user_context()
   * could technically also be called at the end of init_kernel() directly,
   * there is no need to return to the assembly code here at all. However, for
   * verification things are a lot easier when init_kernel() is a normal C
   * function that returns. The function restore_user_context() is not a
   * normal C function and thus handled specially in verification, it does
   * highly architecture specific things to exit to user mode.
   */
  la ra, restore_user_context
  jr ra
#endif
