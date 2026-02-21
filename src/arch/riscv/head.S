/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <util.h>

.section .boot.text, "ax"
.global _start
.extern init_kernel
.extern kernel_stack_alloc
.extern __global_pointer$
.extern restore_user_context

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

#if defined(CONFIG_HAVE_CHERI)
  /*
   * sscratchc will be used to hold the per-core kernel's stack
   * on traps. It needs to be a valid capability as the first
   * few trap instructions execute in capability mode and they
   * swap csp with sscratchc as the first instruction then access
   * memory via csp in the following two instructions.
   */
.option push
.option capmode
  modesw.cap
  auipc ct0, 0
  csrw sscratchc, t0
  modesw.int
.option pop
#endif

/* setup the per-core stack */
  mv t0, a7
  slli t0, t0, CONFIG_KERNEL_STACK_BITS
  add  sp, sp, t0
  /* put the stack in sscratch */
  csrw sscratch, sp
#endif

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
