/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
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
.extern trap_entry

/*
 * When SMP is enabled, the elfloader passes the hart ID in a4
 * and logical core ID in a5.
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

#if CONFIG_MAX_NUM_NODES > 1
/* setup the per-core stack */
  mv t0, a5
  slli t0, t0, CONFIG_KERNEL_STACK_BITS
  add  sp, sp, t0
  /* put the stack in sscratch */
  csrw sscratch, sp
#endif

  jal init_kernel

  la ra, restore_user_context
  jr ra
