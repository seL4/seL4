/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
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

_start:
.option push
.option norelax
1:auipc gp, %pcrel_hi(__global_pointer$)
  addi  gp, gp, %pcrel_lo(1b)
.option pop

  /* hartid is in a4 */
  li  sp, BIT(CONFIG_KERNEL_STACK_BITS)
  mul sp, sp, a4
  la  x1, (kernel_stack_alloc + (BIT(CONFIG_KERNEL_STACK_BITS)))
  add sp, sp, x1

  csrrw x0, sscratch, a4 /* zero sscratch for the init task */

  /* la sp, (kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS)) */
  jal init_kernel

  la ra, restore_user_context
  jr ra
