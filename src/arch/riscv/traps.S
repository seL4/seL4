/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>
#include <arch/machine/hardware.h>
#include <arch/api/syscall.h>
#include <arch/machine/registerset.h>
#include <util.h>

#define REGBYTES (CONFIG_WORD_SIZE / 8)

.section .text

.global trap_entry
.extern c_handle_syscall
.extern c_handle_fastpath_reply_recv
.extern c_handle_fastpath_call
.extern c_handle_interrupt
.extern c_handle_exception

trap_entry:

#ifdef ENABLE_SMP_SUPPORT
/* The sscratch contains the stack for the current core */
  csrrw sp, sscratch, sp
/* Now we have a valid kernel stack */
  STORE t0, (-2*REGBYTES)(sp)
  LOAD  t0, (-1*REGBYTES)(sp)
#else
  csrrw t0, sscratch, t0
#endif
  STORE ra, (0*REGBYTES)(t0)
#ifndef ENABLE_SMP_SUPPORT
  STORE sp, (1*REGBYTES)(t0)
#endif
  STORE gp, (2*REGBYTES)(t0)
  STORE tp, (3*REGBYTES)(t0)
  STORE t1, (5*REGBYTES)(t0)
  STORE t2, (6*REGBYTES)(t0)
  STORE s0, (7*REGBYTES)(t0)
  STORE s1, (8*REGBYTES)(t0)
  STORE a0, (9*REGBYTES)(t0)
  STORE a1, (10*REGBYTES)(t0)
  STORE a2, (11*REGBYTES)(t0)
  STORE a3, (12*REGBYTES)(t0)
  STORE a4, (13*REGBYTES)(t0)
  STORE a5, (14*REGBYTES)(t0)
  STORE a6, (15*REGBYTES)(t0)
  STORE a7, (16*REGBYTES)(t0)
  STORE s2, (17*REGBYTES)(t0)
  STORE s3, (18*REGBYTES)(t0)
  STORE s4, (19*REGBYTES)(t0)
  STORE s5, (20*REGBYTES)(t0)
  STORE s6, (21*REGBYTES)(t0)
  STORE s7, (22*REGBYTES)(t0)
  STORE s8, (23*REGBYTES)(t0)
  STORE s9, (24*REGBYTES)(t0)
  STORE s10, (25*REGBYTES)(t0)
  STORE s11, (26*REGBYTES)(t0)
  STORE t3, (27*REGBYTES)(t0)
  STORE t4, (28*REGBYTES)(t0)
  STORE t5, (29*REGBYTES)(t0)
  STORE t6, (30*REGBYTES)(t0)
  /* save t0 value */
#ifdef ENABLE_SMP_SUPPORT
  LOAD  x1, (-2*REGBYTES)(sp)
#else
  csrr  x1, sscratch
#endif
  STORE    x1, (4*REGBYTES)(t0)

  csrr x1, sstatus
  STORE x1, (32*REGBYTES)(t0)

  csrr s0, scause
  STORE s0, (31*REGBYTES)(t0)

.option push
.option norelax
  la gp, __global_pointer$
.option pop

#ifdef ENABLE_SMP_SUPPORT
  /* save the user sp */
  csrr  x1, sscratch
  STORE x1, (1*REGBYTES)(t0)
  /* restore the sscratch */
  csrw  sscratch, sp
#else
  /* Load kernel's stack address */
  la sp, (kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS))
#endif

  /* Save exception PC */
  csrr x1,  sepc
  STORE   x1, (33*REGBYTES)(t0)

  /* Check if it's an interrupt */
  bltz s0, interrupt

  /* ratified priv has value 8 for ecall from U-mode exception */
  li   s4, 8
  bne  s0, s4, exception

handle_syscall:
  /* Set the return address to sepc + 4 in the case of a system/environment call */
  addi x1, x1, 4
  /* Save NextIP */
  STORE   x1, (34*REGBYTES)(t0)

#ifdef CONFIG_FASTPATH
  li t3, SYSCALL_CALL
  bne a7, t3, .Ltest_replyrecv
  j c_handle_fastpath_call

.Ltest_replyrecv:
  li t3, SYSCALL_REPLY_RECV
#ifdef CONFIG_KERNEL_MCS
  /* move reply to 3rd argument */
  mv a2, a6
#endif
  bne a7, t3, .Lslowpath
  j c_handle_fastpath_reply_recv

.Lslowpath:
#endif

  /* move syscall number to 3rd argument */
  mv a2, a7

  j c_handle_syscall

/* Not an interrupt or a syscall */
exception:
  /* Save NextIP */
  STORE   x1, (34*REGBYTES)(t0)
  j c_handle_exception

interrupt:
  /* Save NextIP */
  STORE   x1, (34*REGBYTES)(t0)
  j c_handle_interrupt
