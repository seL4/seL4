/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>
#include <arch/machine/hardware.h>
#include <arch/api/syscall.h>
#include <arch/machine/registerset.h>
#include <util.h>

.global trap_entry
.extern c_handle_syscall
.extern c_handle_fastpath_reply_recv
.extern c_handle_fastpath_call
.extern c_handle_interrupt
.extern c_handle_exception

.section .text.traps, "ax"
BEGIN_FUNC(trap_entry)

#ifdef ENABLE_SMP_SUPPORT
/* The sscratch contains the stack for the current core */
  CSRRW REGN(sp), SSCRATCH, REGN(sp)
/* Now we have a valid kernel stack */
  STORE REGN(t0), (-2*REGBYTES)(PTRN(sp))
  LOAD  REGN(t0), (-1*REGBYTES)(PTRN(sp))
#else
  CSRRW REGN(t0), SSCRATCH, REGN(t0)
#endif

  STORE REGN(ra), (0*REGBYTES)(PTRN(t0))
#ifndef ENABLE_SMP_SUPPORT
  STORE REGN(sp), (1*REGBYTES)(PTRN(t0))
#endif
  STORE REGN(gp), (2*REGBYTES)(PTRN(t0))
  STORE REGN(tp), (3*REGBYTES)(PTRN(t0))
  STORE REGN(t1), (5*REGBYTES)(PTRN(t0))
  STORE REGN(t2), (6*REGBYTES)(PTRN(t0))
  STORE REGN(s0), (7*REGBYTES)(PTRN(t0))
  STORE REGN(s1), (8*REGBYTES)(PTRN(t0))
  STORE REGN(a0), (9*REGBYTES)(PTRN(t0))
  STORE REGN(a1), (10*REGBYTES)(PTRN(t0))
  STORE REGN(a2), (11*REGBYTES)(PTRN(t0))
  STORE REGN(a3), (12*REGBYTES)(PTRN(t0))
  STORE REGN(a4), (13*REGBYTES)(PTRN(t0))
  STORE REGN(a5), (14*REGBYTES)(PTRN(t0))
  STORE REGN(a6), (15*REGBYTES)(PTRN(t0))
  STORE REGN(a7), (16*REGBYTES)(PTRN(t0))
  STORE REGN(s2), (17*REGBYTES)(PTRN(t0))
  STORE REGN(s3), (18*REGBYTES)(PTRN(t0))
  STORE REGN(s4), (19*REGBYTES)(PTRN(t0))
  STORE REGN(s5), (20*REGBYTES)(PTRN(t0))
  STORE REGN(s6), (21*REGBYTES)(PTRN(t0))
  STORE REGN(s7), (22*REGBYTES)(PTRN(t0))
  STORE REGN(s8), (23*REGBYTES)(PTRN(t0))
  STORE REGN(s9), (24*REGBYTES)(PTRN(t0))
  STORE REGN(s10), (25*REGBYTES)(PTRN(t0))
  STORE REGN(s11), (26*REGBYTES)(PTRN(t0))
  STORE REGN(t3), (27*REGBYTES)(PTRN(t0))
  STORE REGN(t4), (28*REGBYTES)(PTRN(t0))
  STORE REGN(t5), (29*REGBYTES)(PTRN(t0))
  STORE REGN(t6), (30*REGBYTES)(PTRN(t0))
  /* save t0 value */
#ifdef ENABLE_SMP_SUPPORT
  LOAD  REG(1), (-2*REGBYTES)(PTRN((sp))
#else
  CSRR REG(1), SSCRATCH
#endif
  STORE    REG(1), (4*REGBYTES)(PTRN(t0))

  csrr x1, sstatus
  ISTORE x1, (32*REGBYTES)(PTRN(t0))

  csrr s0, scause
  ISTORE s0, (31*REGBYTES)(PTRN(t0))

.option push
.option norelax
#if defined(__CHERI_PURE_CAPABILITY__)
  CLGC REGN(gp), __global_pointer$
#else
  la gp, __global_pointer$
#endif
.option pop

#ifdef ENABLE_SMP_SUPPORT
  /* save the user sp */
  CSRR REG(1), SSCRATCH
  STORE REG(1), (1*REGBYTES)(PTRN(t0))

  /* restore sscratch(c) */
  CSRRW SSCRATCH, REGN(sp)
#else
  /* Load kernel's stack address */
#if defined(__CHERI_PURE_CAPABILITY__)
  CLGC csp, kernel_stack_alloc
  li t1, BIT(CONFIG_KERNEL_STACK_BITS)
  CADD csp, csp, t1
#else
  la sp, (kernel_stack_alloc + BIT(CONFIG_KERNEL_STACK_BITS))
#endif
#endif

  /* Save exception PC */
  /* A mode switch to capmode is required even for hybrid for the standard CHERI-RISC-V
   * but not in purecap mode or Cambridge's CHERI ISAv9 CHERI-RISC-V. MODESW is
   * just nops for those other cases.
   */
  MODESW
  CSRR REG(1), SEPC
  MODESW

  STORE   REG(1), (33*REGBYTES)(PTRN(t0))

  /* Check if it's an interrupt */
  bltz s0, interrupt

  /* ratified priv has value 8 for ecall from U-mode exception */
  li   s4, 8
  bne  s0, s4, exception

handle_syscall:
  /* Set the return address to sepc + 4 in the case of a system/environment call */
  CADDI REG(1), REG(1), 4
  /* Save NextIP */
  STORE   REG(1), (34*REGBYTES)(PTRN(t0))

#ifdef CONFIG_FASTPATH
  li t3, SYSCALL_CALL
  bne a7, t3, .Ltest_replyrecv
#if defined(__CHERI_PURE_CAPABILITY__) && !defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
  CLGC ct0, c_handle_fastpath_call
  jr.cap ct0
#else
  j c_handle_fastpath_call
#endif

.Ltest_replyrecv:
  li t3, SYSCALL_REPLY_RECV
#ifdef CONFIG_KERNEL_MCS
  /* move reply to 3rd argument */
  mv a2, a6
#endif
  bne a7, t3, .Lslowpath
#if defined(__CHERI_PURE_CAPABILITY__) && !defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
  CLGC ct0, c_handle_fastpath_reply_recv
  jr.cap ct0
#else
  j c_handle_fastpath_reply_recv
#endif

.Lslowpath:
#endif

  /* move syscall number to 3rd argument */
  mv a2, a7

#if defined(__CHERI_PURE_CAPABILITY__) && !defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
  CLGC ct0, c_handle_syscall
  jr.cap ct0
#else
  j c_handle_syscall
#endif

/* Not an interrupt or a syscall */
exception:
  /* Save NextIP */
  STORE   REG(1), (34*REGBYTES)(PTRN(t0))
#if defined(__CHERI_PURE_CAPABILITY__) && !defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
  CLGC ct0, c_handle_exception
  jr.cap ct0
#else
  j c_handle_exception
#endif

interrupt:
  /* Save NextIP */
  STORE   REG(1), (34*REGBYTES)(PTRN(t0))
#if defined(__CHERI_PURE_CAPABILITY__) && !defined(CONFIG_ARCH_CHERI_RISCV_V_0_9)
  CLGC ct0, c_handle_interrupt
  jr.cap ct0
#else
  j c_handle_interrupt
#endif
END_FUNC(trap_entry)
