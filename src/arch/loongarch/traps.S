/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * 
 * Derived from:
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */


#include <config.h>
#include <machine/assembler.h>
#include <arch/machine/hardware.h>
#include <arch/machine.h>
#include <arch/api/syscall.h>
#include <arch/machine/registerset.h>
#include <util.h>

#define REGBYTES (CONFIG_WORD_SIZE / 8)

.section k_eentry

.global trap_entry
.extern c_handle_syscall
.extern c_handle_exception
.extern c_handle_interrupt

trap_entry:
#ifdef ENABLE_SMP_SUPPORT
    //TODO
#else
    csrwr $t0, LOONGARCH_CSR_KS3 //exchange the value of t0 and LOONGARCH_CSR_KS3
#endif
    st.d $ra, $t0, 0*REGBYTES
    st.d $tp, $t0, 1*REGBYTES
#ifndef ENABLE_SMP_SUPPORT
    st.d $sp, $t0, 2*REGBYTES //store SP
#endif
    st.d $a0, $t0, 3*REGBYTES
    st.d $a1, $t0, 4*REGBYTES
    st.d $a2, $t0, 5*REGBYTES
    st.d $a3, $t0, 6*REGBYTES
    st.d $a4, $t0, 7*REGBYTES
    st.d $a5, $t0, 8*REGBYTES
    st.d $a6, $t0, 9*REGBYTES
    st.d $a7, $t0, 10*REGBYTES
    
    st.d $t1, $t0, 12*REGBYTES
    st.d $t2, $t0, 13*REGBYTES
    st.d $t3, $t0, 14*REGBYTES
    st.d $t4, $t0, 15*REGBYTES
    st.d $t5, $t0, 16*REGBYTES
    st.d $t6, $t0, 17*REGBYTES
    st.d $t7, $t0, 18*REGBYTES
    st.d $t8, $t0, 19*REGBYTES
    st.d $r21, $t0,20*REGBYTES
    
    st.d $s0, $t0, 21*REGBYTES
    st.d $s1, $t0, 22*REGBYTES
    st.d $s2, $t0, 23*REGBYTES
    st.d $s3, $t0, 24*REGBYTES
    st.d $s4, $t0, 25*REGBYTES
    st.d $s5, $t0, 26*REGBYTES
    st.d $s6, $t0, 27*REGBYTES
    st.d $s7, $t0, 28*REGBYTES
    st.d $s8, $t0, 29*REGBYTES
    st.d $fp, $t0, 30*REGBYTES
    /* save t0 value */
#ifdef ENABLE_SMP_SUPPORT
    //TODO
#else
    csrrd $t8, LOONGARCH_CSR_KS3
#endif
    st.d $t8, $t0, 11*REGBYTES

    csrrd $ra, LOONGARCH_CSR_ERA   // save csr_era,i.e., exception PC
    st.d $ra, $t0, 31*REGBYTES

    csrrd $t8, LOONGARCH_CSR_BADV  // save csr_badvaddr
    st.d $t8, $t0, 32*REGBYTES

    csrrd $t8, LOONGARCH_CSR_PRMD  // save csr_prmd, we don`t need to save csr_crmd
    st.d $t8, $t0, 33*REGBYTES

    csrrd $t8, LOONGARCH_CSR_EUEN  // save csr_euen
    st.d $t8, $t0, 34*REGBYTES

    csrrd $t8, LOONGARCH_CSR_ECFG  // save csr_ecfg
    st.d $t8, $t0, 35*REGBYTES

    csrrd $s0, LOONGARCH_CSR_ESTAT // read csr_estat to s0

#ifdef ENABLE_SMP_SUPPORT
    //TODO
#else
    /* Load kernel's stack address */
    la $sp, kernel_stack_alloc
	addi.d $t8, $zero, 1
	slli.d $t8, $t8, CONFIG_KERNEL_STACK_BITS
	add.d $sp, $sp, $t8
#endif
    
    /* Get the is of estat at [12:0]*/
    bstrpick.d $s1, $s0, 12, 0

    /* Check if it's an timer interrupt */
    bnez $s1, interrupt

    /* Get the excode of estat at [21:16]*/
    bstrpick.d $s1, $s0, 21, 16

    li.d $s2, 11
    bne $s1, $s2, exception

handle_syscall:
    /* Set the return address to era + 4 in the case of a system call */
    addi.d $ra, $ra, 4
    /* Save NextIP*/
    st.d $ra, $t0, 36*REGBYTES

#ifdef CONFIG_FASTPATH
//Not tested yet, may triger a problem
    li.d $t3, SYSCALL_CALL
    beq $a7, $t3, c_handle_fastpath_call

    li.d $t3, SYSCALL_REPLY_RECV
#ifdef CONFIG_KERNEL_MCS
//TODO
#endif
//Not tested yet, may triger a problem
    beq $a7, $t3, c_handle_fastpath_reply_recv
#endif
    /* move syscall number to 3rd argument */
    move $a2, $a7
    b c_handle_syscall

exception:
    /* Save NextIP*/
    st.d $ra, $t0, 36*REGBYTES    
    b c_handle_exception


interrupt:
    /* Save NextIP */
    st.d $ra, $t0, 36*REGBYTES
    b c_handle_interrupt
