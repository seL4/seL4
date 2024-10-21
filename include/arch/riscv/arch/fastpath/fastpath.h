/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <linker.h>
#include <api/types.h>
#include <api/syscall.h>
#include <api/types.h>
#include <smp/lock.h>
#include <arch/machine/hardware.h>
#include <machine/fpu.h>

void slowpath(syscall_t syscall)
NORETURN;

static inline
void fastpath_call(word_t cptr, word_t r_msgInfo)
NORETURN;

static inline
#ifdef CONFIG_KERNEL_MCS
void fastpath_reply_recv(word_t cptr, word_t r_msgInfo, word_t reply)
#else
void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
#endif
NORETURN;

/* Use macros to not break verification */
#define endpoint_ptr_get_epQueue_tail_fp(ep_ptr) TCB_PTR(endpoint_ptr_get_epQueue_tail(ep_ptr))
#define cap_vtable_cap_get_vspace_root_fp(vtable_cap) PTE_PTR(cap_page_table_cap_get_capPTBasePtr(vtable_cap))

static inline void FORCE_INLINE switchToThread_fp(tcb_t *thread, pte_t *vroot, pte_t stored_hw_asid)
{
    asid_t asid = (asid_t)(stored_hw_asid.words[0]);

    setVSpaceRoot(addrFromPPtr(vroot), asid);

    NODE_STATE(ksCurThread) = thread;
}

static inline void mdb_node_ptr_mset_mdbNext_mdbRevocable_mdbFirstBadged(
    mdb_node_t *node_ptr, pptr_t mdbNext,
    word_t mdbRevocable, word_t mdbFirstBadged)
{
#if defined(__CHERI_PURE_CAPABILITY__)
    mdb_node_ptr_set_mdbFirstBadged(node_ptr, mdbFirstBadged);
    mdb_node_ptr_set_mdbRevocable(node_ptr, mdbRevocable);
    mdb_node_ptr_set_mdbNext(node_ptr, mdbNext);
#else
    /* FIXME: This should be more portable not assuming/inferring how the bitfield
     * generator lays out its fields, including variable names such as "words[1]". It should
     * instead use the setter functions for that.
     */
    node_ptr->words[1] = mdbNext | (mdbRevocable << 1) | mdbFirstBadged;
#endif
}

static inline void mdb_node_ptr_set_mdbPrev_np(mdb_node_t *node_ptr, pptr_t mdbPrev)
{
#if defined(__CHERI_PURE_CAPABILITY__)
    mdb_node_ptr_set_mdbPrev(node_ptr, mdbPrev);
#else
    /* FIXME: This could should be more portable not assuming/inferring how the bitfield
     * generator lays out its fields, including variable names such as "words[0]". It should
     * instead use the setter functions for that.
     */
    node_ptr->words[0] = mdbPrev;
#endif
}

static inline bool_t isValidVTableRoot_fp(cap_t vspace_root_cap)
{
    return cap_capType_equals(vspace_root_cap, cap_page_table_cap) &&
           cap_page_table_cap_get_capPTIsMapped(vspace_root_cap);
}

/* This is an accelerated check that msgLength, which appears
   in the bottom of the msgInfo word, is <= 4 and that msgExtraCaps
   which appears above it is zero. We are assuming that n_msgRegisters == 4
   for this check to be useful. By masking out the bottom 3 bits, we are
   really checking that n + 3 <= MASK(3), i.e. n + 3 <= 7 or n <= 4. */
compile_assert(n_msgRegisters_eq_4, n_msgRegisters == 4)
static inline int
fastpath_mi_check(word_t msgInfo)
{
    return (msgInfo & MASK(seL4_MsgLengthBits + seL4_MsgExtraCapBits)) > 4;
}

static inline void fastpath_copy_mrs(word_t length, tcb_t *src, tcb_t *dest)
{
    word_t i;
    regoff_t reg;

    /* assuming that length < n_msgRegisters */
    for (i = 0; i < length; i ++) {
        /* assuming that the message registers simply increment */
        reg = msgRegisters[0] + i;
        setRegister(dest, reg, getRegister(src, reg));
    }
}

static inline int fastpath_reply_cap_check(cap_t cap)
{
    return cap_capType_equals(cap, cap_reply_cap);
}

/** DONT_TRANSLATE */
static inline void NORETURN FORCE_INLINE fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
    NODE_UNLOCK_IF_HELD;

    pptr_t cur_thread_regs = (pptr_t)cur_thread->tcbArch.tcbContext.registers;

#ifdef ENABLE_SMP_SUPPORT
    register_t sp = read_sscratch();
#if defined(__CHERI_PURE_CAPABILITY__)
    sp -= sizeof(register_t);
    *((register_t *)sp) = cur_thread_reg;
#else
    sp -= sizeof(register_t);
    *((register_t *)sp) = cur_thread_reg;
#endif
#endif

    c_exit_hook();

#ifdef CONFIG_HAVE_FPU
    lazyFPURestore(cur_thread);
    set_tcb_fs_state(cur_thread, isFpuEnable());
#endif

    register word_t badge_reg asm("a0") = badge;
    register word_t msgInfo_reg asm("a1") = msgInfo;
    register pptr_t cur_thread_reg asm(REGN(t0)) = cur_thread_regs;

    asm volatile(
        LOAD_S " "REGN(ra) ", (0*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(sp)  ", (1*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(gp)  ", (2*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(tp)  ", (3*%[REGSIZE])("REGN(t0)") \n"
        /* skip t0/x5, t1/x6, they are restored later */
        LOAD_S " "REGN(t2) ", (6*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(s0) ", (7*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(s1) ", (8*%[REGSIZE])("REGN(t0)")  \n"
        /* skip a0/x10, a1/x11, they have been restored already */
        LOAD_S " "REGN(a2) ", (11*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a3) ", (12*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a4) ", (13*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a5) ", (14*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a6) ", (15*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a7) ", (16*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s2) ", (17*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s3) ", (18*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s4) ", (19*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s5) ", (20*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s6) ", (21*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s7) ", (22*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s8) ", (23*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s9) ", (24*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s10) ", (25*%[REGSIZE])("REGN(t0)")\n"
        LOAD_S " "REGN(s11) ", (26*%[REGSIZE])("REGN(t0)")\n"
        LOAD_S " "REGN(t3) ", (27*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t4) ", (28*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t5) ", (29*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t6) ", (30*%[REGSIZE])("REGN(t0)") \n"
        /* get sepc */
        LOAD_S " "REGN(t1) ", (34*%[REGSIZE])("REGN(t0)") \n"
#if defined(__CHERI_PURE_CAPABILITY__)
        "cspecialw sepcc, ct1  \n"
#else
        "csrw sepc, t1  \n"
#endif
#ifndef ENABLE_SMP_SUPPORT
        /* Write back sscratch with cur_thread_reg to get it back on the next trap entry */
#if defined(__CHERI_PURE_CAPABILITY__)
        "cspecialw sscratchc, ct0  \n"
#else
        "csrw sscratch, t0         \n"
#endif
#endif
        LOAD_S " "REGN(t1) ", (32*%[REGSIZE])("REGN(t0)") \n"
        "csrw sstatus, t1\n"

        LOAD_S " "REGN(t1) ", (5*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t0) ", (4*%[REGSIZE])("REGN(t0)") \n"
        "sret"
        : /* no output */
        : ASM_REG_CONSTR(cur_thread_reg),
        [REGSIZE] "i"(sizeof(register_t)),
        "r"(badge_reg),
        "r"(msgInfo_reg)
        : "memory"
    );

    UNREACHABLE();
}

