/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
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

#ifdef CONFIG_HAVE_FPU
    lazyFPURestore(thread);
#endif

    NODE_STATE(ksCurThread) = thread;
}

static inline void mdb_node_ptr_mset_mdbNext_mdbRevocable_mdbFirstBadged(
    mdb_node_t *node_ptr, word_t mdbNext,
    word_t mdbRevocable, word_t mdbFirstBadged)
{
    node_ptr->words[1] = mdbNext | (mdbRevocable << 1) | mdbFirstBadged;
}

static inline void mdb_node_ptr_set_mdbPrev_np(mdb_node_t *node_ptr, word_t mdbPrev)
{
    node_ptr->words[0] = mdbPrev;
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
    register_t reg;

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

    word_t cur_thread_regs = (word_t)cur_thread->tcbArch.tcbContext.registers;

#ifdef ENABLE_SMP_SUPPORT
    word_t sp = read_sscratch();
    sp -= sizeof(word_t);
    *((word_t *)sp) = cur_thread_regs;
#endif

    c_exit_hook();

#ifdef CONFIG_HAVE_FPU
    set_tcb_fs_state(cur_thread, isFpuEnable());
#endif

    register word_t badge_reg asm("a0") = badge;
    register word_t msgInfo_reg asm("a1") = msgInfo;
    register word_t cur_thread_reg asm("t0") = cur_thread_regs;

    asm volatile(
        LOAD_S "  ra, (0*%[REGSIZE])(t0)  \n"
        LOAD_S "  sp, (1*%[REGSIZE])(t0)  \n"
        LOAD_S "  gp, (2*%[REGSIZE])(t0)  \n"
        LOAD_S "  tp, (3*%[REGSIZE])(t0)  \n"
        /* skip t0/x5, t1/x6, they are restored later */
        LOAD_S "  t2, (6*%[REGSIZE])(t0)  \n"
        LOAD_S "  s0, (7*%[REGSIZE])(t0)  \n"
        LOAD_S "  s1, (8*%[REGSIZE])(t0)  \n"
        /* skip a0/x10, a1/x11, they have been restored already */
        LOAD_S "  a2, (11*%[REGSIZE])(t0) \n"
        LOAD_S "  a3, (12*%[REGSIZE])(t0) \n"
        LOAD_S "  a4, (13*%[REGSIZE])(t0) \n"
        LOAD_S "  a5, (14*%[REGSIZE])(t0) \n"
        LOAD_S "  a6, (15*%[REGSIZE])(t0) \n"
        LOAD_S "  a7, (16*%[REGSIZE])(t0) \n"
        LOAD_S "  s2, (17*%[REGSIZE])(t0) \n"
        LOAD_S "  s3, (18*%[REGSIZE])(t0) \n"
        LOAD_S "  s4, (19*%[REGSIZE])(t0) \n"
        LOAD_S "  s5, (20*%[REGSIZE])(t0) \n"
        LOAD_S "  s6, (21*%[REGSIZE])(t0) \n"
        LOAD_S "  s7, (22*%[REGSIZE])(t0) \n"
        LOAD_S "  s8, (23*%[REGSIZE])(t0) \n"
        LOAD_S "  s9, (24*%[REGSIZE])(t0) \n"
        LOAD_S "  s10, (25*%[REGSIZE])(t0)\n"
        LOAD_S "  s11, (26*%[REGSIZE])(t0)\n"
        LOAD_S "  t3, (27*%[REGSIZE])(t0) \n"
        LOAD_S "  t4, (28*%[REGSIZE])(t0) \n"
        LOAD_S "  t5, (29*%[REGSIZE])(t0) \n"
        LOAD_S "  t6, (30*%[REGSIZE])(t0) \n"
        /* get sepc */
        LOAD_S "  t1, (34*%[REGSIZE])(t0)\n"
        "csrw sepc, t1  \n"
#ifndef ENABLE_SMP_SUPPORT
        /* Write back sscratch with cur_thread_reg to get it back on the next trap entry */
        "csrw sscratch, t0\n"
#endif
        LOAD_S "  t1, (32*%[REGSIZE])(t0) \n"
        "csrw sstatus, t1\n"

        LOAD_S "  t1, (5*%[REGSIZE])(t0) \n"
        LOAD_S "  t0, (4*%[REGSIZE])(t0) \n"
        "sret"
        : /* no output */
        : "r"(cur_thread_reg),
        [REGSIZE] "i"(sizeof(word_t)),
        "r"(badge_reg),
        "r"(msgInfo_reg)
        : "memory"
    );

    UNREACHABLE();
}

