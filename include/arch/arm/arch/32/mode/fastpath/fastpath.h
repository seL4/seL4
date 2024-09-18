/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <linker.h>
#include <api/types.h>
#include <api/syscall.h>
#include <armv/context_switch.h>
#include <arch/machine/debug.h>
#include <smp/lock.h>
#include <machine/fpu.h>

/* When building the fastpath the assembler in traps.S makes these
 * assumptions. Because compile_asserts are hard to do in assembler,
 * we place them here */
compile_assert(SysCall_Minus1, SysCall == -1)
compile_assert(SysReplyRecv_Minus2, SysReplyRecv == -2)

/* Use macros to not break verification */
#define endpoint_ptr_get_epQueue_tail_fp(ep_ptr) TCB_PTR(endpoint_ptr_get_epQueue_tail(ep_ptr))
#define cap_vtable_cap_get_vspace_root_fp(vtable_cap) PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(vtable_cap))

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void
clearExMonitor_fp(void)
{
    word_t temp1 = 0;
    word_t temp2;
    asm volatile(
        "strex %[output], %[mem], [%[mem]]"
        : [output]"+r"(temp1)
        : [mem]"r"(&temp2)
    );
}

static inline void FORCE_INLINE switchToThread_fp(tcb_t *thread, pde_t *cap_pd, pde_t stored_hw_asid)
{
    hw_asid_t hw_asid;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(thread->tcbArch.tcbVCPU);
    }
    hw_asid = pde_pde_invalid_get_stored_hw_asid(stored_hw_asid);
    armv_contextSwitch_HWASID(cap_pd, hw_asid);

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(NODE_STATE(ksCurThread), thread);
#endif

#ifdef CONFIG_HAVE_FPU
    lazyFPURestore(thread);
#endif /* CONFIG_HAVE_FPU */

    NODE_STATE(ksCurThread) = thread;
    clearExMonitor_fp();
}

#ifndef CONFIG_KERNEL_MCS
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
#endif

static inline bool_t isValidVTableRoot_fp(cap_t pd_cap)
{
    return (pd_cap.words[0] & MASK(5)) ==
           (BIT(4) | cap_page_directory_cap);
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
    return ((msgInfo & MASK(seL4_MsgLengthBits + seL4_MsgExtraCapBits))
            + 3) & ~MASK(3);
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

#ifndef CONFIG_KERNEL_MCS
static inline int fastpath_reply_cap_check(cap_t cap)
{
    return (cap.words[0] & MASK(5)) == cap_reply_cap;
}
#endif

/** DONT_TRANSLATE */
static inline void NORETURN FORCE_INLINE fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
    NODE_UNLOCK;

    c_exit_hook();

#ifdef ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS
    restore_user_debug_context(cur_thread);
#endif

    register word_t badge_reg asm("r0") = badge;
    register word_t msgInfo_reg asm("r1") = msgInfo;
    register word_t cur_thread_reg asm("r2") = (word_t)cur_thread->tcbArch.tcbContext.registers;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        asm volatile( /* r0 and r1 should be preserved */
            "mov sp, r2         \n"
            /* Pop user registers, preserving r0 and r1 */
            "add sp, sp, #8     \n"
            "pop {r2-r12}       \n"
            /* Restore the user stack pointer */
            "pop {lr}           \n"
            "msr sp_usr, lr     \n"
            /* prepare the exception return lr */
            "ldr lr, [sp, #4]   \n"
            "msr elr_hyp, lr    \n"
            /* prepare the user status register */
            "ldr lr, [sp, #8]   \n"
            "msr spsr, lr       \n"
            /* Finally, pop our LR */
            "pop {lr}           \n"
            /* Return to user */
            "eret"
            :
            : [badge] "r"(badge_reg),
            [msginfo]"r"(msgInfo_reg),
            [cur_thread]"r"(cur_thread_reg)
            : "memory"
        );
    } else {
        asm volatile("mov sp, r2 \n\
                  add sp, sp, %[LR_SVC_OFFSET] \n\
                  ldmdb sp, {r2-lr}^ \n\
                  rfeia sp"
                     :
                     : [badge]"r"(badge_reg),
                     [msginfo]"r"(msgInfo_reg),
                     [cur_thread]"r"(cur_thread_reg),
                     [LR_SVC_OFFSET]"i"(NextIP * sizeof(word_t))
                     : "memory"
                    );
    }
    UNREACHABLE();
}

