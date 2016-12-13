/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_FASTPATH_32_H
#define __ARCH_FASTPATH_32_H

#include <config.h>
#include <util.h>
#include <arch/linker.h>
#include <api/types.h>
#include <api/syscall.h>
#include <armv/context_switch.h>

/* When building the fastpath the assembler in traps.S makes these
 * assumptions. Because compile_asserts are hard to do in assembler,
 * we place them here */
compile_assert(SysCall_Minus1, SysCall == -1)
compile_assert(SysReplyRecv_Minus2, SysReplyRecv == -2)

/* Use macros to not break verification */
#define cap_cnode_cap_get_capCNodePtr_fp(cnode_cap) CTE_PTR(cap_cnode_cap_get_capCNodePtr(cnode_cap))
#define cap_endpoint_cap_get_capEPPtr_fp(ep_cap) EP_PTR(cap_endpoint_cap_get_capEPPtr(ep_cap))
#define endpoint_ptr_get_epQueue_tail_fp(ep_ptr) TCB_PTR(endpoint_ptr_get_epQueue_tail(ep_ptr))
#define cap_vtable_cap_get_vspace_root_fp(vtable_cap) PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(vtable_cap))

/** MODIFIES: [*] */
/** DONT_TRANSLATE */
static inline void
clearExMonitor_fp(void)
{
    word_t temp1 = 0;
    word_t temp2;
    asm volatile (
        "strex %[output], %[mem], [%[mem]]"
        : [output]"+r"(temp1)
        : [mem]"r"(&temp2)
    );
}

static inline void FORCE_INLINE
switchToThread_fp(tcb_t *thread, pde_t *cap_pd, pde_t stored_hw_asid)
{
    hw_asid_t hw_asid;

    hw_asid = pde_pde_invalid_get_stored_hw_asid(stored_hw_asid);
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(thread->tcbArch.vcpu);
    }
    armv_contextSwitch_HWASID(cap_pd, hw_asid);

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(ksCurThread, thread);
#endif

#if defined(CONFIG_IPC_BUF_GLOBALS_FRAME)
    *armKSGlobalsFrame = thread->tcbIPCBuffer;
#elif defined(CONFIG_IPC_BUF_TPIDRURW)
#else
#error "Unknown IPC buffer strategy"
#endif
    ksCurThread = thread;
    clearExMonitor_fp();
}

static inline void
mdb_node_ptr_mset_mdbNext_mdbRevocable_mdbFirstBadged(
    mdb_node_t *node_ptr, word_t mdbNext,
    word_t mdbRevocable, word_t mdbFirstBadged)
{
    node_ptr->words[1] = mdbNext | (mdbRevocable << 1) | mdbFirstBadged;
}

static inline void
mdb_node_ptr_set_mdbPrev_np(mdb_node_t *node_ptr, word_t mdbPrev)
{
    node_ptr->words[0] = mdbPrev;
}

static inline bool_t
isValidVTableRoot_fp(cap_t pd_cap)
{
    return (pd_cap.words[0] & MASK(5)) ==
           (BIT(4) | cap_page_directory_cap);
}

/* This is an accelerated check that msgLength, which appears
   in the bottom of the msgInfo word, is <= 4 and that msgExtraCaps
   which appears above it is zero. We are assuming that n_msgRegisters == 4
   for this check to be useful. By masking out the bottom 3 bits, we are
   really checking that n + 3 <= MASK(3), i.e. n + 3 <= 7 or n <= 4. */
compile_assert (n_msgRegisters_eq_4, n_msgRegisters == 4)
static inline int
fastpath_mi_check(word_t msgInfo)
{
    return ((msgInfo & MASK(seL4_MsgLengthBits + seL4_MsgExtraCapBits))
            + 3) & ~MASK(3);
}

static inline void
fastpath_copy_mrs(word_t length, tcb_t *src, tcb_t *dest)
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

static inline int
fastpath_reply_cap_check(cap_t cap)
{
    return (cap.words[0] & MASK(5)) == cap_reply_cap;
}

#endif /* __ARCH_FASTPATH_32_H */
