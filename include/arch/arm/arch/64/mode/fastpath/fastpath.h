/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
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
#include <mode/model/statedata.h>
#include <arch/object/vcpu.h>
#include <machine/fpu.h>
#include <smp/lock.h>

/* When building the fastpath the assembler in traps.S makes these
 * assumptions. Because compile_asserts are hard to do in assembler,
 * we place them here */
compile_assert(SysCall_Minus1, SysCall == -1)
compile_assert(SysReplyRecv_Minus2, SysReplyRecv == -2)

/* Use macros to not break verification */
#define endpoint_ptr_get_epQueue_tail_fp(ep_ptr) TCB_PTR(endpoint_ptr_get_epQueue_tail(ep_ptr))
#define cap_vtable_cap_get_vspace_root_fp(vtable_cap) VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(vtable_cap))

static inline void FORCE_INLINE
switchToThread_fp(tcb_t *thread, vspace_root_t *vroot, pde_t stored_hw_asid)
{
    asid_t asid;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(thread->tcbArch.tcbVCPU);
    }
    asid = (asid_t)(stored_hw_asid.words[0] & 0xffff);
    armv_contextSwitch_HWASID(vroot, asid);

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(NODE_STATE(ksCurThread), thread);
#endif

#ifdef CONFIG_HAVE_FPU
    lazyFPURestore(thread);
#endif /* CONFIG_HAVE_FPU */

    NODE_STATE(ksCurThread) = thread;
}

#ifdef CONFIG_EXCEPTION_FASTPATH
static inline void fastpath_set_tcbfault_vm_fault(vm_fault_type_t type)
{
    switch (type) {
    case ARMDataAbort: {
        word_t addr, fault;

        addr = getFAR();
        fault = getDFSR();

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        /* use the IPA */
        if (ARCH_NODE_STATE(armHSVCPUActive)) {
            addr = GET_PAR_ADDR(addressTranslateS1(addr)) | (addr & MASK(PAGE_BITS));
        }
#endif
        NODE_STATE(ksCurThread)->tcbFault = seL4_Fault_VMFault_new(addr, fault, false);
        break;
    }
    case ARMPrefetchAbort: {
        word_t pc, fault;

        pc = getRestartPC(NODE_STATE(ksCurThread));
        fault = getIFSR();

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (ARCH_NODE_STATE(armHSVCPUActive)) {
            pc = GET_PAR_ADDR(addressTranslateS1(pc)) | (pc & MASK(PAGE_BITS));
        }
#endif
        NODE_STATE(ksCurThread)->tcbFault = seL4_Fault_VMFault_new(pc, fault, true);
        break;
    }
    }
}
#endif

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
    return cap_capType_equals(vspace_root_cap, cap_vspace_cap)
           && cap_vspace_cap_get_capVSIsMapped(vspace_root_cap);
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

#ifndef CONFIG_KERNEL_MCS
static inline int fastpath_reply_cap_check(cap_t cap)
{
    return cap_capType_equals(cap, cap_reply_cap);
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

    register word_t badge_reg asm("x0") = badge;
    register word_t msgInfo_reg asm("x1") = msgInfo;
    register word_t cur_thread_reg asm("x2") = (word_t)cur_thread->tcbArch.tcbContext.registers;

    asm volatile(
        "mov     sp, x2                     \n"

        /* Restore thread's SPSR, LR, and SP */
        "ldp     x21, x22, [sp, %[SP_EL0]]  \n"
        "ldr     x23, [sp, %[SPSR_EL1]]     \n"
        "msr     sp_el0, x21                \n"
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        "msr     elr_el2, x22               \n"
        "msr     spsr_el2, x23              \n"
#else
        "msr     elr_el1, x22               \n"
        "msr     spsr_el1, x23              \n"
#endif

        /* Restore remaining registers */
        "ldp     x2,  x3,  [sp, #16 * 1]    \n"
        "ldp     x4,  x5,  [sp, #16 * 2]    \n"
        "ldp     x6,  x7,  [sp, #16 * 3]    \n"
        "ldp     x8,  x9,  [sp, #16 * 4]    \n"
        "ldp     x10, x11, [sp, #16 * 5]    \n"
        "ldp     x12, x13, [sp, #16 * 6]    \n"
        "ldp     x14, x15, [sp, #16 * 7]    \n"
        "ldp     x16, x17, [sp, #16 * 8]    \n"
        "ldp     x18, x19, [sp, #16 * 9]    \n"
        "ldp     x20, x21, [sp, #16 * 10]   \n"
        "ldp     x22, x23, [sp, #16 * 11]   \n"
        "ldp     x24, x25, [sp, #16 * 12]   \n"
        "ldp     x26, x27, [sp, #16 * 13]   \n"
        "ldp     x28, x29, [sp, #16 * 14]   \n"
        "ldr     x30, [sp, %[LR]]           \n"
        "eret                                 "
        :
        : "r"(badge_reg), "r"(msgInfo_reg), "r"(cur_thread_reg),
        [SP_EL0] "i"(PT_SP_EL0), [SPSR_EL1] "i"(PT_SPSR_EL1), [LR] "i"(PT_LR)
        : "memory"
    );

    UNREACHABLE();
}

