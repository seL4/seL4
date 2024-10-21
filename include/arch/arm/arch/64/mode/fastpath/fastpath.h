/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
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
    regoff_t reg;

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

#ifdef CONFIG_HAVE_FPU
    lazyFPURestore(cur_thread);
#endif /* CONFIG_HAVE_FPU */

    register word_t badge_reg asm("x0") = badge;
    register word_t msgInfo_reg asm("x1") = msgInfo;
    register pptr_t cur_thread_reg = (pptr_t)cur_thread->tcbArch.tcbContext.registers;

    asm volatile(
        "mov     " REGN(sp) ", %2                               \n"

        /* Restore thread's SPSR, LR, and SP */
        "ldp     "REG(21)", "REG(22)", ["REGN(sp)", %[SP_EL0]]  \n"
        "ldr     "REG(23)", ["REGN(sp)", %[SPSR_EL1]]           \n"
        "msr     "REGN(sp_el0)", " REG(21)"                     \n"
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        "msr     "REGN(elr_el2)", "REG(22)"                     \n"
        "msr     spsr_el2, x23                                  \n"
#else
        "msr     "REGN(elr_el1)", "REG(22)"                     \n"
        "msr     spsr_el1, x23                                  \n"
#endif

        /* Restore remaining registers */
        "ldp     "REG(2)",  "REG(3)",  ["REGN(sp)", #(%[REGSZ] * 2) * 1]    \n"
        "ldp     "REG(4)",  "REG(5)",  ["REGN(sp)", #(%[REGSZ] * 2) * 2]    \n"
        "ldp     "REG(6)",  "REG(7)",  ["REGN(sp)", #(%[REGSZ] * 2) * 3]    \n"
        "ldp     "REG(8)",  "REG(9)",  ["REGN(sp)", #(%[REGSZ] * 2) * 4]    \n"
        "ldp     "REG(10)", "REG(11)", ["REGN(sp)", #(%[REGSZ] * 2) * 5]    \n"
        "ldp     "REG(12)", "REG(13)", ["REGN(sp)", #(%[REGSZ] * 2) * 6]    \n"
        "ldp     "REG(14)", "REG(15)", ["REGN(sp)", #(%[REGSZ] * 2) * 7]    \n"
        "ldp     "REG(16)", "REG(17)", ["REGN(sp)", #(%[REGSZ] * 2) * 8]    \n"
        "ldp     "REG(18)", "REG(19)", ["REGN(sp)", #(%[REGSZ] * 2) * 9]    \n"
        "ldp     "REG(20)", "REG(21)", ["REGN(sp)", #(%[REGSZ] * 2) * 10]   \n"
        "ldp     "REG(22)", "REG(23)", ["REGN(sp)", #(%[REGSZ] * 2) * 11]   \n"
        "ldp     "REG(24)", "REG(25)", ["REGN(sp)", #(%[REGSZ] * 2) * 12]   \n"
        "ldp     "REG(26)", "REG(27)", ["REGN(sp)", #(%[REGSZ] * 2) * 13]   \n"
        "ldp     "REG(28)", "REG(29)", ["REGN(sp)", #(%[REGSZ] * 2) * 14]   \n"
        "ldr     "REG(30)", ["REGN(sp)", %[LR]]                             \n"
        "eret"
        :
        : "r"(badge_reg), "r"(msgInfo_reg), ASM_REG_CONSTR(cur_thread_reg),
        [SP_EL0] "i"(PT_SP_EL0), [SPSR_EL1] "i"(PT_SPSR_EL1), [LR] "i"(PT_LR),
        [REGSZ] "i" (REGSIZE)
        : "memory"
    );

    UNREACHABLE();
}

