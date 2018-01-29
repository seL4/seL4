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

#include <util.h>
#include <linker.h>
#include <arch/machine/debug.h>
#include <api/types.h>
#include <api/syscall.h>
#include <benchmark/benchmark_track.h>
#include <mode/stack.h>
#include <arch/kernel/tlb_bitmap.h>

static inline tcb_t *
endpoint_ptr_get_epQueue_tail_fp(endpoint_t *ep_ptr)
{
    return TCB_PTR(endpoint_ptr_get_epQueue_tail(ep_ptr));
}

static inline vspace_root_t *
cap_vtable_cap_get_vspace_root_fp(cap_t vtable_cap)
{
    return PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(vtable_cap));
}

static inline void FORCE_INLINE
switchToThread_fp(tcb_t *thread, vspace_root_t *pd, pde_t stored_hw_asid)
{
    uint32_t new_pd = pptr_to_paddr(pd);

    if (likely(getCurrentPD() != new_pd)) {
        SMP_COND_STATEMENT(tlb_bitmap_unset(paddr_to_pptr(getCurrentPD()), getCurrentCPUIndex());)
        SMP_COND_STATEMENT(tlb_bitmap_set(pd, getCurrentCPUIndex());)

        setCurrentPD(new_pd);
    }
    if (config_set(CONFIG_KERNEL_X86_IBPB_ON_CONTEXT_SWITCH)) {
        x86_ibpb();
    }
    if (config_set(CONFIG_KERNEL_X86_RSB_ON_CONTEXT_SWITCH)) {
        x86_flush_rsb();
    }

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(NODE_STATE(ksCurThread), thread);
#endif

    NODE_STATE(ksCurThread) = thread;
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
isValidVTableRoot_fp(cap_t vspace_root_cap)
{
    return cap_capType_equals(vspace_root_cap, cap_page_directory_cap) && cap_page_directory_cap_get_capPDIsMapped(vspace_root_cap);
}

static inline void
fastpath_copy_mrs(word_t length, tcb_t *src, tcb_t *dest)
{
    if (length == 2) {
        setRegister(dest, EBP, getRegister(src, EBP));
    }
    if (length == 2 || length == 1) {
        setRegister(dest, EDI, getRegister(src, EDI));
    }
}

/* This is an accelerated check that msgLength, which appears
   in the bottom of the msgInfo word, is <= 2 and that msgExtraCaps
   which appears above it is zero. We are assuming that n_msgRegisters == 2
   for this check to be useful.*/
compile_assert (n_msgRegisters_eq_2, n_msgRegisters == 2)
static inline int
fastpath_mi_check(word_t msgInfo)
{
    return (msgInfo & MASK(seL4_MsgLengthBits + seL4_MsgExtraCapBits)) > 2;
}

static inline bool_t hasDefaultSelectors(tcb_t *thread)
{
    return thread->tcbArch.tcbContext.registers[DS] == SEL_DS_3   &&
           thread->tcbArch.tcbContext.registers[ES] == SEL_DS_3;
}

static inline void NORETURN FORCE_INLINE
fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
    c_exit_hook();

    NODE_UNLOCK;
    lazyFPURestore(cur_thread);

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(cur_thread);
    assert(!cur_thread->tcbArch.tcbContext.breakpointState.single_step_enabled);
#endif

    setKernelEntryStackPointer(cur_thread);

    word_t base = getRegister(cur_thread, TLS_BASE);
    x86_write_gs_base(base, SMP_TERNARY(getCurrentCPUIndex(), 0));

    base = cur_thread->tcbIPCBuffer;
    x86_write_fs_base(base, SMP_TERNARY(getCurrentCPUIndex(), 0));

    if (config_set(CONFIG_KERNEL_X86_IBRS_BASIC)) {
        x86_disable_ibrs();
    }

    cur_thread->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;
    if (likely(hasDefaultSelectors(cur_thread))) {
        asm volatile(
            "movl %%ecx, %%esp\n"
            "popl %%edi \n"
            "popl %%ebp \n"
#if defined(CONFIG_FSGSBASE_GDT)
            "addl $8, %%esp \n"
            "popl %%fs \n"
            "popl %%gs \n"
            "addl $12, %%esp \n"
#elif defined(CONFIG_FSGSBASE_MSR)
            "addl $28, %%esp \n"
#else
#error "Invalid method to set IPCBUF/TLS"
#endif
            "popl %%edx \n"
            "movl 8(%%esp), %%ecx \n"
            "addl $4, %%esp \n"
            "popfl \n"
            "orl %[IFMASK], -4(%%esp)\n"
            "sti\n"
            "sysexit \n"
            :
            : "c"(&cur_thread->tcbArch.tcbContext.registers[EDI]),
            "a" (cur_thread->tcbArch.tcbContext.registers[EAX]),
            "b" (badge),
            "S" (msgInfo),
            [IFMASK]"i"(FLAGS_IF)
            : "memory"
        );
    } else {
        asm volatile(
            "movl %%ecx, %%esp \n"
            "popl %%edi \n"
            "popl %%ebp \n"
            "popl %%ds \n"
            "popl %%es \n"
#if defined(CONFIG_FSGSBASE_GDT)
            "popl %%fs \n"
            "popl %%gs \n"
            "addl $12, %%esp \n"
#elif defined(CONFIG_FSGSBASE_MSR)
            "addl $20, %%esp \n"
#else
#error "Invalid method to set IPCBUF/TLS"
#endif
            "popl %%edx \n"
            "movl 8(%%esp), %%ecx \n"
            "addl $4, %%esp \n"
            "popfl \n"
            "orl %[IFMASK], -4(%%esp)\n"
            "sti\n"
            "sysexit \n"
            :
            : "c"(&cur_thread->tcbArch.tcbContext.registers[EDI]),
            "a" (cur_thread->tcbArch.tcbContext.registers[EAX]),
            "b" (badge),
            "S" (msgInfo),
            [IFMASK]"i"(FLAGS_IF)
            : "memory"
        );
    }

    UNREACHABLE();
}

#endif
