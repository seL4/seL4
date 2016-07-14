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
#include <arch/linker.h>
#include <api/types.h>
#include <api/syscall.h>
#include <benchmark_track.h>

static inline void
switchToThread_fp(tcb_t *thread, pde_t *pd, pde_t stored_hw_asid)
{
    word_t base;
    uint32_t new_pd = pptr_to_paddr(pd);

    if (likely(getCurrentPD() != new_pd)) {
        setCurrentPD(new_pd);
    }

    /* Code equivalent to in Arch_switchToThread, see arch/object/structures.bf
     * for layout of gdt_data */
    /* update the GDT_TLS entry with the thread's TLS_BASE address */
    base = getRegister(thread, TLS_BASE);
    x86_write_fs_base(base);

    /* update the GDT_IPCBUF entry with the thread's IPC buffer address */
    base = thread->tcbIPCBuffer;
    x86_write_gs_base(base);

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(ksCurThread, thread);
#endif

    ksCurThread = thread;
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
#if defined(CONFIG_PAE_PAGING)
    return cap_capType_equals(vspace_root_cap, cap_pdpt_cap) && cap_pdpt_cap_get_capPDPTIsMapped(vspace_root_cap);
#else
    return cap_capType_equals(vspace_root_cap, cap_page_directory_cap) && cap_page_directory_cap_get_capPDIsMapped(vspace_root_cap);
#endif
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

static inline void NORETURN
fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
    benchmark_track_exit();
#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */

    if (unlikely(cur_thread == x86KSfpuOwner)) {
        /* We are using the FPU, make sure it is enabled */
        enableFpu();
    } else if (unlikely(x86KSfpuOwner)) {
        /* Someone is using the FPU and it might be enabled */
        disableFpu();
    } else {
        /* No-one (including us) is using the FPU, so we assume it
         * is currently disabled */
    }

    tss_ptr_set_esp0(&x86KStss.tss, ((uint32_t)&ksCurThread->tcbArch.tcbContext.registers) + (sizeof(word_t)*n_contextRegisters));
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
            "sysexit \n"
            :
            : "c"(&cur_thread->tcbArch.tcbContext.registers[EDI]),
            "a" (cur_thread->tcbArch.tcbContext.registers[EAX]),
            "b" (badge),
            "S" (msgInfo)
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
            "sysexit \n"
            :
            : "c"(&cur_thread->tcbArch.tcbContext.registers[EDI]),
            "a" (cur_thread->tcbArch.tcbContext.registers[EAX]),
            "b" (badge),
            "S" (msgInfo)
            : "memory"
        );
    }

    UNREACHABLE();
}

#endif

