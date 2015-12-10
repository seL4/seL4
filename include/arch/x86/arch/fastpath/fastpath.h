/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_FASTPATH_H
#define __ARCH_FASTPATH_H

#include <util.h>
#include <arch/linker.h>
#include <api/types.h>
#include <api/syscall.h>

static inline void FASTCALL
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
    gdt_entry_gdt_data_ptr_set_base_low(ia32KSgdt + GDT_TLS, base);
    gdt_entry_gdt_data_ptr_set_base_mid(ia32KSgdt + GDT_TLS,  (base >> 16) & 0xFF);
    gdt_entry_gdt_data_ptr_set_base_high(ia32KSgdt + GDT_TLS, (base >> 24) & 0xFF);

    /* update the GDT_IPCBUF entry with the thread's IPC buffer address */
    base = thread->tcbIPCBuffer;
    gdt_entry_gdt_data_ptr_set_base_low(ia32KSgdt + GDT_IPCBUF, base);
    gdt_entry_gdt_data_ptr_set_base_mid(ia32KSgdt + GDT_IPCBUF,  (base >> 16) & 0xFF);
    gdt_entry_gdt_data_ptr_set_base_high(ia32KSgdt + GDT_IPCBUF, (base >> 24) & 0xFF);

    ksCurThread = thread;
}

static inline void
thread_state_ptr_set_blockingIPCDiminish_np(thread_state_t *ts_ptr, word_t dim)
{
    ts_ptr->words[2] &= BIT(0);
    ts_ptr->words[1] = dim;
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
#ifdef CONFIG_PAE_PAGING
    return cap_capType_equals(pd_cap, cap_pdpt_cap);
#else
    return cap_capType_equals(pd_cap, cap_page_directory_cap);
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

static inline void FASTCALL NORETURN
fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
    if (unlikely(cur_thread == ia32KSfpuOwner)) {
        /* We are using the FPU, make sure it is enabled */
        enableFpu();
    } else if (unlikely(ia32KSfpuOwner)) {
        /* Someone is using the FPU and it might be enabled */
        disableFpu();
    } else {
        /* No-one (including us) is using the FPU, so we assume it
         * is currently disabled */
    }
    tss_ptr_set_esp0(&ia32KStss, ((uint32_t)cur_thread) + 0x4c);
    cur_thread->tcbArch.tcbContext.registers[EFLAGS] &= ~0x200;
    if (likely(hasDefaultSelectors(cur_thread))) {
        asm volatile("\
                movl %%ecx, %%esp \n\
                popl %%edi \n\
                popl %%ebp \n\
                addl $8, %%esp \n\
                popl %%fs \n\
                popl %%gs \n\
                addl $20, %%esp \n\
                popfl \n\
                orl $0x200, 44(%%ecx) \n\
                movl 36(%%ecx), %%edx \n\
                pop %%ecx \n\
                sti \n\
                sysexit \n\
            "
                     :
                     : "c"(&cur_thread->tcbArch.tcbContext.registers[EDI]),
                     "a" (cur_thread->tcbArch.tcbContext.registers[EAX]),
                     "b" (badge),
                     "S" (msgInfo)
                     : "memory"
                    );
    } else {
        asm volatile("\
                movl %%ecx, %%esp \n\
                popl %%edi \n\
                popl %%ebp \n\
                popl %%ds \n\
                popl %%es \n\
                popl %%fs \n\
                popl %%gs \n\
                addl $20, %%esp \n\
                popfl \n\
                orl $0x200, 44(%%ecx) \n\
                movl 36(%%ecx), %%edx \n\
                pop %%ecx \n\
                sti \n\
                sysexit \n\
            "
                     :
                     : "c"(&cur_thread->tcbArch.tcbContext.registers[EDI]),
                     "a" (cur_thread->tcbArch.tcbContext.registers[EAX]),
                     "b" (badge),
                     "S" (msgInfo)
                     : "memory"
                    );
    }
    /* This function is marked NORETURN, but gcc is not aware that the previous assembly
       block will return to user level. This loop prevents gcc complaining, and also helps
       it optimize register usage in this function (since gcc knows it can clobber everything
       as it will not be returning or calling anything else */
    while (1);
}

static inline int
fastpath_reply_cap_check(cap_t cap)
{
    return cap_capType_equals(cap, cap_reply_cap);
}

void slowpath(syscall_t syscall)
NORETURN;

void fastpath_call(word_t cptr, word_t r_msgInfo)
VISIBLE FASTCALL NORETURN;

void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
VISIBLE FASTCALL NORETURN;

#endif
