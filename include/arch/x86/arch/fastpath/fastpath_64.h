/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_FASTPATH_64_H
#define __ARCH_FASTPATH_64_H

#include <util.h>
#include <arch/linker.h>
#include <api/types.h>
#include <api/syscall.h>

static inline void
switchToThread_fp(tcb_t *thread, vspace_root_t *vroot, pde_t stored_hw_asid)
{
    word_t base;
    word_t new_vroot = pptr_to_paddr(vroot);
    /* the asid is the 12-bit PCID */
    asid_t asid = (asid_t)(stored_hw_asid.words[0] & 0xfff);
    if (likely(getCurrentVSpaceRoot() != (new_vroot | asid))) {
        setCurrentVSpaceRoot(new_vroot, asid);
    }

    /* Code equivalent to in Arch_switchToThread, see arch/object/structures.bf
     * for layout of gdt_data */
    /* update the GDT_TLS entry with the thread's TLS_BASE address */
    base = getRegister(thread, TLS_BASE);
    x86_write_fs_base(base);

    /* update the GDT_IPCBUF entry with the thread's IPC buffer address */
    base = thread->tcbIPCBuffer;
    x86_write_gs_base(base);

    ksCurThread = thread;
}

static inline void
thread_state_ptr_set_blockingIPCDiminish_np(thread_state_t *ts_ptr, word_t dim)
{
    ts_ptr->words[1] = (ts_ptr->words[1] & 1) | dim;
}

static inline void
mdb_node_ptr_mset_mdbNext_mdbRevocable_mdbFirstBadged(
    mdb_node_t *node_ptr, word_t mdbNext,
    word_t mdbRevocable, word_t mdbFirstBadged)
{
    node_ptr->words[1] = (mdbNext << 16) | (mdbRevocable << 1) | mdbFirstBadged;
}

static inline void
mdb_node_ptr_set_mdbPrev_np(mdb_node_t *node_ptr, word_t mdbPrev)
{
    node_ptr->words[0] = mdbPrev << 16;
}

static inline bool_t
isValidVTableRoot_fp(cap_t vspace_root_cap)
{
    return cap_capType_equals(vspace_root_cap, cap_pml4_cap) && cap_pml4_cap_get_capPML4IsMapped(vspace_root_cap);
}

static inline void
fastpath_copy_mrs(word_t length, tcb_t *src, tcb_t *dest)
{
    /* currently we do not take the advantage of 8 additional registers */
    if (length == 2) {
        setRegister(dest, RBP, getRegister(src, RBP)); 
    }
    if (length == 2 || length == 1) {
        setRegister(dest, RDI, getRegister(src, RDI));
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

static inline void NORETURN
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
    {
        word_t context_base = ((word_t)ksCurThread) + (n_contextRegisters * sizeof(word_t));
        tss_ptr_set_rsp0_l(&x86KStss, (uint32_t)context_base);
        tss_ptr_set_rsp0_u(&x86KStss, (uint32_t)(context_base >> 32));
        ksCurThread->tcbArch.tcbContext.registers[RFLAGS] &= ~0x200;

        asm volatile (
                "movq %%rcx, %%rsp\n"
                "popq %%rdi\n"
                "popq %%rbp\n"
                "popq %%r8\n"
                "popq %%r9\n"
                "popq %%r10\n"
                "popq %%r11\n"
                "popq %%r12\n"
                "popq %%r13\n"
                "popq %%r14\n"
                "popq %%r15\n"
                // skip DS, ES, FS, GS, FaultIP, TLS_BASE, padding, Error
                "addq $64, %%rsp\n"
                // Restore NextIP
                "popq %%rdx\n"
                // skip CS
                "addq $8, %%rsp\n"
                // restore RFLAGS
                "popfq\n"
                // reset interrupt bit
                "orq $0x200, -8(%%rsp)\n"
                // restore RSP
                "popq %%rcx\n"
                "sti\n"
                "rex.w sysexit\n"
                :
                : "c" (&cur_thread->tcbArch.tcbContext.registers[RDI]),
                  "a" (cur_thread->tcbArch.tcbContext.registers[RAX]),
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
#endif

