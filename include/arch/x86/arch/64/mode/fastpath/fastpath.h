/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

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
    if (likely(getCurrentCR3().words[0] != cr3_new(new_vroot, asid).words[0])) {
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
        setRegister(dest, R8, getRegister(src, R8));
    }
    if (length == 2 || length == 1) {
        setRegister(dest, R10, getRegister(src, R10));
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
    c_exit_hook();

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

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(ksCurThread);
#endif

    if (config_set(CONFIG_SYSENTER)) {
        cur_thread->tcbArch.tcbContext.registers[FLAGS] &= ~0x200;

        asm volatile (
            "movq %%rcx, %%rsp\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rbp\n"
            "popq %%r12\n"
            "popq %%r13\n"
            "popq %%r14\n"
            "popq %%r15\n"
            // Skip RDX, we need to put NextIP into it
            "addq $8, %%rsp\n"
            "popq %%r10\n"
            "popq %%r8\n"
            "popq %%r9\n"
            // restore RFLAGS
            "popfq\n"
            // reset interrupt bit
            "orq $0x200, -8(%%rsp)\n"
            // Restore NextIP
            "popq %%rdx\n"
            // skip Error
            "addq $8, %%rsp\n"
            // restore RSP
            "popq %%rcx\n"
            // Skip TLS_BASE FaultIP
            "addq $16, %%rsp\n"
            "popq %%r11\n"
            "sti\n"
            "rex.w sysexit\n"
            :
            : "c" (&cur_thread->tcbArch.tcbContext.registers[RAX]),
            "D" (badge),
            "S" (msgInfo)
            : "memory"
        );
    } else {
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %0, %%rsp\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rbp\n"
            "popq %%r12\n"
            "popq %%r13\n"
            "popq %%r14\n"
            "popq %%r15\n"
            "popq %%rdx\n"
            "popq %%r10\n"
            "popq %%r8\n"
            "popq %%r9\n"
            //restore RFLAGS
            "popq %%r11\n"
            // Restore NextIP
            "popq %%rcx\n"
            // clear RSP to not leak information to the user
            "xor %%rsp, %%rsp\n"
            // More register but we can ignore and are done restoring
            // enable interrupt disabled by sysenter
            "rex.w sysret\n"
            :
            : "r"(&ksCurThread->tcbArch.tcbContext.registers[RAX]),
            "D" (badge),
            "S" (msgInfo)
            : "memory"
        );
    }
    UNREACHABLE();
}
