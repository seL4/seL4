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

#ifndef __ARCH_MODE_FASTPATH_FASTPATH_H_
#define __ARCH_MODE_FASTPATH_FASTPATH_H_

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
#if CONFIG_MAX_NUM_NODES > 1
    x86_wrmsr(IA32_KERNEL_GS_BASE_MSR, base);
    asm volatile("movq %[value], %%gs:%c[offset]"
            :
            : [value] "r" (&thread->tcbArch.tcbContext.registers[Error + 1]),
              [offset] "i" (OFFSETOF(nodeInfo_t, currentThreadUserContext)));
#else
    x86_write_gs_base(base);
#endif

    NODE_STATE(ksCurThread) = thread;
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
    return likely(cap_capType_equals(vspace_root_cap, cap_pml4_cap) && cap_pml4_cap_get_capPML4IsMapped(vspace_root_cap));
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
    if (config_set(CONFIG_SYSENTER) && config_set(CONFIG_HARDWARE_DEBUG_API) && ((getRegister(NODE_STATE(ksCurThread), FLAGS) & FLAGS_TF) != 0)) {
        /* If single stepping using sysenter we need to do a return using iret to avoid
         * a race condition in restoring the flags (which enables stepping and interrupts) and
         * calling sysexit. This case is handled in restore_user_context so we just go there
         */
        restore_user_context();
    }
    NODE_UNLOCK;
    c_exit_hook();
    if (unlikely(nativeThreadUsingFPU(cur_thread))) {
        /* We are using the FPU, make sure it is enabled */
        enableFpu();
    } else if (unlikely(ARCH_NODE_STATE(x86KSActiveFPUState))) {
        /* Someone is using the FPU and it might be enabled */
        disableFpu();
    } else {
        /* No-one (including us) is using the FPU, so we assume it
         * is currently disabled */
    }

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(NODE_STATE(ksCurThread));
#endif

    if (config_set(CONFIG_SYSENTER)) {
        cur_thread->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;

        asm volatile (
#if CONFIG_MAX_NUM_NODES > 1
                // Switch to the user GS value
            "swapgs\n"
#endif
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
            "orq %[IF], -8(%%rsp)\n"
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
            "S" (msgInfo),
            [IF] "i" (FLAGS_IF)
            : "memory"
        );
    } else {
        asm volatile(
#if CONFIG_MAX_NUM_NODES > 1
                // Switch to the user GS value
            "swapgs\n"
#endif
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
            : "r"(&NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[RAX]),
            "D" (badge),
            "S" (msgInfo)
            : "memory"
        );
    }
    UNREACHABLE();
}

#endif /* __ARCH_MODE_FASTPATH_FASTPATH_H_ */
