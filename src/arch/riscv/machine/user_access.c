/*
 * Copyright 2025
 *
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * Safe user memory access in S-mode via SUM (Supervisor User Memory).
 * Memory operations are enclosed in a temporary SATP switch and fault handler.
 */

#include <arch/machine/user_access.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/traps.h>
#include <mode/hardware.h>
#include <arch/model/smp.h>
#include <model/statedata.h>
#include <object/structures.h>
#include <util.h>

#define user_access_state()  (&riscv_user_access_state[CURRENT_CPU_INDEX()])

static inline void restore_vspace_and_sstatus(void)
{
    riscv_user_access_state_t *s = user_access_state();
    write_sstatus(s->saved_sstatus);
    write_satp(s->saved_satp);
    s->in_progress = false;
    sfence();
}

/*
 * Minimal S-mode fault handler for the user access window.
 * Bypasses trap_entry to prevent corrupting the user thread's TCB.
 */
static void __attribute__((naked, aligned(4))) riscv_user_access_fault_handler(void)
{
    asm volatile(
        ".option push\n"
        ".option norelax\n"
        "la gp, __global_pointer$\n"
        ".option pop\n"
        "la t0, trap_entry\n"
        "csrw stvec, t0\n"
        "csrr a0, scause\n"
        "call riscv_user_access_handle_fault\n"
        "1: j 1b\n"
    );
}

bool_t riscv_load_word_user(vptr_t user_addr, word_t *out_val,
                          paddr_t vspace_root_paddr, asid_t asid)
{
    if (user_addr & (sizeof(word_t) - 1)) {
        return false;
    }
    if (user_addr >= USER_TOP || (USER_TOP - user_addr) < sizeof(word_t)) {
        return false;
    }

    riscv_user_access_state_t *s = user_access_state();
    if (setjmp(s->jmp_buf) != 0) {
        return false;
    }

    s->saved_satp = read_satp();
    s->saved_sstatus = read_sstatus();
    s->in_progress = true;

    word_t old_stvec = read_stvec();
    write_stvec((word_t)riscv_user_access_fault_handler);

    setVSpaceRoot(vspace_root_paddr, asid);
    write_sstatus(read_sstatus() | SSTATUS_SUM);

    *out_val = *(word_t *)(word_t)user_addr;

    write_stvec(old_stvec);
    restore_vspace_and_sstatus();
    return true;
}

bool_t riscv_store_word_user(vptr_t user_addr, word_t val,
                           paddr_t vspace_root_paddr, asid_t asid)
{
    if (user_addr & (sizeof(word_t) - 1)) {
        return false;
    }
    if (user_addr >= USER_TOP || (USER_TOP - user_addr) < sizeof(word_t)) {
        return false;
    }

    riscv_user_access_state_t *s = user_access_state();
    if (setjmp(s->jmp_buf) != 0) {
        return false;
    }

    s->saved_satp = read_satp();
    s->saved_sstatus = read_sstatus();
    s->in_progress = true;

    word_t old_stvec = read_stvec();
    write_stvec((word_t)riscv_user_access_fault_handler);

    setVSpaceRoot(vspace_root_paddr, asid);
    write_sstatus(read_sstatus() | SSTATUS_SUM);

    *(word_t *)(word_t)user_addr = val;

    write_stvec(old_stvec);
    restore_vspace_and_sstatus();
    return true;
}

bool_t riscv_user_access_handle_fault(word_t scause)
{
    riscv_user_access_state_t *s = user_access_state();
    if (!s->in_progress) {
        return false;
    }

    word_t cause = scause & MASK(63);
    if (cause != RISCVLoadPageFault && cause != RISCVStorePageFault &&
        cause != RISCVLoadAccessFault && cause != RISCVStoreAccessFault) {
        return false;
    }

    restore_vspace_and_sstatus();
    longjmp(s->jmp_buf, 1);
    UNREACHABLE();
}

#ifdef CONFIG_DEBUG_BUILD
exception_t handle_SysRiscvDebugReadUserWord(void)
{
    tcb_t *thread = NODE_STATE(ksCurThread);
    word_t user_addr = getRegister(thread, a0);
    word_t val;
    cap_t threadRoot = TCB_PTR_CTE_PTR(thread, tcbVTable)->cap;

    if (!isValidVTableRoot(threadRoot)) {
        setRegister(thread, a0, 0);
        setRegister(thread, a1, 0);
        return EXCEPTION_NONE;
    }

    pte_t *vspace_root = PTE_PTR(pptr_of_cap(threadRoot));
    asid_t asid = cap_page_table_cap_get_capPTMappedASID(threadRoot);
    paddr_t vspace_root_paddr = addrFromPPtr(vspace_root);

    bool_t ok = riscv_load_word_user((vptr_t)user_addr, &val, vspace_root_paddr, asid);
    setRegister(thread, a0, ok ? 1 : 0);
    setRegister(thread, a1, val);
    return EXCEPTION_NONE;
}
#endif

__attribute__((naked, noinline)) int setjmp(jmp_buf env) {
    asm volatile (
        "sd ra, 0(a0)\n"
        "sd sp, 8(a0)\n"
        "sd gp, 16(a0)\n"
        "sd tp, 24(a0)\n"
        "sd s0, 32(a0)\n"
        "sd s1, 40(a0)\n"
        "sd s2, 48(a0)\n"
        "sd s3, 56(a0)\n"
        "sd s4, 64(a0)\n"
        "sd s5, 72(a0)\n"
        "sd s6, 80(a0)\n"
        "sd s7, 88(a0)\n"
        "sd s8, 96(a0)\n"
        "sd s9, 104(a0)\n"
        "sd s10, 112(a0)\n"
        "sd s11, 120(a0)\n"
        "li a0, 0\n"
        "ret\n"
    );
}

__attribute__((naked, noinline)) void longjmp(jmp_buf env, int val) {
    asm volatile (
        "ld ra, 0(a0)\n"
        "ld sp, 8(a0)\n"
        "ld gp, 16(a0)\n"
        "ld tp, 24(a0)\n"
        "ld s0, 32(a0)\n"
        "ld s1, 40(a0)\n"
        "ld s2, 48(a0)\n"
        "ld s3, 56(a0)\n"
        "ld s4, 64(a0)\n"
        "ld s5, 72(a0)\n"
        "ld s6, 80(a0)\n"
        "ld s7, 88(a0)\n"
        "ld s8, 96(a0)\n"
        "ld s9, 104(a0)\n"
        "ld s10, 112(a0)\n"
        "ld s11, 120(a0)\n"
        "seqz a0, a1\n"
        "add a0, a0, a1\n"
        "ret\n"
    );
}
