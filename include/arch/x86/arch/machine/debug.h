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

#include <config.h>
#ifdef CONFIG_HARDWARE_DEBUG_API

#include <types.h>
#include <api/types.h>
#include <arch/machine/registerset.h>
#include <mode/machine/debug.h>

#define X86_EFLAGS_TRAP_FLAG_SHIFT   (8u)

/* Bit in DR7 that will enable each BP respectively. */
#define X86_DEBUG_BP0_ENABLE_BIT  ((word_t)BIT(1))
#define X86_DEBUG_BP1_ENABLE_BIT  ((word_t)BIT(3))
#define X86_DEBUG_BP2_ENABLE_BIT  ((word_t)BIT(5))
#define X86_DEBUG_BP3_ENABLE_BIT  ((word_t)BIT(7))

/** Per-thread initial state setting.
 *
 * The most significant thing done here is that we pre-load reserved bits from
 * the hardware registers into the TCB context.
 *
 * @param context TCB breakpoint context for the thread being initialized.
 */
void Arch_initBreakpointContext(user_breakpoint_state_t *context);

/** Discerns and handles a debug exception.
 *
 * Determines which hardware breakpoint triggered a debug exception, and
 * generates a message to userspace for that breakpoint exception, or generates
 * a message to userspace for a single-step exception, if it was a single-step
 * event that triggered the exception.
 *
 * ARM's exception-path flow works differently.
 *
 * @param int_vector Processor-level vector number on which the exception
 *                   occured. May be 1 or 3, depending on whether the exception
 *                   is a breakpoint, single-step, or INT3 exception.
 */
exception_t handleUserLevelDebugException(int int_vector);

/** These next two functions are part of some state flags.
 *
 * A bitfield of all currently enabled breakpoints for a thread is kept in that
 * thread's TCB. These two functions here set and unset the bits in that
 * bitfield.
 */
static inline void
setBreakpointUsedFlag(arch_tcb_t *uds, uint16_t bp_num)
{
    if (uds != NULL) {
        uds->tcbContext.breakpointState.used_breakpoints_bf |= BIT(bp_num);
    }
}

static inline void
unsetBreakpointUsedFlag(arch_tcb_t *uds, uint16_t bp_num)
{
    if (uds != NULL) {
        uds->tcbContext.breakpointState.used_breakpoints_bf &= ~BIT(bp_num);
    }
}

/** Program the debug registers with values that will disable all breakpoints.
 *
 * This is an optimization for threads that don't use any breakpoints: we won't
 * try to pop all the context from a block of memory, but just unset all the
 * "enable" bits in the registers.
 * @param at arch_tcb_t from which the reserved bits will be loaded before
 *           setting the disable bits.
 */
static void
loadAllDisabledBreakpointState(arch_tcb_t *at)
{
    word_t disable_value;

    disable_value = at->tcbContext.breakpointState.dr[5];
    disable_value &= ~(X86_DEBUG_BP0_ENABLE_BIT | X86_DEBUG_BP1_ENABLE_BIT
                       | X86_DEBUG_BP2_ENABLE_BIT | X86_DEBUG_BP3_ENABLE_BIT);

    writeDr7Reg(disable_value);
}

static inline void
restore_user_debug_context(tcb_t *target_thread)
{
    arch_tcb_t *uds = &target_thread->tcbArch;
    if (uds->tcbContext.breakpointState.used_breakpoints_bf != 0) {
        loadBreakpointState(uds);
    } else {
        loadAllDisabledBreakpointState(uds);
    }

    /* If single-stepping was enabled, we need to re-set the TF flag as well. */
    if (uds->tcbContext.breakpointState.single_step_enabled == true) {
#ifdef CONFIG_ARCH_IA32
        uds->tcbContext.registers[FLAGS] |= BIT(X86_EFLAGS_TRAP_FLAG_SHIFT);
#else
        uds->tcbContext.registers[FLAGS] |= BIT(X86_EFLAGS_TRAP_FLAG_SHIFT);
#endif
    }
}

static inline syscall_error_t
Arch_decodeConfigureSingleStepping(arch_tcb_t *uc,
                                   uint16_t bp_num,
                                   word_t n_instr,
                                   bool_t is_reply)
{
    syscall_error_t ret;

    ret.type = seL4_NoError;
    return ret;
}

bool_t byte8BreakpointsSupported(void);

static inline syscall_error_t
Arch_decodeSetBreakpoint(arch_tcb_t *uds,
                         uint16_t bp_num, word_t vaddr, word_t types,
                         word_t size, word_t rw)
{
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        userError("Debug: invalid bp_num %u.", bp_num);
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
        return ret;
    }
    if (size == 8 && !byte8BreakpointsSupported()) {
        userError("Debug: 8-byte breakpoints/watchpoints unsupported on this CPU.");
        ret.invalidArgumentNumber = 3;
        ret.type = seL4_InvalidArgument;
        return ret;
    }
    return ret;
}

static inline syscall_error_t
Arch_decodeGetBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
{
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        userError("Debug: invalid bp_num %u.", bp_num);
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
    }
    return ret;
}

static inline syscall_error_t
Arch_decodeUnsetBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
{
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        userError("Debug: invalid bp_num %u.", bp_num);
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
    }
    return ret;
}

#endif /* CONFIG_HARDWARE_DEBUG_API */
