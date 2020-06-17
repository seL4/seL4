/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_HARDWARE_DEBUG_API

#include <arch/machine/debug.h>

#define DEBUG_REPLY_N_EXPECTED_REGISTERS        (1)

/* Arch specific setup functions */
BOOT_CODE bool_t Arch_initHardwareBreakpoints(void);
void Arch_breakpointThreadDelete(tcb_t *thread);

/** Sets up (and overwrites) the current configuration of a hardware breakpoint.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 * @param vaddr Address that the breakpoint should be triggered by.
 * @param type Type of operation that should trigger the breakpoint.
 * @param size Operand size that should trigger the breakpoint.
 * @param rwx Access type (read/write) that should trigger the breakpoint.
 * @param uds If NULL, this function call will write directly to the hardware
 *            registers.
 *            If non-NULL, 'uds' is assumed to be a pointer to a debug register
 *            context-saving memory block, and this function will write to that
 *            context-saving memory block instead.
 */
void setBreakpoint(tcb_t *t,
                   uint16_t bp_num,
                   word_t vaddr, word_t type, word_t size, word_t rw);

/** Reads and returns the current configuration of a hardware breakpoint.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 *
 * @return Filled out getBreakpoint_t with the following fields:
 * @param vaddr[out] Address that the breakpoint is set to trigger on.
 * @param type[out] Type of operation that will trigger the breakpoint.
 * @param size[out] operand size that will trigger the breakpoint.
 * @param rw[out] Access type (read/write) that will trigger thr breakpoint.
 * @param uds If NULL, this function call will read directly from the hardware
 *            registers.
 *            If non-NULL, 'uds' is assumed to be a pointer to a debug register
 *            context-saving memory block, and this function will read from that
 *            context-saving memory block instead.
 * @param is_enabled Bool stating whether or not the breakpoint is enabled.
 */
typedef struct getBreakpointRet {
    word_t vaddr, type, size, rw;
    bool_t is_enabled;
} getBreakpoint_t;

getBreakpoint_t getBreakpoint(tcb_t *t, uint16_t bp_num);

/** Clears a breakpoint's configuration and disables it.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 * @param uds If NULL, this function call will write directly to the hardware
 *            registers.
 *            If non-NULL, 'uds' is assumed to be a pointer to a debug register
 *            context-saving memory block, and this function will write to that
 *            context-saving memory block instead.
 */
void unsetBreakpoint(tcb_t *t, uint16_t bp_num);

bool_t configureSingleStepping(tcb_t *t,
                               uint16_t bp_num,
                               word_t n_instr,
                               bool_t is_reply);

static inline bool_t singleStepFaultCounterReady(tcb_t *t)
{
    /* For a single-step exception, the user may have specified a certain
     * number of instructions to skip over before the next stop-point, so
     * we need to decrement the counter.
     *
     * We will check the counter's value when deciding whether or not to
     * actually send a fault message to userspace.
     */
    if (t->tcbArch.tcbContext.breakpointState.n_instructions > 0) {
        t->tcbArch.tcbContext.breakpointState.n_instructions--;
    }
    return t->tcbArch.tcbContext.breakpointState.n_instructions == 0;
}

#endif /* CONFIG_HARDWARE_DEBUG_API */

