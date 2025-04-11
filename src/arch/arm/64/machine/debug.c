/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#ifdef CONFIG_HARDWARE_DEBUG_API

#include <machine/debug.h>
#include <arch/kernel/vspace.h>
#include <arch/machine/debug.h>
#include <arch/machine/debug_conf.h>
#include <arch/machine/registerset.h>
#include <arch/model/statedata.h>
#include <armv/debug.h>
#include <mode/machine/debug.h>
#include <sel4/constants.h> /* seL4_NumExclusiveBreakpoints/Watchpoints */
#include <string.h>
#include <util.h>

#define MDSCR_MDE (BIT(15))
#define MDSCR_SS  (BIT(0))
#define SPSR_SS   (BIT(21))

#define ESR_EXCEPTION_CLASS_MASK 0xFC000000
#define ESR_EXCEPTION_CLASS_OFF 26

#define DEBUG_ENTRY_BREAKPOINT      0x30
#define DEBUG_ENTRY_SINGLE_STEP     0x32
#define DEBUG_ENTRY_WATCHPOINT      0x34
#define DEBUG_ENTRY_EXPLICIT_BKPT   0x3C

#define OSDLR_LOCK (BIT(0))
#define OSLAR_LOCK (BIT(0))

bool_t byte8WatchpointsSupported(void)
{
    return true;
}

exception_t handleDebugFaultEvent(word_t esr)
{
    MCS_DO_IF_BUDGET({
        current_fault = handleUserLevelDebugException(esr, getRestartPC(NODE_STATE(ksCurThread)));
        if (seL4_Fault_get_seL4_FaultType(current_fault) != seL4_Fault_NullFault)
        {
            handleFault(NODE_STATE(ksCurThread));
        }
    })
    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

/** Initiates or halts single-stepping on the target process.
 *
 * @param at arch_tcb_t for the target process to be configured.
 * @param bp_num The hardware ID of the breakpoint register to be used.
 * @param n_instr The number of instructions to step over.
 */
bool_t configureSingleStepping(tcb_t *t, uint16_t bp_num, word_t n_instr,
                               bool_t is_reply)
{
    if (n_instr > 0) {
        /* Enable single stepping */
        t->tcbArch.tcbContext.breakpointState.single_step_enabled = true;
    } else {
        /* Disable single stepping */
        t->tcbArch.tcbContext.breakpointState.single_step_enabled = false;
    }

    t->tcbArch.tcbContext.breakpointState.n_instructions = n_instr;
    return true;
}

/* Guides the debug hardware initialization sequence. */
BOOT_CODE bool_t Arch_initHardwareBreakpoints(void)
{
    /*
     * ARMv8 Architecture Reference Manual for A-profile Architecture
     * D2.2: The Enable controls for each debug exception are:
     *    ... MDSCR_EL1.MDE
     */

    word_t mdscr = 0;
    MRS("MDSCR_EL1", mdscr);
    mdscr |= MDSCR_MDE;
    MSR("MDSCR_EL1", mdscr);

    /*
     * ARMv8 Architecture Reference Manual for A-profile Architecture
     * D2.4: A debug exception can be taken only if all the following are true:
     *    - The OS Lock is unlocked
     *    - DoubleLockStatus() = False
     *    - The debug exception is enabled from the current exception level
     *    - The debug exception is enabled from the current security state
     */

    /* Ensure that the OS double lock is unset */
    word_t osdlr = 0;
    MRS("osdlr_el1", osdlr);
    osdlr &= ~OSDLR_LOCK;
    MSR("osdlr_el1", osdlr);

    /* Ensure that the OS lock is unset */
    word_t oslar = 0;
    MSR("oslar_el1", oslar);

    /* Ensure that all the breakpoint and watchpoint registers are initially disabled */
    disableAllBpsAndWps();

    /* Ensure that single stepping is initially disabled */
    MRS("MDSCR_EL1", mdscr);
    mdscr &= ~MDSCR_SS;
    MSR("MDSCR_EL1", mdscr);

    /* Finally, also pre-load some initial register state that can be used
     * for all new threads so that their initial saved debug register state
     * is valid when it's first loaded onto the CPU.
     */
    for (int i = 0; i < seL4_NumExclusiveBreakpoints; i++) {
        armKSNullBreakpointState.breakpoint[i].cr = readBcrCp(i) & ~DBGBCR_ENABLE;
    }
    for (int i = 0; i < seL4_NumExclusiveWatchpoints; i++) {
        armKSNullBreakpointState.watchpoint[i].cr = readWcrCp(i) & ~DBGWCR_ENABLE;
    }

    return true;
}

/* Abstract wrapper around the ESR fault status value */

static word_t getFaultStatus(word_t esr)
{
    return (esr & ESR_EXCEPTION_CLASS_MASK) >> ESR_EXCEPTION_CLASS_OFF;
}

/** Called to determine if an abort was a debug exception.
 *
 * The ARM debug exceptions look like Prefetch Aborts or Data Aborts, and you
 * have to examine some extra register state to determine whether or not the
 * abort you currently have on your hands is actually a debug exception.
 *
 * This routine takes care of the checks.
 * @param fs An abstraction of the DFSR/IFSR values, meant to make it irrelevant
 *           whether we're using the long/short descriptors. Bit positions and
 *           values change. This also makes the debug code forward compatible
 *           aarch64.
 */
bool_t isDebugFault(word_t esr)
{
    word_t exception_class = getFaultStatus(esr);
    return (exception_class == DEBUG_ENTRY_BREAKPOINT ||
            exception_class == DEBUG_ENTRY_SINGLE_STEP ||
            exception_class == DEBUG_ENTRY_WATCHPOINT ||
            exception_class == DEBUG_ENTRY_EXPLICIT_BKPT);
}

/** Called to process a debug exception.
 *
 * On x86, you're told which breakpoint register triggered the exception. On
 * ARM, you're told the virtual address that triggered the exception and what
 * type of access (data access vs instruction execution) triggered the exception
 * and you have to figure out which register triggered it.
 *
 * For watchpoints, it's not very complicated: just check to see which
 * register matches the virtual address.
 *
 * For breakpoints, it's a bit more complex: since both breakpoints and single-
 * stepping are configured using the same registers, we need to first detect
 * whether single-stepping is enabled. If not, then we check for a breakpoint.
 * @param fault_vaddr The instruction vaddr which triggered the exception, as
 *                    extracted by the kernel.
 */
seL4_Fault_t handleUserLevelDebugException(word_t esr, word_t fault_vaddr)
{
    int active_bp;
    word_t bp_reason, bp_vaddr;
    word_t exception_class = getFaultStatus(esr);

#ifdef TRACK_KERNEL_ENTRIES
    ksKernelEntry.path = Entry_DebugFault;
    ksKernelEntry.word = exception_class;
#endif

    switch (exception_class) {
    case DEBUG_ENTRY_BREAKPOINT:
        bp_reason = seL4_InstructionBreakpoint;
        bp_vaddr = fault_vaddr;
        break;
    case DEBUG_ENTRY_WATCHPOINT:
        bp_reason = seL4_DataBreakpoint;
        bp_vaddr = getFAR();
        break;
    case DEBUG_ENTRY_SINGLE_STEP:
        bp_reason = seL4_SingleStep;
        bp_vaddr = fault_vaddr;
        active_bp = 0;
        break;
    default: /* EXPLICIT_BKPT: BKPT instruction */
        assert(exception_class == DEBUG_ENTRY_EXPLICIT_BKPT);
        bp_reason = seL4_SoftwareBreakRequest;
        bp_vaddr = fault_vaddr;
        active_bp = 0;
    }

    /* There is no hardware register associated with BKPT instruction
    * triggers or single stepping.
    */
    if (bp_reason != seL4_SoftwareBreakRequest && bp_reason != seL4_SingleStep) {
        active_bp = getAndResetActiveBreakpoint(bp_vaddr, bp_reason);
        active_bp = getBpNumFromType(active_bp, bp_reason);
        assert(active_bp >= 0);
    }

    if (bp_reason == seL4_SingleStep && !singleStepFaultCounterReady(NODE_STATE(ksCurThread))) {
        return seL4_Fault_NullFault_new();
    }

    return seL4_Fault_DebugException_new(bp_vaddr, active_bp, bp_reason);
}

#endif /* CONFIG_HARDWARE_DEBUG_API */

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE

/** Pops debug register context for a thread into the CPU.
 *
 * Mirrors the idea of restore_user_context.
 */
void aarch64_restore_user_debug_context(tcb_t *target_thread)
{
    assert(target_thread != NULL);

    /* Set/unset single stepping if applicable */
    word_t mdscr = 0, spsr = 0;
    MRS("MDSCR_EL1", mdscr);
    spsr = getRegister(target_thread, SPSR_EL1);
    if (target_thread->tcbArch.tcbContext.breakpointState.single_step_enabled) {
        /* Enable single stepping */
        mdscr |= MDSCR_SS;
        spsr |= SPSR_SS;
    } else {
        /* Disable single stepping */
        mdscr &= ~MDSCR_SS;
        spsr &= ~SPSR_SS;
    }
    MSR("MDSCR_EL1", mdscr);
    setRegister(target_thread, SPSR_EL1, spsr);
}

#endif /* ARM_BASE_CP14_SAVE_AND_RESTORE */
