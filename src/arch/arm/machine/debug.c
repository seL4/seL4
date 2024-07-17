/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#ifdef CONFIG_HARDWARE_DEBUG_API

#include <string.h>
#include <util.h>
#include <arch/model/statedata.h>
#include <arch/machine/debug.h>
#include <arch/machine/debug_conf.h>
#include <arch/kernel/vspace.h>
#include <arch/machine/registerset.h>
#include <armv/debug.h>
#include <mode/machine/debug.h>
#include <sel4/constants.h> /* seL4_NumExclusiveBreakpoints/Watchpoints */

/* ARMv7 Manuals, c3.3.1:
 *  "Breakpoint debug events are synchronous. That is, the debug event acts
 *  like an exception that cancels the breakpointed instruction."
 *
 * ARMv7 Manuals, c3.4.1:
 *  "Watchpoint debug events are precise and can be synchronous or asynchronous:
 *  a synchronous Watchpoint debug event acts like a synchronous abort
 *  exception on the memory access instruction itself. An asynchronous
 *  Watchpoint debug event acts like a precise asynchronous abort exception that
 *  cancels a later instruction."
 */

enum watchpoint_privilege /* WCR[2:1] */ {
    DBGWCR_PRIV_RESERVED = 0u,
    DBGWCR_PRIV_PRIVILEGED = 1u,
    DBGWCR_PRIV_USER = 2u,
    DBGWCR_PRIV_EITHER = 3u
};

enum watchpoint_access /* WCR[4:3] */ {
    DBGWCR_ACCESS_RESERVED = 0u,
    DBGWCR_ACCESS_LOAD = 1u,
    DBGWCR_ACCESS_STORE = 2u,
    DBGWCR_ACCESS_EITHER = 3u
};

#endif /* CONFIG_HARDWARE_DEBUG_API */

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE

/* These next few functions (read*Context()/write*Context()) read from TCB
 * context and not from the hardware registers.
 */
word_t readBcrContext(tcb_t *t, uint16_t index)
{
    assert(index < seL4_NumExclusiveBreakpoints);
    return t->tcbArch.tcbContext.breakpointState.breakpoint[index].cr;
}

static word_t readBvrContext(tcb_t *t, uint16_t index)
{
    assert(index < seL4_NumExclusiveBreakpoints);
    return t->tcbArch.tcbContext.breakpointState.breakpoint[index].vr;
}

static word_t readWcrContext(tcb_t *t, uint16_t index)
{
    assert(index < seL4_NumExclusiveWatchpoints);
    return t->tcbArch.tcbContext.breakpointState.watchpoint[index].cr;
}

static word_t readWvrContext(tcb_t *t, uint16_t index)
{
    assert(index < seL4_NumExclusiveWatchpoints);
    return t->tcbArch.tcbContext.breakpointState.watchpoint[index].vr;
}

void writeBcrContext(tcb_t *t, uint16_t index, word_t val)
{
    assert(index < seL4_NumExclusiveBreakpoints);
    t->tcbArch.tcbContext.breakpointState.breakpoint[index].cr = val;
}

void writeBvrContext(tcb_t *t, uint16_t index, word_t val)
{
    assert(index < seL4_NumExclusiveBreakpoints);
    t->tcbArch.tcbContext.breakpointState.breakpoint[index].vr = val;
}

static void writeWcrContext(tcb_t *t, uint16_t index, word_t val)
{
    assert(index < seL4_NumExclusiveWatchpoints);
    t->tcbArch.tcbContext.breakpointState.watchpoint[index].cr = val;
}

static void writeWvrContext(tcb_t *t, uint16_t index, word_t val)
{
    assert(index < seL4_NumExclusiveWatchpoints);
    t->tcbArch.tcbContext.breakpointState.watchpoint[index].vr = val;
}

#endif /* ARM_BASE_CP14_SAVE_AND_RESTORE */

#ifdef CONFIG_HARDWARE_DEBUG_API

/** For debugging: prints out the debug register pair values as returned by the
 * coprocessor.
 *
 * @param nBp Number of breakpoint reg pairs to print, starting at BP #0.
 * @param nBp Number of watchpoint reg pairs to print, starting at WP #0.
 */
UNUSED static void dumpBpsAndWpsCp(int nBp, int nWp)
{
    int i;

    for (i = 0; i < nBp; i++) {
        userError("CP BP %d: Bcr %lx, Bvr %lx", i, readBcrCp(i), readBvrCp(i));
    }

    for (i = 0; i < nWp; i++) {
        userError("CP WP %d: Wcr %lx, Wvr %lx", i, readWcrCp(i), readWvrCp(i));
    }
}

/** Print a thread's saved debug context. For debugging. This differs from
 * dumpBpsAndWpsCp in that it reads from a thread's saved register context, and
 * not from the hardware coprocessor registers.
 *
 * @param at arch_tcb_t where the thread's reg context is stored.
 * @param nBp Number of BP regs to print, beginning at BP #0.
 * @param mWp Number of WP regs to print, beginning at WP #0.
 */
UNUSED static void dumpBpsAndWpsContext(tcb_t *t, int nBp, int nWp)
{
    int i;

    for (i = 0; i < nBp; i++) {
        userError("Ctxt BP %d: Bcr %lx, Bvr %lx", i, readBcrContext(t, i), readBvrContext(t, i));
    }

    for (i = 0; i < nWp; i++) {
        userError("Ctxt WP %d: Wcr %lx, Wvr %lx", i, readWcrContext(t, i), readWvrContext(t, i));
    }
}

/* ARM allows watchpoint trigger on load, load-exclusive, and "swap" accesses.
 * store, store-exclusive and "swap" accesses. All accesses.
 *
 * The mask defines which bits are EXCLUDED from the comparison.
 * Always program the DBGDWVR with a WORD aligned address, and use the BAS to
 * state which bits form part of the match.
 *
 * It seems the BAS works as a bitmask of bytes to select in the range.
 *
 * To detect support for the 8-bit BAS field:
 *  * If the 8-bit BAS is unsupported, then BAS[7:4] is RAZ/WI.
 *
 * When using an 8-byte watchpoint that is not dword aligned, the result is
 * undefined. You should program it as the aligned base of the range, and select
 * only the relevant bytes then.
 *
 * You cannot do sparse byte selection: you either select a single byte in the
 * BAS or you select a contiguous range. ARM has deprecated sparse byte
 * selection.
 */

/** Convert an arch specific encoded watchpoint size back into a simple integer
 * representation.
 */
static word_t convertArchToSize(word_t archsize)
{
    switch (archsize) {
    case 0x1:
        return 1;
    case 0x3:
        return 2;
    case 0xFF:
        return 8;
    default:
        assert(archsize == 0xF);
        return 4;
    }
}

/** Convert an access perms API value (seL4_BreakOnRead, etc) into the register
 * encoding that matches it.
 */
static word_t convertAccessToArch(word_t access)
{
    switch (access) {
    case seL4_BreakOnRead:
        return DBGWCR_ACCESS_LOAD;
    case seL4_BreakOnWrite:
        return DBGWCR_ACCESS_STORE;
    default:
        assert(access == seL4_BreakOnReadWrite);
        return DBGWCR_ACCESS_EITHER;
    }
}

/** Convert an arch-specific register encoding back into an API access perms
 * value.
 */
static word_t convertArchToAccess(word_t archaccess)
{
    switch (archaccess) {
    case DBGWCR_ACCESS_LOAD:
        return seL4_BreakOnRead;
    case DBGWCR_ACCESS_STORE:
        return seL4_BreakOnWrite;
    default:
        assert(archaccess == DBGWCR_ACCESS_EITHER);
        return seL4_BreakOnReadWrite;
    }
}

uint16_t getBpNumFromType(uint16_t bp_num, word_t type)
{
    assert(type == seL4_InstructionBreakpoint || type == seL4_DataBreakpoint
           || type == seL4_SingleStep);

    switch (type) {
    case seL4_InstructionBreakpoint:
    case seL4_SingleStep:
        return bp_num;
    default: /* seL4_DataBreakpoint: */
        assert(type == seL4_DataBreakpoint);
        return bp_num + seL4_NumExclusiveBreakpoints;
    }
}

/** Sets up the requested hardware breakpoint register.
 *
 * Acts as the backend for seL4_TCB_SetBreakpoint. Doesn't actually operate
 * on the hardware coprocessor, but just modifies the thread's debug register
 * context. The thread will pop off the updated register context when it is
 * popping its context the next time it runs.
 *
 * On ARM the hardware breakpoints are consumed by all operations, including
 * single-stepping, unlike x86, where single-stepping doesn't require the use
 * of an actual hardware breakpoint register (just uses the EFLAGS.TF bit).
 *
 * @param at arch_tcb_t that points to the register context of the thread we
 *           want to modify.
 * @param bp_num The hardware register we want to set up.
 * @params vaddr, type, size, rw: seL4 API values for seL4_TCB_SetBreakpoint.
 *         All documented in the seL4 API Manuals.
 */
void setBreakpoint(tcb_t *t,
                   uint16_t bp_num,
                   word_t vaddr, word_t type, word_t size, word_t rw)
{
    bp_num = convertBpNumToArch(bp_num);

    /* C3.3.4: "A debugger can use either byte address selection or address range
     *  masking, if it is implemented. However, it must not attempt to use both at
     * the same time"
     *
     * "v7 Debug and v7.1 Debug deprecate any use of the DBGBCR.MASK field."
     * ^ So prefer to use DBGBCR.BAS instead. When using masking, you must set
     * BAS to all 1s, and when using BAS you must set the MASK field to all 0s.
     *
     * To detect support for BPAddrMask:
     *  * When it's unsupported: DBGBCR.MASK is always RAZ/WI, and EITHER:
     *      * DBGIDR.DEVID_tmp is RAZ
     *      * OR DBGIDR.DEVID_tmp is RAO and DBGDEVID.{CIDMask, BPAddrMask} are RAZ.
     *  * OR:
     *      * DBGDEVID.BPAddrMask indicates whether addr masking is supported.
     *      * DBGBCR.MASK is UNK/SBZP.
     *
     * Setting BAS to 0b0000 makes the cpu break on every instruction.
     * Be aware that the processor checks the MASK before the BAS.
     * You must set BAS to 0b1111 for all context match comparisons.
     */
    if (type == seL4_InstructionBreakpoint) {
        dbg_bcr_t bcr;

        writeBvrContext(t, bp_num, vaddr);

        /* Preserve reserved bits. */
        bcr.words[0] = readBcrContext(t, bp_num);
        bcr = dbg_bcr_set_enabled(bcr, 1);
        bcr = dbg_bcr_set_lbn(bcr, 0);
        bcr = dbg_bcr_set_pmc(bcr, DBGBCR_PRIV_USER);
        bcr = dbg_bcr_set_hmc(bcr, 0);
        bcr = dbg_bcr_set_ssc(bcr, 0);
        bcr = dbg_bcr_set_bas(bcr, convertSizeToArch(4));
        bcr = Arch_setupBcr(bcr, true);
        writeBcrContext(t, bp_num, bcr.words[0]);
    } else {
        dbg_wcr_t wcr;

        writeWvrContext(t, bp_num, vaddr);

        /* Preserve reserved bits */
        wcr.words[0] = readWcrContext(t, bp_num);
        wcr = dbg_wcr_set_enabled(wcr, 1);
        wcr = dbg_wcr_set_pac(wcr, DBGWCR_PRIV_USER);
        wcr = dbg_wcr_set_bas(wcr, convertSizeToArch(size));
        wcr = dbg_wcr_set_lsc(wcr, convertAccessToArch(rw));
        wcr = dbg_wcr_set_watchpointType(wcr, 0);
        wcr = dbg_wcr_set_lbn(wcr, 0);
        wcr = dbg_wcr_set_addressMask(wcr, 0);
        wcr = dbg_wcr_set_hmc(wcr, 0);
        wcr = dbg_wcr_set_ssc(wcr, 0);
        writeWcrContext(t, bp_num, wcr.words[0]);
    }
}

/** Retrieves the current configuration of a hardware breakpoint for a given
 * thread.
 *
 * Doesn't modify the configuration of that thread's breakpoints.
 *
 * @param at arch_tcb_t that holds the register context for the thread you wish
 *           to query.
 * @param bp_num Hardware breakpoint ID.
 * @return A struct describing the current configuration of the requested
 *         breakpoint.
 */
getBreakpoint_t getBreakpoint(tcb_t *t, uint16_t bp_num)
{
    getBreakpoint_t ret;

    ret.type = getTypeFromBpNum(bp_num);
    bp_num = convertBpNumToArch(bp_num);

    if (ret.type == seL4_InstructionBreakpoint) {
        dbg_bcr_t bcr;

        bcr.words[0] = readBcrContext(t, bp_num);
        if (Arch_breakpointIsSingleStepping(t, bp_num)) {
            ret.type = seL4_SingleStep;
        };
        ret.size = 0;
        ret.rw = seL4_BreakOnRead;
        ret.vaddr = readBvrContext(t, bp_num);
        ret.is_enabled = dbg_bcr_get_enabled(bcr);
    } else {
        dbg_wcr_t wcr;

        wcr.words[0] = readWcrContext(t, bp_num);
        ret.size = convertArchToSize(dbg_wcr_get_bas(wcr));
        ret.rw = convertArchToAccess(dbg_wcr_get_lsc(wcr));
        ret.vaddr = readWvrContext(t, bp_num);
        ret.is_enabled = dbg_wcr_get_enabled(wcr);
    }
    return ret;
}

/** Disables and clears the configuration of a hardware breakpoint.
 *
 * @param at arch_tcb_t holding the reg context for the target thread.
 * @param bp_num The hardware breakpoint you want to disable+clear.
 */
void unsetBreakpoint(tcb_t *t, uint16_t bp_num)
{
    word_t type;

    type = getTypeFromBpNum(bp_num);
    bp_num = convertBpNumToArch(bp_num);

    if (type == seL4_InstructionBreakpoint) {
        dbg_bcr_t bcr;

        bcr.words[0] = readBcrContext(t, bp_num);
        bcr = dbg_bcr_set_enabled(bcr, 0);
        writeBcrContext(t, bp_num, bcr.words[0]);
        writeBvrContext(t, bp_num, 0);
    } else {
        dbg_wcr_t wcr;

        wcr.words[0] = readWcrContext(t, bp_num);
        wcr = dbg_wcr_set_enabled(wcr, 0);
        writeWcrContext(t, bp_num, wcr.words[0]);
        writeWvrContext(t, bp_num, 0);
    }
}

/** Load an initial, all-disabled setup state for the registers.
 */
BOOT_CODE void disableAllBpsAndWps(void)
{
    int i;

    for (i = 0; i < seL4_NumExclusiveBreakpoints; i++) {
        writeBvrCp(i, 0);
        writeBcrCp(i, readBcrCp(i) & ~DBGBCR_ENABLE);
    }
    for (i = 0; i < seL4_NumExclusiveWatchpoints; i++) {
        writeWvrCp(i, 0);
        writeWcrCp(i, readWcrCp(i) & ~DBGWCR_ENABLE);
    }

    isb();
}

/** Determines which breakpoint or watchpoint register caused the debug
 * exception to be triggered.
 *
 * Checks to see which hardware breakpoint was triggered, and saves
 * the ID of that breakpoint.
 * There is no short way to do this on ARM. On x86 there is a status
 * register that tells you which watchpoint has been triggered. On ARM
 * there is no such register, so you have to manually check each to see which
 * one was triggered.
 *
 * The arguments also work a bit differently from x86 as well. On x86 the
 * 2 arguments are dummy values, while on ARM, they contain useful information.
 *
 * @param vaddr The virtual address stored in the IFSR/DFSR register, which
 *              is either the watchpoint address or breakpoint address.
 * @param reason The presumed reason for the exception, which is based on
 *               whether it was a prefetch or data abort.
 * @return Struct with a member "bp_num", which is a positive integer if we
 *         successfully detected which debug register triggered the exception.
 *         "Bp_num" will be negative otherwise.
 */
int getAndResetActiveBreakpoint(word_t vaddr, word_t reason)
{
    word_t align_mask;
    int i, ret = -1;

    if (reason == seL4_InstructionBreakpoint) {
        for (i = 0; i < seL4_NumExclusiveBreakpoints; i++) {
            dbg_bcr_t bcr;
            word_t bvr = readBvrCp(i);

            bcr.words[0] = readBcrCp(i);
            /* The actual trigger address may be an unaligned sub-byte of the
             * range, which means it's not guaranteed to match the aligned value
             * that was programmed into the address register.
             */
            align_mask = convertArchToSize(dbg_bcr_get_bas(bcr));
            align_mask = ~(align_mask - 1);

            if (bvr != (vaddr & align_mask) || !dbg_bcr_get_enabled(bcr)) {
                continue;
            }

            ret = i;
            return ret;
        }
    }

    if (reason == seL4_DataBreakpoint) {
        for (i = 0; i < seL4_NumExclusiveWatchpoints; i++) {
            dbg_wcr_t wcr;
            word_t wvr = readWvrCp(i);

            wcr.words[0] = readWcrCp(i);
            align_mask = convertArchToSize(dbg_wcr_get_bas(wcr));
            align_mask = ~(align_mask - 1);

            if (wvr != (vaddr & align_mask) || !dbg_wcr_get_enabled(wcr)) {
                continue;
            }

            ret = i;
            return ret;
        }
    }

    return ret;
}

#endif /* CONFIG_HARDWARE_DEBUG_API */

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE

/** Mirrors Arch_initFpuContext.
 *
 * Zeroes out the BVR thread context and preloads reserved bit values from the
 * control regs into the thread context so we can operate solely on the values
 * cached in RAM in API calls, rather than retrieving the values from the
 * coprocessor.
 */
void Arch_initBreakpointContext(user_context_t *uc)
{
    uc->breakpointState = armKSNullBreakpointState;
}

void loadAllDisabledBreakpointState(void)
{
    int i;

    /* We basically just want to read-modify-write each reg to ensure its
     * "ENABLE" bit is clear. We did preload the register context with the
     * reserved values from the control registers, so we can read our
     * initial values from either the coprocessor or the thread's register
     * context.
     *
     * Both are perfectly fine, and the only discriminant factor is performance.
     * I suspect that reading from RAM is faster than reading from the
     * coprocessor, but I can't be sure.
     */
    for (i = 0; i < seL4_NumExclusiveBreakpoints; i++) {
        writeBcrCp(i, readBcrCp(i) & ~DBGBCR_ENABLE);
    }
    for (i = 0; i < seL4_NumExclusiveWatchpoints; i++) {
        writeWcrCp(i, readWcrCp(i) & ~DBGWCR_ENABLE);
    }
}

/* We only need to save the breakpoint state in the hypervisor
 * build, and only for threads that have an associated VCPU.
 *
 * When the normal kernel is running with the debug API, all
 * changes to the debug regs are done through the debug API.
 * In the hypervisor build, the guest VM has full access to the
 * debug regs in PL1, so we need to save its values on vmexit.
 *
 * When saving the debug regs we will always save all of them.
 * When restoring, we will restore only those that have been used
 * for native threads; and we will restore all of them
 * unconditionally for VCPUs (because we don't know which of
 * them have been changed by the guest).
 *
 * To ensure that all the debug regs are restored unconditionally,
 * we just set the "used_breakpoints_bf" bitfield to all 1s in
 * associateVcpu.
 */
void saveAllBreakpointState(tcb_t *t)
{
    int i;

    assert(t != NULL);

    for (i = 0; i < seL4_NumExclusiveBreakpoints; i++) {
        writeBvrContext(t, i, readBvrCp(i));
        writeBcrContext(t, i, readBcrCp(i));
    }

    for (i = 0; i < seL4_NumExclusiveWatchpoints; i++) {
        writeWvrContext(t, i, readWvrCp(i));
        writeWcrContext(t, i, readWcrCp(i));
    }
}

#ifdef ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
void Arch_debugAssociateVCPUTCB(tcb_t *t)
{
    /* Don't attempt to shift beyond end of word. */
    assert(seL4_NumHWBreakpoints < sizeof(word_t) * 8);

    /* Set all the bits to 1, so loadBreakpointState() will
     * restore all the debug regs unconditionally.
     */
    t->tcbArch.tcbContext.breakpointState.used_breakpoints_bf = MASK(seL4_NumHWBreakpoints);
}

void Arch_debugDissociateVCPUTCB(tcb_t *t)
{
    t->tcbArch.tcbContext.breakpointState.used_breakpoints_bf = 0;
}
#endif

static void loadBreakpointState(tcb_t *t)
{
    int i;

    assert(t != NULL);

    for (i = 0; i < seL4_NumExclusiveBreakpoints; i++) {
        if (t->tcbArch.tcbContext.breakpointState.used_breakpoints_bf & BIT(i)) {
            writeBvrCp(i, readBvrContext(t, i));
            writeBcrCp(i, readBcrContext(t, i));
        } else {
            /* If the thread isn't using the BP, then just load
             * a default "disabled" state.
             */
            writeBcrCp(i, readBcrCp(i) & ~DBGBCR_ENABLE);
        }
    }

    for (i = 0; i < seL4_NumExclusiveWatchpoints; i++) {
        if (t->tcbArch.tcbContext.breakpointState.used_breakpoints_bf &
            BIT(i + seL4_NumExclusiveBreakpoints)) {
            writeWvrCp(i, readWvrContext(t, i));
            writeWcrCp(i, readWcrContext(t, i));
        } else {
            writeWcrCp(i, readWcrCp(i) & ~DBGWCR_ENABLE);
        }
    }
}

/** Pops debug register context for a thread into the CPU.
 *
 * Mirrors the idea of restore_user_context.
 */
void restore_user_debug_context(tcb_t *target_thread)
{
    assert(target_thread != NULL);

    if (target_thread->tcbArch.tcbContext.breakpointState.used_breakpoints_bf == 0) {
        loadAllDisabledBreakpointState();
    } else {
        loadBreakpointState(target_thread);
    }

    /* ARMv7 manual, sec C3.7:
     * "Usually, an exception return sequence is a context change operation as
     * well as a context synchronization operation, in which case the context
     * change operation is guaranteed to take effect on the debug logic by the
     * end of that exception return sequence."
     *
     * So we don't need to execute ISB here because we're about to RFE.
     */

#ifdef CONFIG_ARCH_AARCH64
    aarch64_restore_user_debug_context(target_thread);
#endif /* CONFIG_ARCH_ARCH64 */
}

#endif /* ARM_BASE_CP14_SAVE_AND_RESTORE */
