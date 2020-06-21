/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#ifdef CONFIG_HARDWARE_DEBUG_API

#define DBGVCR_RESERVED_BITS_MASK      (0xFFFFFFF0|BIT(5))

enum v6_breakpoint_meaning /* BCR[22:21] */ {
    DBGBCR_V6MEANING_INSTRUCTION_VADDR_MATCH = 0u,
    DBGBCR_V6MEANING_CONTEXT_ID_MATCH = 1u,
    DBGBCR_V6MEANING_INSTRUCTION_VADDR_MISMATCH = 2u
};

/** Read DBGDSCR from CP14.
 *
 * DBGDSCR_ext (external view) is not exposed on debug v6. Accessing it on
 * v6 triggers an #UNDEFINED abort.
 */
static word_t readDscrCp(void)
{
    word_t v;

    MRC(DBGDSCR_int, v);
    return v;
}

/** Write DBGDSCR (Status and control register).
 *
 * On ARMv6, there is no mmapping, and the coprocessor doesn't expose an
 * external vs internal view of DSCR. There's only the internal, but the MDBGEn
 * but is RW (as opposed to V7 where the internal MDBGEn is RO).
 *
 * Even so, the KZM still ignores our writes anyway *shrug*.
 */
static void writeDscrCp(word_t val)
{
    MCR(DBGDSCR_int, val);
}

/** Determines whether or not 8-byte watchpoints are supported.
 */
static inline bool_t watchpoint8bSupported(void)
{
    /* V6 doesn't support 8B watchpoints. */
    return false;
}

/** Enables the debug architecture mode that allows us to receive debug events
 * as exceptions.
 *
 * CPU can operate in one of 2 debug architecture modes: "halting" and
 * "monitor". In halting mode, when a debug event occurs, the CPU will halt
 * execution and enter a special state in which it can be examined by an
 * external debugger dongle.
 *
 * In monitor mode, the CPU will deliver debug events to the kernel as
 * exceptions. Monitor mode is what's actually useful to us. If it's not
 * supported by the CPU, it's impossible for the API to work.
 *
 * Unfortunately, it's also gated behind a hardware pin signal, #DBGEN. If
 * #DBGEN is held low, monitor mode is unavailable.
 */
BOOT_CODE static bool_t enableMonitorMode(void)
{
    dbg_dscr_t dscr;

    dscr.words[0] = readDscrCp();
    /* HDBGEn is read-only on v6 debug. */
    if (dbg_dscr_get_haltingDebugEnable(dscr) != 0) {
        printf("Halting debug is enabled, and can't be disabled. Monitor mode "
               "unavailable.\n");
        return false;
    }

    dscr = dbg_dscr_set_monitorDebugEnable(dscr, 1);
    writeDscrCp(dscr.words[0]);
    isb();

    /* On V6 debug, we can tell if the #DBGEN signal is enabled by setting
     * the DBGDSCR.MDBGEn bit. If the #DBGEN signal is not enabled, writes
     * to DBGDSCR.MDBGEn will be ignored, and it will always read as zero.
     *
     * We test here to see if the DBGDSCR.MDBGEn bit is still 0, even after
     * we set it to 1 in enableMonitorMode().
     *
     * ARMv6 manual, sec D3.3.2, "Monitor debug-mode enable, bit[15]":
     *
     *  "Monitor debug-mode has to be both selected and enabled (bit 14
     *  clear and bit 15 set) for the core to take a Debug exception."
     *
     *  "If the external interface input DBGEN is low, DSCR[15:14] reads as
     *  0b00. The programmed value is masked until DBGEN is taken high, at
     *  which time value is read and behavior reverts to the programmed
     *  value."
     */
    /* Re-read the value */
    dscr.words[0] = readDscrCp();
    if (dbg_dscr_get_monitorDebugEnable(dscr) == 0) {
        printf("#DBGEN signal held low. Monitor mode unavailable.\n");
        return false;
    }
    return true;
}

static inline dbg_bcr_t Arch_setupBcr(dbg_bcr_t in_val, bool_t is_match)
{
    dbg_bcr_t bcr;

    if (is_match) {
        bcr = dbg_bcr_set_meaning(in_val, DBGBCR_V6MEANING_INSTRUCTION_VADDR_MATCH);
    } else {
        bcr = dbg_bcr_set_meaning(in_val, DBGBCR_V6MEANING_INSTRUCTION_VADDR_MISMATCH);
    }
    bcr = dbg_bcr_set_enableLinking(bcr, 0);
    return bcr;
}

static inline dbg_wcr_t Arch_setupWcr(dbg_wcr_t in_val)
{
    return in_val;
}

static inline bool_t Arch_breakpointIsMismatch(dbg_bcr_t in_val)
{
    return dbg_bcr_get_meaning(in_val) == DBGBCR_V6MEANING_INSTRUCTION_VADDR_MISMATCH;
}

#endif /* CONFIG_HARDWARE_DEBUG_API */
