/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#if defined(CONFIG_HARDWARE_DEBUG_API) || defined(CONFIG_EXPORT_PMU_USER) || defined(CONFIG_ENABLE_BENCHMARKS)

#include <mode/machine/debug.h>
#include <mode/machine.h> /* MRC/MCR */

/** Read DBGDSCR from CP14.
 */
static inline word_t readDscrCp(void)
{
    word_t v;
#ifdef CONFIG_ARM_CORTEX_A8
    MRC(DBGDSCR_int, v);
#else
    MRC(DBGDSCR_ext, v);
#endif
    return v;
}

/** Write DBGDSCR (Status and control register).
 * On ARMv7, the external view of the CP14 DBGDSCR register is preferred since
 * the internal view is fully read-only.
 */
static inline void writeDscrCp(word_t val)
{
    MCR(DBGDSCR_ext, val);
}
#endif /* CONFIG_HARDWARE_DEBUG_API CONFIG_EXPORT_PMU_USER */

#ifdef CONFIG_HARDWARE_DEBUG_API
#define DBGVCR_RESERVED_BITS_MASK      \
                        (BIT(5)|BIT(8)|BIT(9)|BIT(13)|BIT(16)|BIT(24)|BIT(29))

#define DBGWCR_BAS_HIGH_SHIFT         (9u)
#define DBGWCR_0 "p14,0,%0,c0,c0,7"

enum v7_breakpoint_type {
    DBGBCR_TYPE_UNLINKED_INSTRUCTION_MATCH = 0u,
    DBGBCR_TYPE_LINKED_INSTRUCTION_MATCH = 0x1u,
    DBGBCR_TYPE_UNLINKED_CONTEXT_MATCH = 0x2u,
    DBGBCR_TYPE_LINKED_CONTEXT_MATCH = 0x3u,

    DBGBCR_TYPE_UNLINKED_INSTRUCTION_MISMATCH = 0x4u,
    DBGBCR_TYPE_LINKED_INSTRUCTION_MISMATCH = 0x5u,

    DBGBCR_TYPE_UNLINKED_VMID_MATCH = 0x8u,
    DBGBCR_TYPE_LINKED_VMID_MATCH = 0x9u,
    DBGBCR_TYPE_UNLINKED_VMID_AND_CONTEXT_MATCH = 0xAu,
    DBGBCR_TYPE_LINKED_VMID_AND_CONTEXT_MATCH = 0xBu
};


/** Determines whether or not 8-byte watchpoints are supported.
 *
 * Checks to see if the 8-byte byte-address-select high bits ignore writes.
 */
static inline bool_t watchpoint8bSupported(void)
{
    word_t wcrtmp;

    /* ARMv7 manual: C11.11.44:
     * "A 4-bit Byte address select field is DBGWCR[8:5]. DBGWCR[12:9] is RAZ/WI."
     *
     * So if 8-byte WPs aren't supported, then the higher 4-bits of the BAS
     * field will be RAZ/WI. We can just test the first WP's BAS bits and see
     * what happens.
     */
    MRC(DBGWCR_0, wcrtmp);
    wcrtmp |= BIT(DBGWCR_BAS_HIGH_SHIFT);
    MCR(DBGWCR_0, wcrtmp);

    /* Re-read to know if the write to the bit was ignored */
    MRC(DBGWCR_0, wcrtmp);
    return wcrtmp & BIT(DBGWCR_BAS_HIGH_SHIFT);
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
    dscr = dbg_dscr_set_haltingDebugEnable(dscr, 0);
    dscr = dbg_dscr_set_disableAllUserAccesses(dscr, 1);
    dscr = dbg_dscr_set_monitorDebugEnable(dscr, 1);

    writeDscrCp(dscr.words[0]);
    isb();

    /* We can tell if the #DBGEN signal is enabled by setting
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

    bcr = dbg_bcr_set_addressMask(in_val, 0);
    bcr = dbg_bcr_set_hypeModeControl(bcr, 0);
    bcr = dbg_bcr_set_secureStateControl(bcr, 0);
    if (is_match) {
        bcr = dbg_bcr_set_breakpointType(bcr, DBGBCR_TYPE_UNLINKED_INSTRUCTION_MATCH);
    } else {
        bcr = dbg_bcr_set_breakpointType(bcr, DBGBCR_TYPE_UNLINKED_INSTRUCTION_MISMATCH);
    }
    return bcr;
}

static inline dbg_wcr_t Arch_setupWcr(dbg_wcr_t in_val)
{
    dbg_wcr_t wcr;

    wcr = dbg_wcr_set_addressMask(in_val, 0);
    wcr = dbg_wcr_set_hypeModeControl(wcr, 0);
    wcr = dbg_wcr_set_secureStateControl(wcr, 0);
    return wcr;
}

static inline bool_t Arch_breakpointIsMismatch(dbg_bcr_t in_val)
{
    /* Detect if the register is set up for mismatch (single-step). */
    if (dbg_bcr_get_breakpointType(in_val) == DBGBCR_TYPE_UNLINKED_INSTRUCTION_MISMATCH) {
        return true;
    }
    return false;
}

#endif /* CONFIG_HARDWARE_DEBUG_API */

