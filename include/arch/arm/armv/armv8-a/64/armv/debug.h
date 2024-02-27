/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#if defined(CONFIG_HARDWARE_DEBUG_API)

enum v8_breakpoint_type {
    DBGBCR_TYPE_UNLINKED_INSTRUCTION_MATCH = 0u,
    DBGBCR_TYPE_LINKED_INSTRUCTION_MATCH = 0x1u,

    DBGBCR_TYPE_UNLINKED_CONTEXT_ID_MATCH = 0x2u,
    DBGBCR_TYPE_LINKED_CONTEXT_ID_MATCH = 0x3u,

    DBGBCR_TYPE_UNLINKED_INSTRUCTION_MISMATCH = 0x4u,
    DBGBCR_TYPE_LINKED_INSTRUCTION_MISMATCH = 0x5u,

    DBGBCR_TYPE_UNLINKED_CONTEXTIDR_EL1_MATCH = 0x6u,
    DBGBCR_TYPE_LINKED_CONTEXTIDR_EL1_MATCH = 0x7u,

    DBGBCR_TYPE_UNLINKED_VMID_MATCH = 0x8u,
    DBGBCR_TYPE_LINKED_VMID_MATCH = 0x9u,

    DBGBCR_TYPE_UNLINKED_CONTEXT_ID_AND_VMID_MATCH = 0xAu,
    DBGBCR_TYPE_LINKED_CONTEXT_ID_AND_VMID_MATCH = 0xBu,

    DBGBCR_TYPE_UNLINKED_CONTEXTIDR_EL2_MATCH = 0xCu,
    DBGBCR_TYPE_LINKED_CONTEXTIDR_EL2_MATCH = 0xDu,

    DBGBCR_TYPE_UNLINKED_FULL_CONTEXT_ID_MATCH = 0xEu,
    DBGBCR_TYPE_LINKED_FULL_CONTEXT_ID_MATCH = 0xFu,
};

static inline dbg_bcr_t Arch_setupBcr(dbg_bcr_t bcr, bool_t is_match)
{
    if (is_match) {
        bcr = dbg_bcr_set_breakpointType(bcr, DBGBCR_TYPE_UNLINKED_INSTRUCTION_MATCH);
    } else {
        bcr = dbg_bcr_set_breakpointType(bcr, DBGBCR_TYPE_UNLINKED_INSTRUCTION_MISMATCH);
    }
    return bcr;
}

static inline bool_t Arch_breakpointIsSingleStepping(tcb_t *t, uint16_t bp_num)
{
    /* AARCH64 does not use breakpoints for single-stepping */
    return false;
}

#endif