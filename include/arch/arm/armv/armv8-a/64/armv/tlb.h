/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <mode/machine.h>

static inline void invalidateLocalTLB_VMID(word_t vmid)
{
    word_t vttbr = getVTTBR();
    word_t v = (vttbr >> 48);
    /* We need to switch to the target VMID for flushing
     * the TLB if necessary.
     * Note that an invalid address is used, and it seems
     * fine. Otherwise, an ASID lookup is required.
     */
    if (v != vmid) {
        ttbr_t ttbr = ttbr_new(vmid, 0);
        MSR("vttbr_el2", ttbr.words[0]);
        isb();
    }
    asm volatile("tlbi vmalls12e1");
    if (v != vmid) {
        /* Restore the previous VTTBR value */
        MSR("vttbr_el2", vttbr);
        // isb not required because it comes with eret.
    }
    dsb_ish();
}

static inline void invalidateLocalTLB_IPA_VMID(word_t ipa_plus_vmid)
{
    word_t vttbr = getVTTBR();
    word_t v = (vttbr >> 48);
    word_t vmid = ipa_plus_vmid >> 48;
    /* The [0:35] bits are IPA, other bits are reserved as 0 */
    word_t ipa = ipa_plus_vmid & 0xfffffffff;
    if (v != vmid) {
        ttbr_t ttbr = ttbr_new(vmid, 0);
        MSR("vttbr_el2", ttbr.words[0]);
        isb();
    }
    asm volatile("tlbi ipas2e1, %0" :: "r"(ipa));
    asm volatile("tlbi vmalle1");

    if (v != vmid) {
        /* Restore the previous VTTBR value */
        MSR("vttbr_el2", vttbr);
        // isb not required because it comes with eret.
    }
    dsb_ish();
}


