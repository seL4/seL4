/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_ARMV_TLB_H
#define __ARCH_ARMV_TLB_H

#include <config.h>
#include <mode/machine.h>

static inline void invalidateLocalTLB_VMID(word_t vmid)
{
    word_t vttbr = getVTTBR();
    word_t v = (vttbr >> 48);
    dsb();
    /* We need to switch to the target VMID for flushing
     * the TLB if necessary.
     * Note that an invalid address is used, and it seems
     * fine. Otherwise, an ASID lookup is required.
     */
    if (v != vmid) {
        setCurrentUserVSpaceRoot(ttbr_new(vmid, 0));
    }
    invalidateLocalTLB_VMALLS12E1();
    if (v != vmid) {
        /* Restore the previous VTTBR value */
        setCurrentUserVSpaceRoot((ttbr_t) {
            .words[0] = vttbr
        });
    }
}

static inline void invalidateLocalTLB_IPA_VMID(word_t ipa_plus_vmid)
{
    word_t vttbr = getVTTBR();
    word_t v = (vttbr >> 48);
    word_t vmid = ipa_plus_vmid >> 48;
    /* The [0:35] bits are IPA, other bits are reserved as 0 */
    word_t ipa = ipa_plus_vmid & 0xfffffffff;
    dsb();
    if (v != vmid) {
        setCurrentUserVSpaceRoot(ttbr_new(vmid, 0));
    }
    invalidateLocalTLB_IPA(ipa);
    if (v != vmid) {
        setCurrentUserVSpaceRoot((ttbr_t) {
            .words[0] = vttbr
        });
    }
}

#endif

