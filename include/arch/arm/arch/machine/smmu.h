/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_SMMU_H
#define __ARCH_SMMU_H

#include <config.h>

#ifdef CONFIG_ARM_SMMU_V2

#include <types.h>
#include <object/structures.h>

uint32_t plat_smmu_get_asid_by_stream_id(uint16_t sid);

int plat_smmu_init(void);

void plat_smmu_tlb_flush_all(void);

iopde_t *plat_smmu_lookup_iopd_by_asid(uint32_t asid);
void plat_smmu_handle_interrupt(void);

#else /* !CONFIG_ARM_SMMU_V2 */

/* dummy functions */
static inline void
plat_smmu_handle_interrupt(void)
{
    return;
}

#endif /* CONFIG_ARM_SMMU_V2 */

#endif /* __ARCH_SMMU_H */
