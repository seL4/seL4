/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_INTEL_VTD_H
#define __PLAT_MACHINE_INTEL_VTD_H

#include <config.h>

#ifdef CONFIG_IOMMU

void invalidate_iotlb(void);
void invalidate_context_cache(void);
void vtd_handle_fault(void);
/* calculate the number of IOPTs needed to map the rmrr regions */
word_t vtd_get_n_paging(acpi_rmrr_list_t *rmrr_list);
/* initialise the number of IOPTs */
bool_t vtd_init_num_iopts(uint32_t num_drhu);
bool_t vtd_init(cpu_id_t  cpu_id, acpi_rmrr_list_t *rmrr_list);

#endif /* CONFIG_IOMMU */

#endif
