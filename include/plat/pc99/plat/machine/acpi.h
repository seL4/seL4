/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_ACPI_H
#define __PLAT_MACHINE_ACPI_H

#include <assert.h>
#include <config.h>
#include <types.h>

/* Generic System Descriptor Table Header */
typedef struct acpi_header {
    char         signature[4];
    uint32_t     length;
    uint8_t      revision;
    uint8_t      checksum;
    char         oem_id[6];
    char         oem_table_id[8];
    uint32_t     oem_revision;
    char         creater_id[4];
    uint32_t     creater_revision;
} PACKED acpi_header_t;

/* Root System Descriptor Table */
typedef struct acpi_rsdt {
    acpi_header_t  header;
    uint32_t entry[1];
} PACKED acpi_rsdt_t;

acpi_rsdt_t* acpi_init(void);

uint32_t acpi_madt_scan(
    acpi_rsdt_t* acpi_rsdt,
    cpu_id_t*    cpu_list,
    uint32_t     max_list_len,
    uint32_t*    num_ioapic,
    paddr_t*     ioapic_addrs
);

void acpi_dmar_scan(
    acpi_rsdt_t* acpi_rsdt,
    paddr_t*     drhu_list,
    uint32_t*    num_drhu,
    uint32_t     max_dhru_list_len,
    dev_id_t*    passthrough_dev_list,
    uint32_t*    num_passthrough_dev,
    uint32_t     max_passthrough_dev_list_len
);

#endif
