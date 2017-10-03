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

/* Root System Descriptor Pointer */
typedef struct acpi_rsdp {
    char         signature[8];
    uint8_t      checksum;
    char         oem_id[6];
    uint8_t      revision;
    uint32_t     rsdt_address;
    uint32_t     length;
    uint64_t     xsdt_address;
    uint8_t      extended_checksum;
    char         reserved[3];
} PACKED acpi_rsdp_t;
compile_assert(acpi_rsdp_packed, sizeof(acpi_rsdp_t) == 36)

/* Root System Descriptor Table */
typedef struct acpi_rsdt {
    acpi_header_t  header;
    uint32_t entry[1];
} PACKED acpi_rsdt_t;

acpi_rsdt_t* acpi_init(acpi_rsdp_t const * rsdp_ptr);
acpi_rsdp_t* acpi_search_rsdp(void);

uint32_t acpi_madt_scan(
    acpi_rsdt_t* acpi_rsdt,
    cpu_id_t*    cpu_list,
    uint32_t*    num_ioapic,
    paddr_t*     ioapic_addrs
);

typedef struct acpi_rmrr_entry {
    dev_id_t device;
    uint32_t base;
    uint32_t limit;
} acpi_rmrr_entry_t;

typedef struct acpi_rmrr_list {
    acpi_rmrr_entry_t entries[CONFIG_MAX_RMRR_ENTRIES];
    int num;
} acpi_rmrr_list_t;

void acpi_dmar_scan(
    acpi_rsdt_t* acpi_rsdt,
    paddr_t*     drhu_list,
    uint32_t*    num_drhu,
    uint32_t     max_dhru_list_len,
    acpi_rmrr_list_t *rmrr_list
);

bool_t acpi_fadt_scan(
    acpi_rsdt_t* acpi_rsdt
);

#endif
