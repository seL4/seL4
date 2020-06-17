/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <assert.h>
#include <config.h>
#include <types.h>

enum acpi_size {
    ACPI_V1_SIZE = 20,
    ACPI_V2_SIZE = 36
};

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
compile_assert(acpi_rsdp_packed, sizeof(acpi_rsdp_t) == ACPI_V2_SIZE)

/* Root System Descriptor Table */
typedef struct acpi_rsdt {
    acpi_header_t  header;
    uint32_t entry[1];
} PACKED acpi_rsdt_t;

/* Attemps to initialize acpi by searching for a valid RSDP block. If found a copy is placed in rsdp_data
 * and true is returned, otherwise the contents of rsdp_data are undefined and false is returned. */
bool_t acpi_init(acpi_rsdp_t *rsdp_data);

/* Validates that a given rsdp block is in fact valid */
BOOT_CODE bool_t acpi_validate_rsdp(acpi_rsdp_t *acpi_rsdp);

uint32_t acpi_madt_scan(
    acpi_rsdp_t *acpi_rsdp,
    cpu_id_t    *cpu_list,
    uint32_t    *num_ioapic,
    paddr_t     *ioapic_addrs
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
    acpi_rsdp_t *acpi_rsdp,
    paddr_t     *drhu_list,
    uint32_t    *num_drhu,
    uint32_t     max_dhru_list_len,
    acpi_rmrr_list_t *rmrr_list
);

bool_t acpi_fadt_scan(
    acpi_rsdp_t *acpi_rsdp
);
