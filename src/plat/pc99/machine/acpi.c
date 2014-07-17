/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <util.h>
#include <assert.h>
#include <machine/io.h>
#include <arch/linker.h>
#include <plat/machine.h>
#include <plat/machine/acpi.h>
#include <plat/machine/devices.h>
#include <plat/machine/pci.h>

enum acpi_type {
    ACPI_RSDP,
    ACPI_RSDT
};

/* Root System Descriptor Pointer */
typedef struct acpi_rsdp {
    char         signature[8];
    uint8_t      checksum;
    char         oem_id[6];
    uint8_t      revision;
    uint32_t     rsdt_address;
    uint32_t     length;
    uint32_t     xsdt_address[2];
    uint8_t      extended_checksum;
    char         reserved[3];
} acpi_rsdp_t;
compile_assert(acpi_rsdp_packed, sizeof(acpi_rsdp_t) == 36)

/* DMA Remapping Reporting Table */
typedef struct acpi_dmar {
    acpi_header_t header;
    uint8_t       host_addr_width;
    uint8_t       flags;
    uint8_t       reserved[10];
} acpi_dmar_t;
compile_assert(acpi_dmar_packed,
               sizeof(acpi_dmar_t) == sizeof(acpi_header_t) + 12)

/* DMA Remapping Structure Header */
typedef struct acpi_dmar_header {
    uint16_t type;
    uint16_t length;
} acpi_dmar_header_t;
compile_assert(acpi_dmar_header_packed, sizeof(acpi_dmar_header_t) == 4)

/* DMA Remapping Structure Types */
enum acpi_table_dmar_struct_type {
    DMAR_DRHD = 0,
    DMAR_RMRR = 1,
    DMAR_ATSR = 2,
};

/* DMA Remapping Hardware unit Definition */
typedef struct acpi_dmar_drhd {
    acpi_dmar_header_t header;
    uint8_t            flags;
    uint8_t            reserved;
    uint16_t           segment;
    uint32_t           reg_base[2];
} acpi_dmar_drhd_t;
compile_assert(acpi_dmar_drhd_packed,
               sizeof(acpi_dmar_drhd_t) == sizeof(acpi_dmar_header_t) + 12)

/* Reserved Memory Region Reporting structure Definition */
typedef struct acpi_dmar_devscope {
    uint8_t  type;
    uint8_t  length;
    uint16_t reserved;
    uint8_t  enum_id;
    uint8_t  start_bus;
    struct {
        uint8_t dev;
        uint8_t fun;
    } path_0;
} acpi_dmar_devscope_t;
compile_assert(acpi_dmar_devscope_packed, sizeof(acpi_dmar_devscope_t) == 8)

/* Reserved Memory Region Reporting structure Definition */
typedef struct acpi_dmar_rmrr {
    acpi_dmar_header_t   header;
    uint16_t             reserved;
    uint16_t             segment;
    uint32_t             reg_base[2];
    uint32_t             reg_limit[2];
    acpi_dmar_devscope_t devscope_0;
} acpi_dmar_rmrr_t;
compile_assert(acpi_dmar_rmrr_packed, sizeof(acpi_dmar_rmrr_t) ==
               sizeof(acpi_dmar_header_t) + 20 + sizeof(acpi_dmar_devscope_t))

/* Multiple APIC Description Table (MADT) */
typedef struct acpi_madt {
    acpi_header_t header;
    uint32_t      apic_addr;
    uint32_t      flags;
} acpi_madt_t;
compile_assert(acpi_madt_packed,
               sizeof(acpi_madt_t) == sizeof(acpi_header_t) + 8)

typedef struct acpi_madt_header {
    uint8_t type;
    uint8_t length;
} acpi_madt_header_t;
compile_assert(acpi_madt_header_packed, sizeof(acpi_madt_header_t) == 2)

enum acpi_table_madt_struct_type {
    MADT_APIC   = 0,
    MADT_IOAPIC = 1,
    MADT_ISO    = 2,
};

typedef struct acpi_madt_apic {
    acpi_madt_header_t header;
    uint8_t            cpu_id;
    uint8_t            apic_id;
    uint32_t           flags;
} acpi_madt_apic_t;
compile_assert(acpi_madt_apic_packed,
               sizeof(acpi_madt_apic_t) == sizeof(acpi_madt_header_t) + 6)

typedef struct acpi_madt_ioapic {
    acpi_madt_header_t header;
    uint8_t            ioapic_id;
    uint8_t            reserved[1];
    uint32_t           ioapic_addr;
    uint32_t           gsib;
} acpi_madt_ioapic_t;
compile_assert(acpi_madt_ioapic_packed,
               sizeof(acpi_madt_ioapic_t) == sizeof(acpi_madt_header_t) + 10)

typedef struct acpi_madt_iso {
    acpi_madt_header_t header;
    uint8_t            bus; /* always 0 (ISA) */
    uint8_t            source;
    uint32_t           gsi;
    uint16_t           flags;
} acpi_madt_iso_t;
/* We can't assert on the sizeof acpi_madt_iso because it contains trailing
 * padding.
 */
compile_assert(acpi_madt_iso_packed,
               OFFSETOF(acpi_madt_iso_t, flags) == sizeof(acpi_madt_header_t) + 6)

/* workaround because string literals are not supported by C parser */
const char acpi_str_rsd[]  = {'R', 'S', 'D', ' ', 'P', 'T', 'R', ' ', 0};
const char acpi_str_apic[] = {'A', 'P', 'I', 'C', 0};

BOOT_CODE static uint8_t
acpi_calc_checksum(char* start, uint32_t length)
{
    uint8_t checksum = 0;

    while (length > 0) {
        checksum += *start;
        start++;
        length--;
    }
    return checksum;
}

BOOT_CODE static acpi_rsdp_t*
acpi_get_rsdp(void)
{
    char* addr;

    for (addr = (char*)BIOS_PADDR_START; addr < (char*)BIOS_PADDR_END; addr += 16) {
        if (strncmp(addr, acpi_str_rsd, 8) == 0) {
            if (acpi_calc_checksum(addr, 20) == 0) {
                return (acpi_rsdp_t*)addr;
            }
        }
    }
    return NULL;
}

void* acpi_table_init(void* entry, enum acpi_type table_type);
BOOT_CODE void*
acpi_table_init(void* entry, enum acpi_type table_type)
{
    void* acpi_table;
    unsigned int pages_for_table;
    unsigned int pages_for_header = 1;

    /* if we need to map another page to read header */
    uint32_t offset_in_page = (uint32_t)entry & MASK(pageBitsForSize(IA32_4M));
    if (MASK(pageBitsForSize(IA32_4M)) - offset_in_page < sizeof(acpi_rsdp_t)) {
        pages_for_header++;
    }

    /* map in table's header */
    acpi_table = map_temp_boot_page(entry, pages_for_header);

    switch (table_type) {
    case ACPI_RSDP: {
        acpi_rsdp_t *rsdp_entry = (acpi_rsdp_t*)entry;
        pages_for_table = (rsdp_entry->length + offset_in_page) / MASK(pageBitsForSize(IA32_4M)) + 1;
        break;
    }
    case ACPI_RSDT: { // RSDT, MADT, DMAR etc.
        acpi_rsdt_t *rsdt_entry = (acpi_rsdt_t*)entry;
        pages_for_table = (rsdt_entry->header.length + offset_in_page) / MASK(pageBitsForSize(IA32_4M)) + 1;
        break;
    }
    default:
        printf("Error: Mapping unknown ACPI table type\n");
        assert(false);
        return NULL;
    }

    /* map in full table */
    acpi_table = map_temp_boot_page(entry, pages_for_table);

    return acpi_table;
}

BOOT_CODE acpi_rsdt_t*
acpi_init(void)
{
    acpi_rsdp_t* acpi_rsdp = acpi_get_rsdp();
    acpi_rsdt_t* acpi_rsdt;
    acpi_rsdt_t* acpi_rsdt_mapped;

    if (acpi_rsdp == NULL) {
        printf("BIOS: No ACPI support detected\n");
        return NULL;
    }
    printf("ACPI: RSDP paddr=0x%x\n", (unsigned int)acpi_rsdp);
    acpi_rsdp = acpi_table_init(acpi_rsdp, ACPI_RSDP);
    printf("ACPI: RSDP vaddr=0x%x\n", (unsigned int)acpi_rsdp);

    acpi_rsdt = (acpi_rsdt_t*)acpi_rsdp->rsdt_address;
    printf("ACPI: RSDT paddr=0x%x\n", (unsigned int)acpi_rsdt);
    acpi_rsdt_mapped = (acpi_rsdt_t*)acpi_table_init(acpi_rsdt, ACPI_RSDT);
    printf("ACPI: RSDT vaddr=0x%x\n", (unsigned int)acpi_rsdt_mapped);

    assert(acpi_rsdt_mapped->header.length > 0);
    if (acpi_calc_checksum((char*)acpi_rsdt_mapped, acpi_rsdt_mapped->header.length) != 0) {
        printf("ACPI: RSDT checksum failure\n");
        return NULL;
    }

    return acpi_rsdt;
}

BOOT_CODE uint32_t
acpi_madt_scan(
    acpi_rsdt_t* acpi_rsdt,
    cpu_id_t*    cpu_list,
    uint32_t     max_list_len
)
{
    unsigned int entries;
    uint32_t            num_cpu;
    uint32_t            count;
    acpi_madt_t*        acpi_madt;
    acpi_madt_header_t* acpi_madt_header;

    acpi_rsdt_t* acpi_rsdt_mapped;
    acpi_madt_t* acpi_madt_mapped;
    acpi_rsdt_mapped = (acpi_rsdt_t*)acpi_table_init(acpi_rsdt, ACPI_RSDT);

    num_cpu = 0;

    assert(acpi_rsdt_mapped->header.length >= sizeof(acpi_header_t));
    entries = (acpi_rsdt_mapped->header.length - sizeof(acpi_header_t)) / sizeof(acpi_header_t*);
    for (count = 0; count < entries; count++) {
        acpi_madt = (acpi_madt_t*)acpi_rsdt_mapped->entry[count];
        acpi_madt_mapped = (acpi_madt_t*)acpi_table_init(acpi_madt, ACPI_RSDT);

        if (strncmp(acpi_str_apic, acpi_madt_mapped->header.signature, 4) == 0) {
            printf("ACPI: MADT paddr=0x%x\n", (unsigned int)acpi_madt);
            printf("ACPI: MADT vaddr=0x%x\n", (unsigned int)acpi_madt_mapped);
            printf("ACPI: MADT apic_addr=0x%x\n", acpi_madt_mapped->apic_addr);
            printf("ACPI: MADT flags=0x%x\n", acpi_madt_mapped->flags);

            acpi_madt_header = (acpi_madt_header_t*)(acpi_madt_mapped + 1);

            while ((char*)acpi_madt_header < (char*)acpi_madt_mapped + acpi_madt_mapped->header.length) {
                switch (acpi_madt_header->type) {
                case MADT_APIC: {
                    /* what Intel calls apic_id is what is called cpu_id in seL4! */
                    uint8_t  cpu_id = ((acpi_madt_apic_t*)acpi_madt_header)->apic_id;
                    uint32_t flags  = ((acpi_madt_apic_t*)acpi_madt_header)->flags;
                    if (flags == 1) {
                        printf("ACPI: MADT_APIC apic_id=0x%x\n", cpu_id);
                        if (num_cpu < max_list_len) {
                            cpu_list[num_cpu] = cpu_id;
                        }
                        num_cpu++;
                    }
                    break;
                }
                case MADT_IOAPIC:
                    printf(
                        "ACPI: MADT_IOAPIC ioapic_id=%d ioapic_addr=0x%x gsib=%d\n",
                        ((acpi_madt_ioapic_t*)acpi_madt_header)->ioapic_id,
                        ((acpi_madt_ioapic_t*)acpi_madt_header)->ioapic_addr,
                        ((acpi_madt_ioapic_t*)acpi_madt_header)->gsib
                    );
                    break;
                default:
                    break;
                }
                acpi_madt_header = (acpi_madt_header_t*)((char*)acpi_madt_header + acpi_madt_header->length);
            }
        }
    }

    printf("ACPI: %d CPU(s) detected\n", num_cpu);

    return num_cpu;
}

#ifdef CONFIG_IOMMU

BOOT_CODE static bool_t
acpi_dev_in_list(dev_id_t* dev_list, uint32_t list_len, dev_id_t dev)
{
    unsigned int i = 0;

    while (i < list_len) {
        if (dev_list[i] == dev) {
            return true;
        }
        i++;
    }
    return false;
}

BOOT_CODE void
acpi_dmar_scan(
    acpi_rsdt_t* acpi_rsdt,
    paddr_t*     drhu_list,
    uint32_t*    num_drhu,
    uint32_t     max_drhu_list_len,
    dev_id_t*    passthrough_dev_list,
    uint32_t*    num_passthrough_dev,
    uint32_t     max_passthrough_dev_list_len
)
{
    unsigned int i;
    unsigned int entries;
    uint32_t count;
    uint32_t reg_basel, reg_baseh;
    dev_id_t dev_id;

    acpi_dmar_t*          acpi_dmar;
    acpi_dmar_header_t*   acpi_dmar_header;
    acpi_dmar_rmrr_t*     acpi_dmar_rmrr;
    acpi_dmar_devscope_t* acpi_dmar_devscope;

    acpi_rsdt_t* acpi_rsdt_mapped;
    acpi_dmar_t* acpi_dmar_mapped;

    acpi_rsdt_mapped = (acpi_rsdt_t*)acpi_table_init(acpi_rsdt, ACPI_RSDT);

    *num_drhu = 0;
    *num_passthrough_dev = 0;

    assert(acpi_rsdt->header.length >= sizeof(acpi_header_t));
    entries = (acpi_rsdt->header.length - sizeof(acpi_header_t)) / sizeof(acpi_header_t*);
    for (count = 0; count < entries; count++) {
        acpi_dmar = (acpi_dmar_t*)acpi_rsdt_mapped->entry[count];
        acpi_dmar_mapped = (acpi_dmar_t*)acpi_table_init(acpi_dmar, ACPI_RSDT);

        if (strncmp("DMAR", acpi_dmar_mapped->header.signature, 4) == 0) {
            printf("ACPI: DMAR paddr=0x%x\n", (unsigned int)acpi_dmar);
            printf("ACPI: DMAR vaddr=0x%x\n", (unsigned int)acpi_dmar_mapped);
            printf("ACPI: IOMMU host address width: %d\n", acpi_dmar_mapped->host_addr_width + 1);
            acpi_dmar_header = (acpi_dmar_header_t*)(acpi_dmar_mapped + 1);

            while ((char*)acpi_dmar_header < (char*)acpi_dmar_mapped + acpi_dmar_mapped->header.length) {
                switch (acpi_dmar_header->type) {

                case DMAR_DRHD:
                    if (*num_drhu == max_drhu_list_len) {
                        printf("ACPI: too many IOMMUs, disabling IOMMU support\n");
                        /* try to increase MAX_NUM_DRHU in config.h */
                        *num_drhu = 0; /* report zero IOMMUs */
                        return;
                    }
                    reg_basel = ((acpi_dmar_drhd_t*)acpi_dmar_header)->reg_base[0];
                    reg_baseh = ((acpi_dmar_drhd_t*)acpi_dmar_header)->reg_base[1];
                    /* check if value fits into uint32_t */
                    if (reg_baseh != 0) {
                        printf("ACPI: DMAR_DRHD reg_base exceeds 32 bit, disabling IOMMU support\n");
                        /* try to make BIOS map it below 4G */
                        *num_drhu = 0; /* report zero IOMMUs */
                        return;
                    }
                    drhu_list[*num_drhu] = (paddr_t)reg_basel;
                    (*num_drhu)++;
                    break;

                case DMAR_RMRR:
                    /* loop through all device scopes of this RMRR */
                    acpi_dmar_rmrr = (acpi_dmar_rmrr_t*)acpi_dmar_header;
                    if (acpi_dmar_rmrr->reg_base[1] != 0 ||
                            acpi_dmar_rmrr->reg_limit[1] != 0) {
                        printf("ACPI: RMRR device above 4GiB, disabling IOMMU support\n");
                        *num_drhu = 0;
                        return ;
                    }

                    /* Provide the region to user level */
                    insert_dev_p_reg((p_region_t) {
                        .start = acpi_dmar_rmrr->reg_base[0], .end = acpi_dmar_rmrr->reg_limit[0] + 2
                    });
                    printf("ACPI: RMRR providing region 0x%x-0x%x\n", acpi_dmar_rmrr->reg_base[0], acpi_dmar_rmrr->reg_limit[0]);

                    for (i = 0; i <= (acpi_dmar_header->length - sizeof(acpi_dmar_rmrr_t)) / sizeof(acpi_dmar_devscope_t); i++) {
                        acpi_dmar_devscope = &acpi_dmar_rmrr->devscope_0 + i;

                        if (acpi_dmar_devscope->type != 1) {
                            /* FIXME - bugzilla bug 170 */
                            printf("ACPI: RMRR device scope: non-PCI-Endpoint-Devices not supported yet, disabling IOMMU support\n");
                            *num_drhu = 0; /* report zero IOMMUs */
                            return;
                        }

                        if (acpi_dmar_devscope->length > sizeof(acpi_dmar_devscope_t)) {
                            /* FIXME - bugzilla bug 170 */
                            printf("ACPI: RMRR device scope: devices behind bridges not supported yet, disabling IOMMU support\n");
                            *num_drhu = 0; /* report zero IOMMUs */
                            return;
                        }

                        dev_id =
                            get_dev_id(
                                acpi_dmar_devscope->start_bus,
                                acpi_dmar_devscope->path_0.dev,
                                acpi_dmar_devscope->path_0.fun
                            );

                        if (!acpi_dev_in_list(passthrough_dev_list, *num_passthrough_dev, dev_id)) {
                            /* FIXME - bugzilla bug 171 */
                            printf("ACPI: registering device for IOMMU passthrough: bus=0x%x dev=0x%x fun=0x%x\n",
                                   acpi_dmar_devscope->start_bus,
                                   acpi_dmar_devscope->path_0.dev,
                                   acpi_dmar_devscope->path_0.fun
                                  );
                            if (*num_passthrough_dev == max_passthrough_dev_list_len) {
                                printf("ACPI: too many passthrough devices, disabling IOMMU support\n");
                                /* try to increase MAX_NUM_PASSTHROUGH_DEV in config.h */
                                *num_drhu = 0; /* report zero IOMMUs */
                                return;
                            }
                            passthrough_dev_list[*num_passthrough_dev] = dev_id;
                            (*num_passthrough_dev)++;
                        }
                    }
                    break;

                case DMAR_ATSR:
                    /* not implemented yet */
                    break;

                default:
                    printf("ACPI: Unknown DMA remapping structure type: %x\n", acpi_dmar_header->type);
                }
                acpi_dmar_header = (acpi_dmar_header_t*)((char*)acpi_dmar_header + acpi_dmar_header->length);
            }
        }
    }
    printf("ACPI: %d IOMMUs detected\n", *num_drhu);
}

#endif /* IOMMU */
