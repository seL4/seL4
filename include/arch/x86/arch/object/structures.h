/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_STRUCTURES_H
#define __ARCH_OBJECT_STRUCTURES_H

#include <assert.h>
#include <config.h>
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>
#include <arch/api/constants.h>

typedef struct arch_tcb {
    user_context_t tcbContext;
} arch_tcb_t;

struct user_data {
    word_t words[BIT(seL4_PageBits) / sizeof(word_t)];
};

typedef struct user_data user_data_t;

#define SEL_NULL    GDT_NULL
#define SEL_CS_0    (GDT_CS_0 << 3)
#define SEL_DS_0    (GDT_DS_0 << 3)
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_TSS     (GDT_TSS << 3)
#define SEL_TLS     ((GDT_TLS << 3) | 3)
#define SEL_IPCBUF  ((GDT_IPCBUF << 3) | 3)

#define IDT_ENTRIES 256

#define VTD_RT_SIZE_BITS  12

#define VTD_CTE_SIZE_BITS 3
#define VTD_CTE_PTR(r)    ((vtd_cte_t*)(r))
#define VTD_CT_BITS       9
#define VTD_CT_SIZE_BITS  (VTD_CT_BITS + VTD_CTE_SIZE_BITS)

#define VTD_PTE_SIZE_BITS 3
#define VTD_PTE_PTR(r)    ((vtd_pte_t*)(r))
#define VTD_PT_BITS       9

compile_assert(vtd_pt_size_sane, VTD_PT_BITS + VTD_PTE_SIZE_BITS == seL4_IOPageTableBits)

/* helper structure for filling descriptor registers */
typedef struct gdt_idt_ptr {
    uint16_t limit;
    word_t base;
} __attribute__((packed)) gdt_idt_ptr_t;

enum vm_rights {
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef uint32_t vm_rights_t;

#include <mode/object/structures.h>

static inline word_t CONST
cap_get_archCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return seL4_PageTableBits;

    case cap_page_directory_cap:
        return seL4_PageDirBits;

    case cap_pdpt_cap:
        return seL4_PDPTBits;

    case cap_io_port_cap:
        return 0;
    case cap_io_space_cap:
        return 0;

    case cap_io_page_table_cap:
        return seL4_IOPageTableBits;
    case cap_asid_control_cap:
        return 0;

    case cap_asid_pool_cap:
        return seL4_ASIDPoolBits;

    default:
        return cap_get_modeCapSizeBits(cap);
    }
}

static inline bool_t CONST
cap_get_archCapIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return true;

    case cap_page_table_cap:
        return true;

    case cap_page_directory_cap:
        return true;

    case cap_pdpt_cap:
        return true;

    case cap_io_port_cap:
        return false;

    case cap_io_space_cap:
        return false;

    case cap_io_page_table_cap:
        return true;

    case cap_asid_control_cap:
        return false;

    case cap_asid_pool_cap:
        return true;

    default:
        return cap_get_modeCapIsPhysical(cap);
    }
}

static inline void * CONST
cap_get_archCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return (void *)(cap_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PD_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_page_directory_cap:
        return PT_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

    case cap_pdpt_cap:
        return PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap));

    case cap_io_port_cap:
        return NULL;

    case cap_io_space_cap:
        return NULL;

    case cap_io_page_table_cap:
        return (void *)(cap_io_page_table_cap_get_capIOPTBasePtr(cap));

    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

    default:
        return cap_get_modeCapPtr(cap);
    }
}

#endif
