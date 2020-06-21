/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <assert.h>
#include <config.h>
#include <util.h>
#include <api/types.h>
#include <sel4/macros.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>
#include <sel4/arch/constants.h>

enum tcb_arch_cnode_index {
#ifdef CONFIG_VTX
    /* VSpace root for running any associated VCPU in */
    tcbArchEPTRoot = tcbCNodeEntries,
    tcbArchCNodeEntries
#else
    tcbArchCNodeEntries = tcbCNodeEntries
#endif
};

typedef struct arch_tcb {
    user_context_t tcbContext;
#ifdef CONFIG_VTX
    /* Pointer to associated VCPU. NULL if not associated.
     * tcb->tcbVCPU->vcpuTCB == tcb. */
    struct vcpu *tcbVCPU;
#endif /* CONFIG_VTX */
} arch_tcb_t;

#define SEL_NULL    GDT_NULL
#define SEL_CS_0    (GDT_CS_0 << 3)
#define SEL_DS_0    (GDT_DS_0 << 3)
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_TSS     (GDT_TSS << 3)
#define SEL_FS      ((GDT_FS << 3) | 3)
#define SEL_GS      ((GDT_GS << 3) | 3)

#define IDT_ENTRIES 256

#define VTD_RT_SIZE_BITS  12

#define VTD_CTE_SIZE_BITS 4
#define VTD_CTE_PTR(r)    ((vtd_cte_t*)(r))
#define VTD_CT_BITS       8
#define VTD_CT_SIZE_BITS  (VTD_CT_BITS + VTD_CTE_SIZE_BITS)

#define VTD_PTE_SIZE_BITS 3
#define VTD_PTE_PTR(r)    ((vtd_pte_t*)(r))
#define VTD_PT_INDEX_BITS       9

compile_assert(vtd_pt_size_sane, VTD_PT_INDEX_BITS + VTD_PTE_SIZE_BITS == seL4_IOPageTableBits)

#ifdef CONFIG_VTX

#define EPT_PML4E_SIZE_BITS seL4_X86_EPTPML4EntryBits
#define EPT_PML4_INDEX_BITS seL4_X86_EPTPML4IndexBits
#define EPT_PDPTE_SIZE_BITS seL4_X86_EPTPDPTEntryBits
#define EPT_PDPT_INDEX_BITS seL4_X86_EPTPDPTIndexBits
#define EPT_PDE_SIZE_BITS   seL4_X86_EPTPDEntryBits
#define EPT_PD_INDEX_BITS   seL4_X86_EPTPDIndexBits
#define EPT_PTE_SIZE_BITS   seL4_X86_EPTPTEntryBits
#define EPT_PT_INDEX_BITS   seL4_X86_EPTPTIndexBits

#define EPT_PT_INDEX_OFFSET (seL4_PageBits)
#define EPT_PD_INDEX_OFFSET (EPT_PT_INDEX_OFFSET + EPT_PT_INDEX_BITS)
#define EPT_PDPT_INDEX_OFFSET (EPT_PD_INDEX_OFFSET + EPT_PD_INDEX_BITS)
#define EPT_PML4_INDEX_OFFSET (EPT_PDPT_INDEX_OFFSET + EPT_PDPT_INDEX_BITS)

#define GET_EPT_PML4_INDEX(x) ( (((uint64_t)(x)) >> (EPT_PML4_INDEX_OFFSET)) & MASK(EPT_PML4_INDEX_BITS))
#define GET_EPT_PDPT_INDEX(x) ( ((x) >> (EPT_PDPT_INDEX_OFFSET)) & MASK(EPT_PDPT_INDEX_BITS))
#define GET_EPT_PD_INDEX(x) ( ((x) >> (EPT_PD_INDEX_OFFSET)) & MASK(EPT_PD_INDEX_BITS))
#define GET_EPT_PT_INDEX(x) ( ((x) >> (EPT_PT_INDEX_OFFSET)) & MASK(EPT_PT_INDEX_BITS))

#define EPT_PML4E_PTR(r)     ((ept_pml4e_t *)(r))
#define EPT_PML4E_PTR_PTR(r) ((ept_pml4e_t **)(r))
#define EPT_PML4E_REF(p)     ((word_t)(p))

#define EPT_PML4_SIZE_BITS seL4_X86_EPTPML4Bits
#define EPT_PML4_PTR(r)    ((ept_pml4e_t *)(r))
#define EPT_PML4_REF(p)    ((word_t)(p))

#define EPT_PDPTE_PTR(r)     ((ept_pdpte_t *)(r))
#define EPT_PDPTE_PTR_PTR(r) ((ept_pdpte_t **)(r))
#define EPT_PDPTE_REF(p)     ((word_t)(p))

#define EPT_PDPT_SIZE_BITS seL4_X86_EPTPDPTBits
#define EPT_PDPT_PTR(r)    ((ept_pdpte_t *)(r))
#define EPT_PDPT_REF(p)    ((word_t)(p))

#define EPT_PDE_PTR(r)     ((ept_pde_t *)(r))
#define EPT_PDE_PTR_PTR(r) ((ept_pde_t **)(r))
#define EPT_PDE_REF(p)     ((word_t)(p))

#define EPT_PD_SIZE_BITS seL4_X86_EPTPDBits
#define EPT_PD_PTR(r)    ((ept_pde_t *)(r))
#define EPT_PD_REF(p)    ((word_t)(p))

#define EPT_PTE_PTR(r)    ((ept_pte_t *)(r))
#define EPT_PTE_REF(p)    ((word_t)(p))

#define EPT_PT_SIZE_BITS seL4_X86_EPTPTBits
#define EPT_PT_PTR(r)    ((ept_pte_t *)(r))
#define EPT_PT_REF(p)    ((word_t)(p))

#define VCPU_PTR(r)       ((vcpu_t *)(r))
#define VCPU_REF(p)       ((word_t)(p))

#endif /* CONFIG_VTX */

struct rdmsr_safe_result {
    uint64_t value;
    bool_t success;
};

typedef struct rdmsr_safe_result rdmsr_safe_result_t;

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
typedef word_t vm_rights_t;

#include <mode/object/structures.h>

static inline word_t CONST cap_get_archCapSizeBits(cap_t cap)
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

    case cap_io_port_cap:
        return 0;

#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return 0;
    case cap_io_page_table_cap:
        return seL4_IOPageTableBits;
#endif

    case cap_asid_control_cap:
        return 0;

    case cap_asid_pool_cap:
        return seL4_ASIDPoolBits;

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        return seL4_X86_VCPUBits;

    case cap_ept_pml4_cap:
        return seL4_X86_EPTPML4Bits;
    case cap_ept_pdpt_cap:
        return seL4_X86_EPTPDPTBits;
    case cap_ept_pd_cap:
        return seL4_X86_EPTPDBits;
    case cap_ept_pt_cap:
        return seL4_X86_EPTPTBits;
#endif /* CONFIG_VTX */

    default:
        return cap_get_modeCapSizeBits(cap);
    }
}

static inline bool_t CONST cap_get_archCapIsPhysical(cap_t cap)
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

    case cap_io_port_cap:
        return false;

#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return false;

    case cap_io_page_table_cap:
        return true;
#endif

    case cap_asid_control_cap:
        return false;

    case cap_asid_pool_cap:
        return true;

#ifdef CONFIG_VTX
    case cap_ept_pt_cap:
        return true;

    case cap_ept_pd_cap:
        return true;

    case cap_ept_pdpt_cap:
        return true;

    case cap_ept_pml4_cap:
        return true;
#endif /* CONFIG_VTX */

    default:
        return cap_get_modeCapIsPhysical(cap);
    }
}

static inline void *CONST cap_get_archCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return (void *)(cap_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_page_directory_cap:
        return PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

    case cap_io_port_cap:
        return NULL;

#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return NULL;

    case cap_io_page_table_cap:
        return (void *)(cap_io_page_table_cap_get_capIOPTBasePtr(cap));
#endif

    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

#ifdef CONFIG_VTX
    case cap_ept_pt_cap:
        return EPT_PT_PTR(cap_ept_pt_cap_get_capPTBasePtr(cap));

    case cap_ept_pd_cap:
        return EPT_PD_PTR(cap_ept_pd_cap_get_capPDBasePtr(cap));

    case cap_ept_pdpt_cap:
        return EPT_PDPT_PTR(cap_ept_pdpt_cap_get_capPDPTBasePtr(cap));

    case cap_ept_pml4_cap:
        return EPT_PML4_PTR(cap_ept_pml4_cap_get_capPML4BasePtr(cap));
#endif /* CONFIG_VTX */

    default:
        return cap_get_modeCapPtr(cap);
    }
}

static inline bool_t CONST Arch_isCapRevocable(cap_t derivedCap, cap_t srcCap)
{
    switch (cap_get_capType(derivedCap)) {
    case cap_io_port_cap:
        return cap_get_capType(srcCap) == cap_io_port_control_cap;

    default:
        return false;
    }
}
