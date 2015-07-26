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
#include <plat/machine/hardware_gen.h>

/* Object sizes*/
#define EP_SIZE_BITS 4
#define AEP_SIZE_BITS 4
#define CTE_SIZE_BITS 4
#define TCB_BLOCK_SIZE_BITS 10
enum tcb_arch_cnode_index {
    /* VSpace root for running any associated VCPU in */
    tcbArchEPTRoot = tcbCNodeEntries,
    tcbArchCNodeEntries
};

typedef struct arch_tcb {
    user_context_t tcbContext;

#ifdef CONFIG_VTX
    /* Pointer to associated VCPU. NULL if not associated.
     * tcb->vcpu->tcb == tcb. */
    struct vcpu *vcpu;
#endif
} arch_tcb_t;
#ifdef CONFIG_VTX
/* Access to the VCPU element of the tcb is done through a hard coded offset in traps.S
 * this assert makes sure they remain consistent. If this assert fails update the
 * offset in traps.S, and match it here */
compile_assert(vcpu_offset_correct, __builtin_offsetof(struct arch_tcb, vcpu) == 0x250);
#endif

#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_CS_3    3
#define GDT_DS_3    4
#define GDT_TSS     5
#define GDT_TLS     6
#define GDT_IPCBUF  7
#define GDT_ENTRIES 8

#define SEL_NULL    GDT_NULL
#define SEL_CS_0    (GDT_CS_0 << 3)
#define SEL_DS_0    (GDT_DS_0 << 3)
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_TSS     (GDT_TSS << 3)
#define SEL_TLS     ((GDT_TLS << 3) | 3)
#define SEL_IPCBUF  ((GDT_IPCBUF << 3) | 3)

#define IDT_ENTRIES 256

#ifdef CONFIG_PAE_PAGING
#define PDPTE_SIZE_BITS 3
#define PDPT_BITS    2
#define PDE_SIZE_BITS  3
#define PD_BITS      9
#define PTE_SIZE_BITS 3
#define PT_BITS      9
#else
#define PDPTE_SIZE_BITS 0
#define PDPT_BITS 0
#define PDE_SIZE_BITS  2
#define PD_BITS      10
#define PTE_SIZE_BITS 2
#define PT_BITS      10
#endif

#define PDPTE_PTR(r)   ((pdpte_t *)(r))
#define PDPTE_PTR_PTR(r) ((pdpte_t**)(r))
#define PDPTE_REF(p)   ((unsigned int)(p))

#define PDPT_SIZE_BITS (PDPT_BITS + PDPTE_SIZE_BITS)
#define PDPT_PTR(r)    ((pdpte_t*)(r))
#define PDPT_REF(p)   ((unsigned int)(p))

#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)(r))
#define PDE_REF(p)     ((unsigned int)(p))

#define PD_SIZE_BITS (PD_BITS + PDE_SIZE_BITS)
#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((unsigned int)(p))

#define PTE_PTR(r)    ((pte_t *)(r))
#define PTE_REF(p)    ((unsigned int)(p))

#define PT_SIZE_BITS (PT_BITS + PTE_SIZE_BITS)
#define PT_PTR(r)    ((pte_t *)(r))
#define PT_REF(p)    ((unsigned int)(p))

#ifdef CONFIG_IOMMU

#define VTD_RT_SIZE_BITS  12

#define VTD_CTE_SIZE_BITS 3
#define VTD_CTE_PTR(r)    ((vtd_cte_t*)(r))
#define VTD_CTE_REF(p)    ((unsigned int)(p))
#define VTD_CT_BITS       9
#define VTD_CT_SIZE_BITS  (VTD_CT_BITS + VTD_CTE_SIZE_BITS)

#define VTD_PTE_SIZE_BITS 3
#define VTD_PTE_PTR(r)    ((vtd_pte_t*)(r))
#define VTD_PTE_REF(p)    ((unsigned int)(p))
#define VTD_PT_BITS       9
#define VTD_PT_SIZE_BITS  (VTD_PT_BITS + VTD_PTE_SIZE_BITS)
#define VTD_PT_REF(p)     ((unsigned int)(p))

#endif

#ifdef CONFIG_VTX

#define EPT_PDPTE_SIZE_BITS  3
#define EPT_PDPTE_PTR(r)     ((ept_pdpte_t *)(r))
#define EPT_PDPTE_PTR_PTR(r) ((ept_pdpte_t **)(r))
#define EPT_PDPTE_REF(p)     ((unsigned int)(p))

#define EPT_PDPT_BITS      9
#define EPT_PDPT_SIZE_BITS (EPT_PDPT_BITS+EPT_PDPTE_SIZE_BITS)
#define EPT_PML4_SIZE_BITS (EPT_PDPT_SIZE_BITS+1)
#define EPT_PDPT_PTR(r)    ((ept_pdpte_t *)(r))
#define EPT_PDPT_REF(p)    ((unsigned int)(p))
#define EPT_PDPT_OFFSET    (1 << EPT_PDPT_SIZE_BITS)

#define EPT_PDE_SIZE_BITS  3
#define EPT_PDE_PTR(r)     ((ept_pde_t *)(r))
#define EPT_PDE_PTR_PTR(r) ((ept_pde_t **)(r))
#define EPT_PDE_REF(p)     ((unsigned int)(p))

#define EPT_PD_BITS      9
#define EPT_PD_SIZE_BITS (PD_BITS+PDE_SIZE_BITS)
#define EPT_PD_PTR(r)    ((ept_pde_t *)(r))
#define EPT_PD_REF(p)    ((unsigned int)(p))

#define EPT_PTE_SIZE_BITS 3
#define EPT_PTE_PTR(r)    ((ept_pte_t *)(r))
#define EPT_PTE_REF(p)    ((unsigned int)(p))

#define EPT_PT_BITS      9
#define EPT_PT_SIZE_BITS (PT_BITS+PTE_SIZE_BITS)
#define EPT_PT_PTR(r)    ((ept_pte_t *)(r))
#define EPT_PT_REF(p)    ((unsigned int)(p))

#define VTX_VCPU_BITS 14

/* Generate a vcpu_t pointer from a vcpu block reference */
#define VCPU_PTR(r)       ((vcpu_t *)(r))
#define VCPU_REF(p)       ((unsigned int)(p))
#endif

/* helper structure for filling descriptor registers */
typedef struct gdt_idt_ptr {
    uint16_t limit;
    uint16_t basel;
    uint16_t baseh;
} gdt_idt_ptr_t;

compile_assert(gdt_idt_ptr_packed,
               sizeof(gdt_idt_ptr_t) == sizeof(uint16_t) * 3)

#define WORD_SIZE_BITS 2

enum vm_rights {
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef uint32_t vm_rights_t;

static inline unsigned int CONST
cap_get_archCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return PT_SIZE_BITS;

    case cap_page_directory_cap:
        return PD_SIZE_BITS;

    case cap_pdpt_cap:
        return PDPT_SIZE_BITS;

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        return VTX_VCPU_BITS;
#endif
    case cap_io_port_cap:
        return 0;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return 0;

    case cap_io_page_table_cap:
        return VTD_PT_SIZE_BITS;
#endif

    case cap_ipi_cap:
        return 0;

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
        return EPT_PML4_SIZE_BITS;

    case cap_ept_page_directory_cap:
        return EPT_PD_SIZE_BITS;

    case cap_ept_page_table_cap:
        return EPT_PT_SIZE_BITS;
#endif


    default:
        fail("Invalid arch cap type");
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

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        return (void*)(cap_vcpu_cap_get_capVCPUPtr(cap));
#endif

    case cap_io_port_cap:
        return NULL;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return NULL;

    case cap_io_page_table_cap:
        return (void *)(cap_io_page_table_cap_get_capIOPTBasePtr(cap));
#endif

    case cap_ipi_cap:
        return NULL;

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
        return EPT_PDPT_PTR((uint32_t)cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(cap) - EPT_PDPT_OFFSET);

    case cap_ept_page_directory_cap:
        return EPT_PD_PTR(cap_ept_page_directory_cap_get_capPDBasePtr(cap));

    case cap_ept_page_table_cap:
        return EPT_PT_PTR(cap_ept_page_table_cap_get_capPTBasePtr(cap));
#endif

    default:
        fail("Invalid arch cap type");
    }
}

#endif /* __ARCH_OBJECT_STRUCTURES_H */

