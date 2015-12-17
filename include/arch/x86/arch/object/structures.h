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

/* sizeof (tcb_t) + sizeof (arch_tcb_t) */
#ifdef CONFIG_VTX
#define EXPECTED_TCB_SIZE 664
#else
#define EXPECTED_TCB_SIZE 660
#endif

#define SEL_NULL    GDT_NULL
#define SEL_CS_0    (GDT_CS_0 << 3)
#define SEL_DS_0    (GDT_DS_0 << 3)
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_TSS     (GDT_TSS << 3)
#define SEL_TLS     ((GDT_TLS << 3) | 3)
#define SEL_IPCBUF  ((GDT_IPCBUF << 3) | 3)

#define IDT_ENTRIES 256

#ifdef CONFIG_IOMMU

#define VTD_RT_SIZE_BITS  12

#define VTD_CTE_SIZE_BITS 3
#define VTD_CTE_PTR(r)    ((vtd_cte_t*)(r))
#define VTD_CT_BITS       9
#define VTD_CT_SIZE_BITS  (VTD_CT_BITS + VTD_CTE_SIZE_BITS)

#define VTD_PTE_SIZE_BITS 3
#define VTD_PTE_PTR(r)    ((vtd_pte_t*)(r))
#define VTD_PT_BITS       9
#define VTD_PT_SIZE_BITS  (VTD_PT_BITS + VTD_PTE_SIZE_BITS)

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
    word_t base;
} __attribute__((packed)) gdt_idt_ptr_t;

enum vm_rights {
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef uint32_t vm_rights_t;

#include <mode/object/structures.h>

#endif
