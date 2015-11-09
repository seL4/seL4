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

typedef struct arch_tcb {
    user_context_t tcbContext;
} arch_tcb_t;

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
#define VTD_PT_SIZE_BITS  (VTD_PT_BITS + VTD_PTE_SIZE_BITS)

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

#if defined(X86_32)
#include <arch/object/structures_32.h>
#endif

#endif
