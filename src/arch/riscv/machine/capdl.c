/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_DEBUG_BUILD

#include <machine/capdl.h>
#include <arch/machine/capdl.h>

word_t get_tcb_sp(tcb_t *tcb)
{
    return tcb->tcbArch.tcbContext.registers[SP];
}

#ifdef CONFIG_PRINTING

static void obj_asidpool_print_attrs(cap_t asid_cap);
static void obj_frame_print_attrs(paddr_t paddr);
static void riscv_obj_pt_print_slots(pte_t *lvl1pt, pte_t *pt, int level);
static void cap_frame_print_attrs_vptr(word_t vptr, pte_t *lvl1pt);
static void cap_frame_print_attrs_pt(pte_t *ptSlot);

static void obj_asidpool_print_attrs(cap_t asid_cap)
{
    asid_t asid = cap_asid_pool_cap_get_capASIDBase(asid_cap);
    printf("(asid_high: 0x%lx)\n", ASID_HIGH(asid));
}

void print_ipc_buffer_slot(tcb_t *tcb)
{
    word_t vptr = tcb->tcbIPCBuffer;
    asid_t asid = cap_page_table_cap_get_capPTMappedASID(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
    findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);

    printf("ipc_buffer_slot: ");
    cap_frame_print_attrs_vptr(vptr, find_ret.vspace_root);
}

static void riscv_cap_pt_print_slots(pte_t *upperPtSlot, word_t ptIndex, int level)
{
    pte_t *pt;
    if (level == CONFIG_PT_LEVELS) {
        printf("%p_pd {\n", upperPtSlot);
        pt = upperPtSlot;
    } else {
        printf("pt_%p_%04lu {\n", upperPtSlot, ptIndex);
        pt = getPPtrFromHWPTE(upperPtSlot);
    }
    level -= 1;

    word_t ptBitsLeft = PT_INDEX_BITS * level + seL4_PageBits;

    /* - 1 to avoid overflowing */
    for (word_t i = 0; i < BIT(ptBitsLeft + PT_INDEX_BITS) - 1; i += (1 << (ptBitsLeft))) {
        word_t ptSlotIndex = ((i >> ptBitsLeft) & MASK(PT_INDEX_BITS));
        pte_t *ptSlot = pt + ptSlotIndex;
        if (pte_ptr_get_valid(ptSlot)) {
            if (level) { /* pt */
                printf("0x%lx: pt_%p_%04lu\n", ptSlotIndex, ptSlot, ptSlotIndex);
            } else { /* frame */
                printf("0x%lx: frame_%p_%04lu", ptSlotIndex, ptSlot, ptSlotIndex);
                cap_frame_print_attrs_pt(ptSlot);
            }
        }
    }
    printf("}\n"); /* lvl1pt/pt */

    for (word_t i = 0; i < BIT(ptBitsLeft + PT_INDEX_BITS) - 1; i += (1 << (ptBitsLeft))) {
        word_t ptSlotIndex = ((i >> ptBitsLeft) & MASK(PT_INDEX_BITS));
        pte_t *ptSlot = pt + ptSlotIndex;
        if (pte_ptr_get_valid(ptSlot)) {
            if (level) { /* pt */
                riscv_cap_pt_print_slots(ptSlot, ptSlotIndex, level);
            }
        }
    }
}

void obj_vtable_print_slots(tcb_t *tcb)
{
    if (isValidVTableRoot(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap) && !seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap)) {
        pte_t *lvl1pt = PTE_PTR(pptr_of_cap(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap));
        add_to_seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        riscv_cap_pt_print_slots(lvl1pt, 0, CONFIG_PT_LEVELS);
    }
}

static void cap_frame_print_attrs_pt(pte_t *ptSlot)
{
    printf("(");

    /* rights */
    if (pte_ptr_get_read(ptSlot)) {
        printf("R");
    }

    if (pte_ptr_get_write(ptSlot)) {
        printf("W");
    }

    if (pte_ptr_get_execute(ptSlot)) {
        printf("X");
    }

    /* cacheable not supported yet */

    printf(")\n");
}

static void cap_frame_print_attrs_vptr(word_t vptr, pte_t *lvl1pt)
{
    lookupPTSlot_ret_t lu_ret = lookupPTSlot(lvl1pt, vptr);
    assert(lu_ret.ptBitsLeft == seL4_PageBits);
    word_t slot = ((vptr >> lu_ret.ptBitsLeft) & MASK(PT_INDEX_BITS));

    printf("frame_%p_%04lu ", lu_ret.ptSlot, slot);
    cap_frame_print_attrs_pt(lu_ret.ptSlot);
}

void print_cap_arch(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_page_table_cap: {
        asid_t asid = cap_page_table_cap_get_capPTMappedASID(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
        vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(cap);

        word_t ptBitsLeft = PT_INDEX_BITS * CONFIG_PT_LEVELS + seL4_PageBits;
        word_t slot = ((vptr >> ptBitsLeft) & MASK(PT_INDEX_BITS));
        if (asid) {
            printf("pt_%p_%04lu (asid: %lu)\n",
                   lookupPTSlot(find_ret.vspace_root, vptr).ptSlot, slot, (long unsigned int)asid);
        } else {
            printf("pt_%p_%04lu\n", lookupPTSlot(find_ret.vspace_root, vptr).ptSlot, slot);
        }
        break;
    }
    case cap_asid_control_cap: {
        /* only one in the system */
        printf("asid_control\n");
        break;
    }
    case cap_frame_cap: {
        vptr_t vptr = cap_frame_cap_get_capFMappedAddress(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(cap_frame_cap_get_capFMappedASID(cap));

        assert(find_ret.status == EXCEPTION_NONE);
        cap_frame_print_attrs_vptr(vptr, find_ret.vspace_root);
        break;
    }
    case cap_asid_pool_cap: {
        printf("%p_asid_pool\n", (void *)cap_asid_pool_cap_get_capASIDPool(cap));
        break;
    }
    /* riscv specific caps */
    /* nothing */
    default: {
        printf("[unknown cap %lu]\n", (long unsigned int)cap_get_capType(cap));
        break;
    }
    }
}

static void obj_frame_print_attrs(paddr_t paddr)
{
    printf("(4k, paddr: 0x%p)\n", (void *)paddr);
}

void print_object_arch(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
    case cap_page_table_cap:
        /* don't need to deal with these objects since they get handled from vtable */
        break;

    case cap_asid_pool_cap: {
        printf("%p_asid_pool = asid_pool ",
               (void *)cap_asid_pool_cap_get_capASIDPool(cap));
        obj_asidpool_print_attrs(cap);
        break;
    }
    /* riscv specific caps */
    /* nothing */
    default: {
        printf("[unknown object %lu]\n", (long unsigned int)cap_get_capType(cap));
        break;
    }
    }
}

static void riscv_obj_pt_print_slots(pte_t *lvl1pt, pte_t *pt, int level)
{
    word_t ptBitsLeft = PT_INDEX_BITS * level + seL4_PageBits;

    for (word_t i = 0; i < BIT(ptBitsLeft + PT_INDEX_BITS); i += (1 << (ptBitsLeft))) {
        word_t ptIndex = ((i >> ptBitsLeft) & MASK(PT_INDEX_BITS));
        pte_t *ptSlot = pt + ptIndex;
        if (pte_ptr_get_valid(ptSlot)) {
            if (level) { /* pt */
                printf("pt_%p_%04lu = pt\n", ptSlot, ptIndex);
                riscv_obj_pt_print_slots(lvl1pt, getPPtrFromHWPTE(ptSlot), level - 1);
            } else { /* frame */
                paddr_t paddr = pte_ptr_get_ppn(ptSlot);
                printf("frame_%p_%04lu = frame ", ptSlot, ptIndex);
                obj_frame_print_attrs(paddr);
            }
        }
    }
}

void obj_tcb_print_vtable(tcb_t *tcb)
{
    if (isValidVTableRoot(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap) && !seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap)) {
        add_to_seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        pte_t *lvl1pt = PTE_PTR(pptr_of_cap(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap));
        printf("%p_pd = pt\n", lvl1pt);
        riscv_obj_pt_print_slots(lvl1pt, lvl1pt, CONFIG_PT_LEVELS - 1);
    }
}

#endif /* CONFIG_PRINTING */

void debug_capDL(void)
{
    printf("arch riscv\n");
    printf("objects {\n");
#ifdef CONFIG_PRINTING
    print_objects();
#endif
    printf("}\n");

    printf("caps {\n");

    /* reset the seen list */
    reset_seen_list();

#ifdef CONFIG_PRINTING
    print_caps();
    printf("}\n");

    obj_irq_print_maps();
#endif /* CONFIG_PRINTING */
}

#endif /* CONFIG_DEBUG_BUILD */
