/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_DEBUG_BUILD

#include <arch/machine/capdl.h>
#include <string.h>
#include <kernel/cspace.h>

word_t get_tcb_sp(tcb_t *tcb)
{
    return tcb->tcbArch.tcbContext.registers[SP_EL0];
}

#ifdef CONFIG_PRINTING

static void obj_frame_print_attrs(lookupFrame_ret_t ret);
static void cap_frame_print_attrs_pud(pude_t *pudSlot);
static void cap_frame_print_attrs_pd(pde_t *pdSlot);
static void cap_frame_print_attrs_pt(pte_t *ptSlot);
static void cap_frame_print_attrs_impl(word_t SH, word_t AP, word_t NXN);
static void cap_frame_print_attrs_vptr(word_t vptr, cap_t vspace);

static void _cap_frame_print_attrs_vptr(word_t vptr, vspace_root_t *vspaceRoot);

static void arm64_obj_pt_print_slots(pde_t *pdSlot);
static void arm64_obj_pd_print_slots(pude_t *pudSlot);
static void arm64_obj_pud_print_slots(void *pgdSlot_or_vspace);

static void arm64_cap_pt_print_slots(pde_t *pdSlot, vptr_t vptr);
static void arm64_cap_pd_print_slots(pude_t *pudSlot, vptr_t vptr);
static void arm64_cap_pud_print_slots(void *pgdSlot_or_vspace, vptr_t vptr);

/* Stage-1 access permissions:
 * AP[2:1]  higer EL        EL0
 *   00       rw            None
 *   01       rw            rw
 *   10       r             None
 *   11       r             r
 *
 * Stage-2 access permissions:
 * S2AP    Access from Nonsecure EL1 or Non-secure EL0
 *  00                      None
 *  01                      r
 *  10                      w
 *  11                      rw
 *
 *  For VMs or native seL4 applications, if hypervisor support
 *  is enabled, we use the S2AP. The kernel itself running in
 *  EL2 still uses the Stage-1 AP format.
 */
/* use when only have access to pte of frames */
static void cap_frame_print_attrs_pud(pude_t *pudSlot)
{
    cap_frame_print_attrs_impl(pude_pude_1g_ptr_get_SH(pudSlot),
                               pude_pude_1g_ptr_get_AP(pudSlot),
                               pude_pude_1g_ptr_get_UXN(pudSlot));
}

static void cap_frame_print_attrs_pd(pde_t *pdSlot)
{
    cap_frame_print_attrs_impl(pde_pde_large_ptr_get_SH(pdSlot),
                               pde_pde_large_ptr_get_AP(pdSlot),
                               pde_pde_large_ptr_get_UXN(pdSlot));
}

static void cap_frame_print_attrs_pt(pte_t *ptSlot)
{
    cap_frame_print_attrs_impl(pte_ptr_get_SH(ptSlot),
                               pte_ptr_get_AP(ptSlot),
                               pte_ptr_get_UXN(ptSlot));
}

static void cap_frame_print_attrs_impl(word_t SH, word_t AP, word_t NXN)
{
    printf("(");

    /* rights */
    switch (AP) {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case 0b00:
        break;
    case 0b01:
        printf("R");
        break;
    case 0b10:
        printf("W");
        break;
    case 0b11:
        printf("RW");
        break;
#else
    case 0b00:
        break;
    case 0b01:
        printf("RW");
    case 0b10:
        break;
    case 0b11:
        printf("R");
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
    default:
        break;
    }

    if (!NXN) {
        printf("X");
    }

    /* Only has effect if SMP enabled */
    if (SH != SMP_TERNARY(SMP_SHARE, 0)) {
        printf(", uncached");
    }

    printf(")\n");
}

/* use when only have access to vptr of frames */
static void _cap_frame_print_attrs_vptr(word_t vptr, vspace_root_t *vspace)
{
    lookupPUDSlot_ret_t pudSlot = lookupPUDSlot(vspace, vptr);
    if (pudSlot.status != EXCEPTION_NONE) {
        return;
    }

    switch (pude_ptr_get_pude_type(pudSlot.pudSlot)) {
    case pude_pude_1g:
        printf("frame_%p_%04lu ", pudSlot.pudSlot, GET_PUD_INDEX(vptr));
        cap_frame_print_attrs_pud(pudSlot.pudSlot);
        break;

    case pude_pude_pd: {
        pde_t *pd = paddr_to_pptr(pude_pude_pd_ptr_get_pd_base_address(pudSlot.pudSlot));
        pde_t *pdSlot = pd + GET_PD_INDEX(vptr);

        switch (pde_ptr_get_pde_type(pdSlot)) {
        case pde_pde_large:
            printf("frame_%p_%04lu ", pdSlot, GET_PD_INDEX(vptr));
            cap_frame_print_attrs_pd(pdSlot);
            break;

        case pde_pde_small: {
            pte_t *pt = paddr_to_pptr(pde_pde_small_ptr_get_pt_base_address(pdSlot));
            pte_t *ptSlot = pt + GET_PT_INDEX(vptr);

            if (pte_ptr_get_present(ptSlot)) {
                printf("frame_%p_%04lu ", ptSlot, GET_PT_INDEX(vptr));
                cap_frame_print_attrs_pt(ptSlot);
                break;
            } else {
                return;
            }
        }
        default:
            assert(0);
        }
        break;
    }
    default:
        assert(0);
    }
}

void cap_frame_print_attrs_vptr(word_t vptr, cap_t vspace)
{
    _cap_frame_print_attrs_vptr(vptr, VSPACE_PTR(pptr_of_cap(vspace)));
}

/*
 * print object slots
 */
static void arm64_cap_pt_print_slots(pde_t *pdSlot, vptr_t vptr)
{
    pte_t *pt = paddr_to_pptr(pde_pde_small_ptr_get_pt_base_address(pdSlot));
    printf("pt_%p_%04lu {\n", pdSlot, GET_PD_INDEX(vptr));

    for (word_t i = 0; i < BIT(PT_INDEX_OFFSET + PT_INDEX_BITS); i += (1 << PT_INDEX_OFFSET)) {
        pte_t *ptSlot = pt + GET_PT_INDEX(i);

        if (pte_ptr_get_present(ptSlot)) {
            // print pte entries
            printf("0x%lx: frame_%p_%04lu", GET_PT_INDEX(i), ptSlot, GET_PT_INDEX(i));
            cap_frame_print_attrs_pt(ptSlot);
        }
    }
    printf("}\n"); /* pt */
}

static void arm64_cap_pd_print_slots(pude_t *pudSlot, vptr_t vptr)
{
    printf("pd_%p_%04lu {\n", pudSlot, GET_PUD_INDEX(vptr));
    pde_t *pd = paddr_to_pptr(pude_pude_pd_ptr_get_pd_base_address(pudSlot));

    for (word_t i = 0; i < BIT(PD_INDEX_OFFSET + PD_INDEX_BITS); i += (1 << PD_INDEX_OFFSET)) {
        pde_t *pdSlot = pd + GET_PD_INDEX(i);

        switch (pde_ptr_get_pde_type(pdSlot)) {

        case pde_pde_large:
            printf("0x%lx: frame_%p_%04lu", GET_PD_INDEX(i), pdSlot, GET_PD_INDEX(i));
            cap_frame_print_attrs_pd(pdSlot);
            break;

        case pde_pde_small:
            printf("0x%lx: pt_%p_%04lu\n", GET_PD_INDEX(i), pdSlot, GET_PD_INDEX(i));
            break;
        }
    }

    printf("}\n"); /* pd */

    for (word_t i = 0; i < BIT(PD_INDEX_OFFSET + PD_INDEX_BITS); i += (1 << PD_INDEX_OFFSET)) {
        pde_t *pdSlot = pd + GET_PD_INDEX(i);
        if (pde_ptr_get_pde_type(pdSlot) == pde_pde_small) {
            arm64_cap_pt_print_slots(pdSlot, i);
        }
    }
}

static void arm64_cap_pud_print_slots(void *pgdSlot_or_vspace, vptr_t vptr)
{
#ifdef AARCH64_VSPACE_S2_START_L1
    pude_t *pud = pgdSlot_or_vspace;
    printf("%p_pd {\n", pgdSlot_or_vspace);
#else
    pude_t *pud = paddr_to_pptr(pgde_pgde_pud_ptr_get_pud_base_address(pgdSlot_or_vspace));
    printf("pud_%p_%04lu {\n", pgdSlot_or_vspace, GET_PGD_INDEX(vptr));
#endif

    for (word_t i = 0; i < BIT(PUD_INDEX_OFFSET + UPUD_INDEX_BITS); i += (1 << PUD_INDEX_OFFSET)) {
        pude_t *pudSlot = pud + GET_PUD_INDEX(i);
        if (pude_ptr_get_pude_type(pudSlot) == pude_pude_pd) {
            printf("0x%lx: pd_%p_%04lu\n", GET_PUD_INDEX(i), pudSlot, GET_PUD_INDEX(i));
        }
    }

    printf("}\n"); /* pgd/pud */

    for (word_t i = 0; i < BIT(PUD_INDEX_OFFSET + UPUD_INDEX_BITS); i += (1 << PUD_INDEX_OFFSET)) {
        pude_t *pudSlot = pud + GET_PUD_INDEX(i);
        if (pude_ptr_get_pude_type(pudSlot) == pude_pude_pd) {
            arm64_cap_pd_print_slots(pudSlot, i);
        }
    }
}

void obj_vtable_print_slots(tcb_t *tcb)
{
    if (isVTableRoot(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap) && !seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap)) {
        add_to_seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        vspace_root_t *vspace = cap_vtable_root_get_basePtr(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);

        /*
        * ARM hyp uses 3 level translation rather than the usual 4 level.
        * levels: PGD -> UPD -> PD -> PT
        */
#ifdef AARCH64_VSPACE_S2_START_L1
        arm64_cap_pud_print_slots(vspace, 0);
#else
        printf("%p_pd {\n", vspace);
        for (word_t i = 0; i < BIT(PGD_INDEX_OFFSET + PGD_INDEX_BITS); i += (1UL << PGD_INDEX_OFFSET)) {
            lookupPGDSlot_ret_t pgdSlot = lookupPGDSlot(vspace, i);
            if (pgde_pgde_pud_ptr_get_present(pgdSlot.pgdSlot)) {
                printf("0x%lx: pud_%p_%04lu\n", GET_PGD_INDEX(i), pgdSlot.pgdSlot, GET_PGD_INDEX(i));
            }
        }
        printf("}\n"); /* pd */

        for (word_t i = 0; i < BIT(PGD_INDEX_OFFSET + PGD_INDEX_BITS); i += (1UL << PGD_INDEX_OFFSET)) {
            lookupPGDSlot_ret_t pgdSlot = lookupPGDSlot(vspace, i);
            if (pgde_pgde_pud_ptr_get_present(pgdSlot.pgdSlot)) {
                arm64_cap_pud_print_slots(pgdSlot.pgdSlot, i);
            }
        }
#endif
    }
}

void print_ipc_buffer_slot(tcb_t *tcb)
{
    word_t vptr = tcb->tcbIPCBuffer;
    printf("ipc_buffer_slot: ");
    cap_frame_print_attrs_vptr(vptr, TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
}

void print_cap_arch(cap_t cap)
{

    switch (cap_get_capType(cap)) {
    case cap_page_table_cap: {
        asid_t asid = cap_page_table_cap_get_capPTMappedASID(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
        vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(cap);
        if (asid) {
            printf("pt_%p_%04lu (asid: %lu)\n",
                   lookupPDSlot(find_ret.vspace_root, vptr).pdSlot, GET_PD_INDEX(vptr), (long unsigned int)asid);
        } else {
            printf("pt_%p_%04lu\n", lookupPDSlot(find_ret.vspace_root, vptr).pdSlot, GET_PD_INDEX(vptr));
        }
        break;
    }
    case cap_page_directory_cap: {
        asid_t asid = cap_page_directory_cap_get_capPDMappedASID(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
        vptr_t vptr = cap_page_directory_cap_get_capPDMappedAddress(cap);
        if (asid) {
            printf("pd_%p_%04lu (asid: %lu)\n",
                   lookupPUDSlot(find_ret.vspace_root, vptr).pudSlot, GET_PUD_INDEX(vptr), (long unsigned int)asid);
        } else {
            printf("pd_%p_%04lu\n",
                   lookupPUDSlot(find_ret.vspace_root, vptr).pudSlot, GET_PUD_INDEX(vptr));
        }
        break;
    }
    case cap_page_upper_directory_cap: {
        asid_t asid = cap_page_upper_directory_cap_get_capPUDMappedASID(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
        vptr_t vptr = cap_page_upper_directory_cap_get_capPUDMappedAddress(cap);

#ifdef AARCH64_VSPACE_S2_START_L1
        if (asid) {
            printf("pud_%p_%04lu (asid: %lu)\n",
                   find_ret.vspace_root, GET_PGD_INDEX(vptr), (long unsigned int)asid);
        } else {
            printf("pud_%p_%04lu\n", find_ret.vspace_root, GET_PGD_INDEX(vptr));
        }
#else
        if (asid) {
            printf("pud_%p_%04lu (asid: %lu)\n",
                   lookupPGDSlot(find_ret.vspace_root, vptr).pgdSlot, GET_PGD_INDEX(vptr), (long unsigned int)asid);
        } else {
            printf("pud_%p_%04lu\n", lookupPGDSlot(find_ret.vspace_root, vptr).pgdSlot, GET_PGD_INDEX(vptr));
        }
#endif
        break;
    }
    case cap_page_global_directory_cap: {
        asid_t asid = cap_page_global_directory_cap_get_capPGDMappedASID(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
        if (asid) {
            printf("%p_pd (asid: %lu)\n",
                   find_ret.vspace_root, (long unsigned int)asid);
        } else {
            printf("%p_pd\n", find_ret.vspace_root);
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
        _cap_frame_print_attrs_vptr(vptr, find_ret.vspace_root);
        break;
    }
    case cap_asid_pool_cap: {
        printf("%p_asid_pool\n", (void *)cap_asid_pool_cap_get_capASIDPool(cap));
        break;
    }
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap: {
        printf("%p_vcpu\n", (void *)cap_vcpu_cap_get_capVCPUPtr(cap));
        break;
    }
#endif

        /* ARM specific caps */
#ifdef CONFIG_TK1_SMMU
    case cap_io_space_cap: {
        printf("%p_io_space\n", (void *)cap_io_space_cap_get_capModuleID(cap));
        break;
    }
#endif
    default: {
        printf("[unknown cap %lu]\n", (long unsigned int)cap_get_capType(cap));
        break;
    }
    }
}

void print_object_arch(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
    case cap_page_table_cap:
    case cap_page_directory_cap:
    case cap_page_upper_directory_cap:
    case cap_page_global_directory_cap:
        /* don't need to deal with these objects since they get handled from vtable */
        break;

    case cap_asid_pool_cap: {
        printf("%p_asid_pool = asid_pool ",
               (void *)cap_asid_pool_cap_get_capASIDPool(cap));
        obj_asidpool_print_attrs(cap);
        break;
    }
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap: {
        printf("%p_vcpu = vcpu\n", (void *)cap_vcpu_cap_get_capVCPUPtr(cap));
        break;
    }
#endif
        /* ARM specific objects */
#ifdef CONFIG_TK1_SMMU
    case cap_io_space_cap: {
        printf("%p_io_space = io_space ", (void *)cap_io_space_cap_get_capModuleID(cap));
        arm_obj_iospace_print_attrs(cap);
        break;
    }
#endif
    default: {
        printf("[unknown object %lu]\n", (long unsigned int)cap_get_capType(cap));
        break;
    }
    }
}

void obj_frame_print_attrs(lookupFrame_ret_t ret)
{
    printf("(");

    /* VM size */
    switch (ret.frameSize) {
    case ARMHugePage:
        printf("1G");
        break;
    case ARMLargePage:
        printf("2M");
        break;
    case ARMSmallPage:
        printf("4k");
        break;
    }

    printf(", paddr: 0x%p)\n", (void *)ret.frameBase);
}

void arm64_obj_pt_print_slots(pde_t *pdSlot)
{
    lookupFrame_ret_t ret;
    pte_t *pt = paddr_to_pptr(pde_pde_small_ptr_get_pt_base_address(pdSlot));

    for (word_t i = 0; i < BIT(PT_INDEX_OFFSET + PT_INDEX_BITS); i += (1 << PT_INDEX_OFFSET)) {
        pte_t *ptSlot = pt + GET_PT_INDEX(i);

        if (pte_ptr_get_present(ptSlot)) {
            ret.frameBase = pte_ptr_get_page_base_address(ptSlot);
            ret.frameSize = ARMSmallPage;
            printf("frame_%p_%04lu = frame ", ptSlot, GET_PT_INDEX(i));
            obj_frame_print_attrs(ret);
        }
    }
}

void arm64_obj_pd_print_slots(pude_t *pudSlot)
{
    lookupFrame_ret_t ret;
    pde_t *pd = paddr_to_pptr(pude_pude_pd_ptr_get_pd_base_address(pudSlot));

    for (word_t i = 0; i < BIT(PD_INDEX_OFFSET + PD_INDEX_BITS); i += (1 << PD_INDEX_OFFSET)) {
        pde_t *pdSlot = pd + GET_PD_INDEX(i);

        if (pde_ptr_get_pde_type(pdSlot) == pde_pde_large) {
            ret.frameBase = pde_pde_large_ptr_get_page_base_address(pdSlot);
            ret.frameSize = ARMLargePage;

            printf("frame_%p_%04lu = frame ", pdSlot, GET_PD_INDEX(i));
            obj_frame_print_attrs(ret);
        }

        if (pde_ptr_get_pde_type(pdSlot) == pde_pde_small) {
            printf("pt_%p_%04lu = pt\n", pdSlot, GET_PD_INDEX(i));
            arm64_obj_pt_print_slots(pdSlot);
        }
    }
}

void arm64_obj_pud_print_slots(void *pgdSlot_or_vspace)
{
    lookupFrame_ret_t ret;
    pude_t *pud = paddr_to_pptr(pgde_pgde_pud_ptr_get_pud_base_address(pgdSlot_or_vspace));

    for (word_t i = 0; i < BIT(PUD_INDEX_OFFSET + UPUD_INDEX_BITS); i += (1 << PUD_INDEX_OFFSET)) {
        pude_t *pudSlot = pud + GET_PUD_INDEX(i);

        switch (pude_ptr_get_pude_type(pudSlot)) {
        case pude_pude_1g:
            ret.frameBase = pude_pude_1g_ptr_get_page_base_address(pudSlot);
            ret.frameSize = ARMHugePage;

            printf("frame_%p_%04lu = frame ", pudSlot, GET_PUD_INDEX(i));
            obj_frame_print_attrs(ret);
            break;

        case pude_pude_pd: {
            printf("pd_%p_%04lu = pd\n", pudSlot, GET_PUD_INDEX(i));
            arm64_obj_pd_print_slots(pudSlot);

        }
        }
    }
}

void obj_tcb_print_vtable(tcb_t *tcb)
{
    if (isVTableRoot(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap) && !seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap)) {
        add_to_seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        vspace_root_t *vspace = cap_vtable_root_get_basePtr(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);

        /*
         * ARM hyp uses 3 level translation rather than the usual 4 level.
         * levels: PGD -> PUD -> PD -> PT
         */
#ifdef AARCH64_VSPACE_S2_START_L1
        printf("%p_pd = pud\n", vspace);
        arm64_obj_pud_print_slots(vspace);
#else
        printf("%p_pd = pgd\n", vspace);
        for (word_t i = 0; i < BIT(PGD_INDEX_OFFSET + PGD_INDEX_BITS); i += (1UL << PGD_INDEX_OFFSET)) {
            lookupPGDSlot_ret_t pgdSlot = lookupPGDSlot(vspace, i);
            if (pgde_pgde_pud_ptr_get_present(pgdSlot.pgdSlot)) {
                printf("pud_%p_%04lu = pud\n", pgdSlot.pgdSlot, GET_PGD_INDEX(i));
                arm64_obj_pud_print_slots(pgdSlot.pgdSlot);
            }
        }
#endif
    }
}

#endif /* CONFIG_PRINTING */

void debug_capDL(void)
{
    printf("arch aarch64\n");
    printf("objects {\n");
    print_objects();
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
