/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_DEBUG_BUILD

#include <arch/machine/capdl.h>
#include <string.h>
#include <kernel/cspace.h>

#define MAX_UL          0xffffffff
#define PT_INDEX(vptr)  ((vptr >> PAGE_BITS) & MASK(PT_INDEX_BITS))
#define PD_INDEX(vptr)  (vptr >> (PAGE_BITS + PT_INDEX_BITS))

word_t get_tcb_sp(tcb_t *tcb)
{
    return tcb->tcbArch.tcbContext.registers[SP];
}

#ifdef CONFIG_PRINTING

static void obj_frame_print_attrs(resolve_ret_t ret);
static void cap_frame_print_attrs_pt(pte_t *pte);
static void cap_frame_print_attrs_pd(pde_t *pde);
static void cap_frame_print_attrs_impl(word_t AP, word_t XN, word_t TEX);
static void arm32_obj_pt_print_slots(pte_t *pt);
static void arm32_cap_pt_print_slots(pte_t *pt);

/*
 * Caps
 */

/*
 * AP   S   R   Privileged permissions  User permissions
 * 00   0   0   No access               No access
 * 00   1   0   Read-only               No access
 * 00   0   1   Read-only               Read-only
 * 00   1   1   Unpredictable           Unpredictable
 * 01   x   x   Read/write              No access
 * 10   x   x   Read/write              Read-only
 * 11   x   x   Read/write              Read/write
 */
/* use when only have access to pte of frames */
static void cap_frame_print_attrs_pt(pte_t *pte)
{
    word_t AP, XN, TEX;
    switch (pte_ptr_get_pteType(pte)) {
    case pte_pte_small:
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        AP = pte_pte_small_ptr_get_HAP(pte);
        TEX = pte_pte_small_ptr_get_MemAttr(pte);
#else
        AP = pte_pte_small_ptr_get_AP(pte);
        TEX = pte_pte_small_ptr_get_TEX(pte);
#endif
        XN = pte_pte_small_ptr_get_XN(pte);
        break;

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    case pte_pte_large:
        AP = pte_pte_large_ptr_get_AP(pte);
        TEX = pte_pte_large_ptr_get_TEX(pte);
        XN = pte_pte_large_ptr_get_XN(pte);
        break;
#endif
    default:
        assert(!"should not happend");
    }
    cap_frame_print_attrs_impl(AP, XN, TEX);
}

static void cap_frame_print_attrs_pd(pde_t *pde)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    cap_frame_print_attrs_impl(pde_pde_section_ptr_get_HAP(pde),
                               pde_pde_section_ptr_get_XN(pde),
                               pde_pde_section_ptr_get_MemAttr(pde));
#else
    cap_frame_print_attrs_impl(pde_pde_section_ptr_get_AP(pde),
                               pde_pde_section_ptr_get_XN(pde),
                               pde_pde_section_ptr_get_TEX(pde));
#endif
}

static void cap_frame_print_attrs_impl(word_t AP, word_t XN, word_t TEX)
{
    printf("(");

    /* rights */
    switch (AP) {
    case 0b00:
    case 0b01:
        break;
    case 0b10:
        printf("R");
        break;
    case 0b11:
        printf("RW");
    default:
        break;
    }

    if (!XN) {
        printf("X");
    }

    if (!TEX) {
        printf(", uncached");
    }

    printf(")\n");
}

static void arm32_cap_pt_print_slots(pte_t *pt)
{
    vm_page_size_t page_size;
    word_t i = 0;
    while (i < BIT(PT_INDEX_BITS + PAGE_BITS)) {
        pte_t *pte = lookupPTSlot_nofail(pt, i);
        switch (pte_ptr_get_pteType(pte)) {
        case pte_pte_small: {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            if (pte_pte_small_ptr_get_contiguous_hint(pte)) {
                page_size = ARMLargePage;
            } else {
                page_size = ARMSmallPage;
            }
#else
            page_size = ARMSmallPage;
#endif
            printf("0x%lx: frame_%p_%04lu ", PT_INDEX(i), pte, PT_INDEX(i));
            cap_frame_print_attrs_pt(pte);
            break;
        }
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        case pte_pte_large: {
            page_size = ARMLargePage;
            printf("0x%lx: frame_%p_%04lu ", PT_INDEX(i), pte, PT_INDEX(i));
            cap_frame_print_attrs_pt(pte);
            break;
        }
#endif
        default:
            page_size = ARMSmallPage;
        }
        i += (1 << pageBitsForSize(page_size));
    }
}

void obj_vtable_print_slots(tcb_t *tcb)
{
    if (isValidVTableRoot(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap) && !seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap)) {
        add_to_seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        pde_t *pd = (pde_t *)pptr_of_cap(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        vm_page_size_t page_size;
        printf("%p_pd {\n", pd);

        /* PD_INDEX_BITS + ARMSectionBits = 32, can't use left shift here */
        word_t i = 0;
        while (i < MAX_UL) {
            pde_t *pde = lookupPDSlot(pd, i);
            switch (pde_ptr_get_pdeType(pde)) {
            case pde_pde_section: {
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
                if (pde_pde_section_ptr_get_size(pde)) {
                    page_size = ARMSuperSection;
                } else {
                    page_size = ARMSection;
                }
#else
                if (pde_pde_section_ptr_get_contiguous_hint(pde)) {
                    page_size = ARMSuperSection;
                } else {
                    page_size = ARMSection;
                }
#endif
                printf("0x%lx: frame_%p_%04lu ", PD_INDEX(i), pde, PD_INDEX(i));
                cap_frame_print_attrs_pd(pde);
                break;
            }

            case pde_pde_coarse: {
                printf("0x%lx: pt_%p_%04lu\n", PD_INDEX(i), pde, PD_INDEX(i));
                page_size = ARMSection;
                break;
            }
            default:
                page_size = ARMSection;
                break;
            }
            i += (1 << pageBitsForSize(page_size));
            if (i < (1 << pageBitsForSize(page_size))) {
                break; /* overflowed */
            }
        }
        printf("}\n"); /* pd */

        i = 0;
        /* PD_INDEX_BITS + ARMSectionBits = 32, can't use left shift here */
        while (i < MAX_UL) {
            pde_t *pde = lookupPDSlot(pd, i);
            if (pde_ptr_get_pdeType(pde) == pde_pde_coarse) {
                pte_t *pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pde));
                printf("pt_%p_%04lu {\n", pde, PD_INDEX(i));
                arm32_cap_pt_print_slots(pt);
                printf("}\n"); /* pt */
            }
            i += (1 << pageBitsForSize(ARMSection));
            if (i < (1 << pageBitsForSize(ARMSection))) {
                break; /* overflowed */
            }
        }
    }
}

/* use when only have access to vptr of frames */
static void cap_frame_print_attrs_vptr(word_t vptr, pde_t *pd)
{
    pde_t *pde = lookupPDSlot(pd, vptr);

    switch (pde_ptr_get_pdeType(pde)) {
    case pde_pde_section: {
        printf("frame_%p_%04lu ", pde, PD_INDEX(vptr));
        cap_frame_print_attrs_pd(pde);
        break;
    }
    case pde_pde_coarse: {
        pte_t *pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pde));
        pte_t *pte = lookupPTSlot_nofail(pt, vptr);
        switch (pte_ptr_get_pteType(pte)) {
        case pte_pte_small: {
            printf("frame_%p_%04lu ", pte, PT_INDEX(vptr));
            cap_frame_print_attrs_pt(pte);
            break;
        }

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        case pte_pte_large: {
            printf("frame_%p_%04lu ", pte, PT_INDEX(vptr));
            cap_frame_print_attrs_pt(pte);
            break;
        }
#endif
        default:
            assert(0);
        }
        break;
    }
    default:
        assert(0);
    }
}

void print_ipc_buffer_slot(tcb_t *tcb)
{
    word_t vptr = tcb->tcbIPCBuffer;
    asid_t asid = cap_page_directory_cap_get_capPDMappedASID(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
    findPDForASID_ret_t find_ret = findPDForASID(asid);
    printf("ipc_buffer_slot: ");
    cap_frame_print_attrs_vptr(vptr, find_ret.pd);
}

void print_cap_arch(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_page_table_cap: {
        asid_t asid = cap_page_table_cap_get_capPTMappedASID(cap);
        findPDForASID_ret_t find_ret = findPDForASID(asid);
        vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(cap);
        if (asid) {
            printf("pt_%p_%04lu (asid: %lu)\n",
                   lookupPDSlot(find_ret.pd, vptr), PD_INDEX(vptr), (long unsigned int)asid);
        } else {
            printf("pt_%p_%04lu\n", lookupPDSlot(find_ret.pd, vptr), PD_INDEX(vptr));
        }
        break;
    }
    case cap_page_directory_cap: {
        asid_t asid = cap_page_directory_cap_get_capPDMappedASID(cap);
        findPDForASID_ret_t find_ret = findPDForASID(asid);
        if (asid) {
            printf("%p_pd (asid: %lu)\n",
                   find_ret.pd, (long unsigned int)asid);
        } else {
            printf("%p_pd\n", find_ret.pd);
        }
        break;
    }
    case cap_asid_control_cap: {
        /* only one in the system */
        printf("asid_control\n");
        break;
    }
    case cap_small_frame_cap: {
        vptr_t vptr = cap_small_frame_cap_get_capFMappedAddress(cap);
        findPDForASID_ret_t find_ret = findPDForASID(cap_small_frame_cap_get_capFMappedASID(cap));
        assert(find_ret.status == EXCEPTION_NONE);
        cap_frame_print_attrs_vptr(vptr, find_ret.pd);
        break;
    }
    case cap_frame_cap: {
        vptr_t vptr = cap_frame_cap_get_capFMappedAddress(cap);
        findPDForASID_ret_t find_ret = findPDForASID(cap_frame_cap_get_capFMappedASID(cap));
        assert(find_ret.status == EXCEPTION_NONE);
        cap_frame_print_attrs_vptr(vptr, find_ret.pd);
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
        printf("[unknown cap %u]\n", cap_get_capType(cap));
        break;
    }
    }
}

void print_object_arch(cap_t cap)
{

    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
    case cap_small_frame_cap:
    case cap_page_table_cap:
    case cap_page_directory_cap:
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
        printf("[unknown object %u]\n", cap_get_capType(cap));
        break;
    }
    }
}

static void obj_frame_print_attrs(resolve_ret_t ret)
{
    printf("(");

    /* VM size */
    switch (ret.frameSize) {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case ARMSection:
        printf("2M");
        break;
    case ARMSuperSection:
        printf("32M");
        break;
#else
    case ARMSection:
        printf("1M");
        break;
    case ARMSuperSection:
        printf("16M");
        break;
#endif
    case ARMLargePage:
        printf("64k");
        break;
    case ARMSmallPage:
        printf("4k");
        break;
    }

    printf(", paddr: %p)\n", (void *)ret.frameBase);
}

static void arm32_obj_pt_print_slots(pte_t *pt)
{
    resolve_ret_t ret;
    word_t i = 0;
    while (i < BIT(PT_INDEX_BITS + PAGE_BITS)) {
        pte_t *pte = lookupPTSlot_nofail(pt, i);
        switch (pte_ptr_get_pteType(pte)) {
        case pte_pte_small: {
            ret.frameBase = pte_pte_small_ptr_get_address(pte);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            if (pte_pte_small_ptr_get_contiguous_hint(pte)) {
                /* Entries are represented as 16 contiguous small frames. We need to mask
                   to get the large frame base */
                ret.frameBase &= ~MASK(pageBitsForSize(ARMLargePage));
                ret.frameSize = ARMLargePage;
            } else {
                ret.frameSize = ARMSmallPage;
            }
#else
            ret.frameSize = ARMSmallPage;
#endif
            printf("frame_%p_%04lu = frame ", pte, PT_INDEX(i));
            obj_frame_print_attrs(ret);
            break;
        }
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        case pte_pte_large: {
            ret.frameBase = pte_pte_large_ptr_get_address(pte);
            ret.frameSize = ARMLargePage;
            printf("frame_%p_%04lu = frame ", pte, PT_INDEX(i));
            obj_frame_print_attrs(ret);
            break;
        }
#endif
        default:
            ret.frameSize = ARMSmallPage;
        }
        i += (1 << pageBitsForSize(ret.frameSize));
    }
}

void obj_tcb_print_vtable(tcb_t *tcb)
{
    if (isValidVTableRoot(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap) && !seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap)) {
        add_to_seen(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        pde_t *pd = (pde_t *)pptr_of_cap(TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap);
        resolve_ret_t ret = {};
        printf("%p_pd = pd\n", pd);

        /* PD_INDEX_BITS + ARMSectionBits = 32, can't use left shift here */
        word_t i = 0;
        while (i < MAX_UL) {
            pde_t *pde = lookupPDSlot(pd, i);
            switch (pde_ptr_get_pdeType(pde)) {
            case pde_pde_section: {
                ret.frameBase = pde_pde_section_ptr_get_address(pde);
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
                if (pde_pde_section_ptr_get_size(pde)) {
                    ret.frameSize = ARMSuperSection;
                } else {
                    ret.frameSize = ARMSection;
                }
#else
                if (pde_pde_section_ptr_get_contiguous_hint(pde)) {
                    /* Entires are represented as 16 contiguous sections. We need to mask
                    to get the super section frame base */
                    ret.frameBase &= ~MASK(pageBitsForSize(ARMSuperSection));
                    ret.frameSize = ARMSuperSection;
                } else {
                    ret.frameSize = ARMSection;
                }
#endif
                printf("frame_%p_%04lu = frame ", pde, PD_INDEX(i));
                obj_frame_print_attrs(ret);
                break;
            }

            case pde_pde_coarse: {
                pte_t *pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pde));
                printf("pt_%p_%04lu = pt\n", pde, PD_INDEX(i));
                arm32_obj_pt_print_slots(pt);
                ret.frameSize = ARMSection;
                break;
            }
            default:
                ret.frameSize = ARMSection;
                break;
            }
            i += (1 << pageBitsForSize(ret.frameSize));
            if (i < (1 << pageBitsForSize(ret.frameSize))) {
                break; /* overflowed */
            }
        }
    }
}

#endif /* CONFIG_PRINTING */

void debug_capDL(void)
{
    printf("arch aarch32\n");
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
