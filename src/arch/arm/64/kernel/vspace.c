/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <benchmark/benchmark.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <kernel/boot.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <kernel/stack.h>
#include <machine/io.h>
#include <machine/debug.h>
#include <model/statedata.h>
#include <object/cnode.h>
#include <object/untyped.h>
#include <arch/api/invocation.h>
#include <arch/kernel/vspace.h>
#include <linker.h>
#include <plat/machine/hardware.h>
#include <armv/context_switch.h>
#include <arch/object/iospace.h>
#include <arch/object/vcpu.h>
#include <arch/machine/tlb.h>

/*
 * Memory types are defined in Memory Attribute Indirection Register.
 *  - nGnRnE Device non-Gathering, non-Reordering, No Early write acknowledgement
 *  - nGnRE Unused Device non-Gathering, non-Reordering, Early write acknowledgement
 *  - GRE Unused Device Gathering, Reordering, Early write acknowledgement
 *  - NORMAL_NC Normal Memory, Inner/Outer non-cacheable
 *  - NORMAL Normal Memory, Inner/Outer Write-back non-transient, Write-allocate, Read-allocate
 *  - NORMAL_WT Normal Memory, Inner/Outer Write-through non-transient, No-Write-allocate, Read-allocate
 * Note: These should match with contents of MAIR_EL1 register!
 */
enum mair_types {
    DEVICE_nGnRnE = 0,
    DEVICE_nGnRE = 1,
    DEVICE_GRE = 2,
    NORMAL_NC = 3,
    NORMAL = 4,
    NORMAL_WT = 5
};

/* Stage-2 translation memory attributes */
enum mair_s2_types {
    S2_DEVICE_nGnRnE = 0,
    S2_DEVICE_nGnRE = 1,
    S2_DEVICE_nGRE  = 2,
    S2_DEVICE_GRE = 3,

    S2_NORMAL_INNER_NC_OUTER_NC = 5,
    S2_NORMAL_INNER_WTC_OUTER_NC = 6,
    S2_NORMAL_INNER_WBC_OUTER_NC = 7,

    S2_NORMAL_INNER_NC_OUTER_WTC = 9,
    S2_NORMAL_INNER_WTC_OUTER_WTC = 10,
    S2_NORMAL_INNER_WBC_OUTER_WTC = 11,

    S2_NORMAL_INNER_NC_OUTER_WBC = 13,
    S2_NORMAL_INNER_WTC_OUTER_WBC = 14,
    S2_NORMAL_INNER_WBC_OUTER_WBC = 15,

    S2_NORMAL = S2_NORMAL_INNER_WBC_OUTER_WBC
};

/* Leif from Linaro said the big.LITTLE clusters should be treated as
 * inner shareable, and we believe so, although the Example B2-1 given in
 * ARM ARM DDI 0487B.b (ID092517) says otherwise.
 */

#define SMP_SHARE   3

struct lookupPTSlot_ret {
    pte_t *ptSlot;
    word_t ptBitsLeft;
};
typedef struct lookupPTSlot_ret lookupPTSlot_ret_t;

struct findVSpaceForASID_ret {
    exception_t status;
    vspace_root_t *vspace_root;
};
typedef struct findVSpaceForASID_ret findVSpaceForASID_ret_t;

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

static word_t CONST APFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMKernelOnly:
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            return 0;
        } else {
            return 0;
        }

    case VMReadWrite:
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            return 3;
        } else {
            return 1;
        }

    case VMReadOnly:
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            return 1;
        } else {
            return 3;
        }

    default:
        fail("Invalid VM rights");
    }
}

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
static inline CONST word_t pte_get_AP(pte_t pte)
{
    assert(pte_is_page_type(pte));
    switch (pte_get_pte_type(pte)) {
    case pte_pte_4k_page:
        return pte_pte_4k_page_get_AP(pte);

    case pte_pte_page:
        return pte_pte_page_get_AP(pte);

    default:
        return 0;
    }
}

static vm_rights_t CONST vmRightsFromPTE(pte_t pte)
{
    word_t access_perms = pte_get_AP(pte);
    switch (access_perms) {
    case 0:
    case 2:
        return VMKernelOnly;
    case 1:
        return VMReadWrite;
    case 3:
        return VMReadOnly;
    default:
        fail("Invalid AP bit");
    }
}
#endif

vm_rights_t CONST maskVMRights(vm_rights_t vm_rights, seL4_CapRights_t cap_rights_mask)
{
    if (vm_rights == VMReadOnly &&
        seL4_CapRights_get_capAllowRead(cap_rights_mask)) {
        return VMReadOnly;
    }
    if (vm_rights == VMReadWrite &&
        seL4_CapRights_get_capAllowRead(cap_rights_mask)) {
        if (!seL4_CapRights_get_capAllowWrite(cap_rights_mask)) {
            return VMReadOnly;
        } else {
            return VMReadWrite;
        }
    }
    if (vm_rights == VMReadWrite &&
        !seL4_CapRights_get_capAllowRead(cap_rights_mask) &&
        seL4_CapRights_get_capAllowWrite(cap_rights_mask)) {
        userError("Attempted to make unsupported write only mapping");
    }
    return VMKernelOnly;
}

/* ==================== BOOT CODE STARTS HERE ==================== */

/* The 54th bit is defined as UXN (unprivileged execute-never) for stage 1
 * of any tranlsation regime for which stage 1 translation can support
 * two VA ranges. This field applies only to execution at EL0. A value
 * of 0 indicates that this control permits execution.
 *
 * The 54th bit is defined as XN (execute-never) for stage 1 of any translation
 * regime for which the stage 1 translation can support only a singe VA range or
 * stage 2 translation when ARMVv8.2-TTS2UXN is not implemented.
 * This field applies to execution at any exception level to which the stage of
 * translation applies. A value of 0 indicates that this control permits execution.
 *
 * When the kernel is running in EL2, the stage-1 translation only supports one
 * VA range so that the 54th bit is XN. Setting the bit to 0 allows execution.
 *
 */
BOOT_CODE void map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights, vm_attributes_t attributes)
{
    assert(vaddr >= PPTR_TOP);

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    word_t uxn = vm_attributes_get_armExecuteNever(attributes);
#else
    word_t uxn = 1; /* unprivileged execute never */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
    word_t attr_index;
    word_t shareable;
    if (vm_attributes_get_armPageCacheable(attributes)) {
        attr_index = NORMAL;
        shareable = SMP_TERNARY(SMP_SHARE, 0);
    } else {
        attr_index = DEVICE_nGnRnE;
        shareable = 0;
    }
    armKSGlobalKernelPT[GET_KPT_INDEX(vaddr, KLVL_FRM_ARM_PT_LVL(3))] = pte_pte_4k_page_new(
#if defined(CONFIG_HAVE_CHERI)
                                                                                            1, 1, 0, /* Enable capability loads/stores for the kernel */
#endif
                                                                                            uxn, paddr,
                                                                                            0, /* global */
                                                                                            1, /* access flag */
                                                                                            shareable,
                                                                                            APFromVMRights(vm_rights),
                                                                                            attr_index);
}

BOOT_CODE void map_kernel_window(void)
{

    paddr_t paddr;
    pptr_t vaddr;
    word_t idx;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    /* verify that the kernel window as at the second entry of the PGD */
    assert(GET_KPT_INDEX(PPTR_BASE, KLVL_FRM_ARM_PT_LVL(0)) == 1);
#else
    /* verify that the kernel window as at the last entry of the PGD */
    assert(GET_KPT_INDEX(PPTR_BASE, KLVL_FRM_ARM_PT_LVL(0)) == BIT(PT_INDEX_BITS) - 1);
#endif
    assert(IS_ALIGNED(PPTR_BASE, seL4_LargePageBits));
    /* verify that the kernel device window is 1gb aligned and 1gb in size */
    assert(GET_KPT_INDEX(PPTR_TOP, KLVL_FRM_ARM_PT_LVL(1)) == BIT(PT_INDEX_BITS) - 1);
    assert(IS_ALIGNED(PPTR_TOP, seL4_HugePageBits));

    /* place the PUD into the PGD */
    armKSGlobalKernelPGD[GET_KPT_INDEX(PPTR_BASE, KLVL_FRM_ARM_PT_LVL(0))] = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                                                                 1, 1, 0, /* Enable capability loads/stores for the kernel */
#endif
                                                                                 addrFromKPPtr(armKSGlobalKernelPUD));

    /* place all PDs except the last one in PUD */
    for (idx = GET_KPT_INDEX(PPTR_BASE, KLVL_FRM_ARM_PT_LVL(1)); idx < GET_KPT_INDEX(PPTR_TOP, KLVL_FRM_ARM_PT_LVL(1));
         idx++) {
        armKSGlobalKernelPUD[idx] = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                        1, 1, 0, /* Enable capability loads/stores for the kernel */
#endif
                                        addrFromKPPtr(&armKSGlobalKernelPDs[idx][0])
                                    );
    }

    /* map the kernel window using large pages */
    vaddr = PPTR_BASE;
    for (paddr = PADDR_BASE; paddr < PADDR_TOP; paddr += BIT(seL4_LargePageBits)) {
        armKSGlobalKernelPDs[GET_KPT_INDEX(vaddr, KLVL_FRM_ARM_PT_LVL(1))][GET_KPT_INDEX(vaddr,
                                                                                         KLVL_FRM_ARM_PT_LVL(2))] = pte_pte_page_new(
#if defined(CONFIG_HAVE_CHERI)
                                                                                                                        1, 1, 0, /* Enable capability loads/stores for the kernel */
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                                                                                                                        0, // XN
#else
                                                                                                                        1, // UXN
#endif
                                                                                                                        paddr,
                                                                                                                        0,                        /* global */
                                                                                                                        1,                        /* access flag */
                                                                                                                        SMP_TERNARY(SMP_SHARE, 0),        /* Inner-shareable if SMP enabled, otherwise unshared */
                                                                                                                        0,                        /* VMKernelOnly */
                                                                                                                        NORMAL
                                                                                                                    );
        vaddr += BIT(seL4_LargePageBits);
    }

    /* put the PD into the PUD for device window */
    armKSGlobalKernelPUD[GET_KPT_INDEX(PPTR_TOP, KLVL_FRM_ARM_PT_LVL(1))] = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                                                                1, 1, 0, /* Enable capability loads/stores for the kernel */
#endif
                                                                                addrFromKPPtr(&armKSGlobalKernelPDs[BIT(PT_INDEX_BITS) - 1][0])
                                                                            );

    /* put the PT into the PD for device window */
    armKSGlobalKernelPDs[BIT(PT_INDEX_BITS) - 1][BIT(PT_INDEX_BITS) - 1] = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                                                               1, 1, 0, /* Enable capability loads/stores for the kernel */
#endif
                                                                               addrFromKPPtr(armKSGlobalKernelPT)
                                                                           );

    map_kernel_devices();
}

/* When the hypervisor support is enabled, the stage-2 translation table format
 * is used for applications.
 * The global bit is always 0.
 * The memory attributes use the S2 translation values.
 */
static BOOT_CODE void map_it_frame_cap(cap_t vspace_cap, cap_t frame_cap, bool_t executable)
{
    vspace_root_t *vspaceRoot = VSPACE_PTR(pptr_of_cap(vspace_cap));
    pte_t *pud;
    pte_t *pd;
    pte_t *pt;

    vptr_t vptr = cap_frame_cap_get_capFMappedAddress(frame_cap);
    void *pptr = (void *)cap_get_archCapPtr(frame_cap);

    assert(cap_frame_cap_get_capFMappedASID(frame_cap) != 0);

#ifdef AARCH64_VSPACE_S2_START_L1
    pud = vspaceRoot;
#else
    vspaceRoot += GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(0));
    assert(pte_pte_table_ptr_get_present(vspaceRoot));
    pud = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(vspaceRoot)));
#endif
    pud += GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(1));
    assert(pte_pte_table_ptr_get_present(pud));
    pd = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(pud)));
    pd += GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(2));
    assert(pte_pte_table_ptr_get_present(pd));
    pt = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(pd)));
    *(pt + GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(3))) = pte_pte_4k_page_new(
#if defined(CONFIG_HAVE_CHERI)
                                                              1, 1, 0, /* Enable capability loads/stores for the root task */
#endif
                                                              !executable,                    /* unprivileged execute never */
                                                              pptr_to_paddr(pptr),            /* page_base_address    */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                                                              0,
#else
                                                              1,                              /* not global */
#endif
                                                              1,                              /* access flag */
                                                              SMP_TERNARY(SMP_SHARE, 0),              /* Inner-shareable if SMP enabled, otherwise unshared */
                                                              APFromVMRights(VMReadWrite),
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                                                              S2_NORMAL
#else
                                                              NORMAL
#endif
                                                          );
}

static BOOT_CODE cap_t create_it_frame_cap(pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large)
{
    vm_page_size_t frame_size;
    if (use_large) {
        frame_size = ARMLargePage;
    } else {
        frame_size = ARMSmallPage;
    }
    return
        cap_frame_cap_new(
            asid,                          /* capFMappedASID */
            pptr,                          /* capFBasePtr */
            frame_size,                    /* capFSize */
            vptr,                          /* capFMappedAddress */
            wordFromVMRights(VMReadWrite), /* capFVMRights */
            false                          /* capFIsDevice */
        );
}

static BOOT_CODE void map_it_pt_cap(cap_t vspace_cap, cap_t pt_cap)
{
    vspace_root_t *vspaceRoot = VSPACE_PTR(pptr_of_cap(vspace_cap));
    pte_t *pud;
    pte_t *pd;
    pte_t *pt = PT_PTR(cap_page_table_cap_get_capPTBasePtr(pt_cap));
    vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(pt_cap);

    assert(cap_page_table_cap_get_capPTIsMapped(pt_cap));

#ifdef AARCH64_VSPACE_S2_START_L1
    pud = vspaceRoot;
#else
    vspaceRoot += GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(0));
    assert(pte_pte_table_ptr_get_present(vspaceRoot));
    pud = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(vspaceRoot)));
#endif
    pud += GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(1));
    assert(pte_pte_table_ptr_get_present(pud));
    pd = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(pud)));
    *(pd + GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(2))) = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                                              1, 1, 0, /* Enable capability loads/stores for the root task */
#endif
                                                              pptr_to_paddr(pt)
                                                          );
}

static BOOT_CODE cap_t create_it_pt_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              asid,                   /* capPTMappedASID */
              pptr,                   /* capPTBasePtr */
              1,                      /* capPTIsMapped */
              vptr                    /* capPTMappedAddress */
          );
    map_it_pt_cap(vspace_cap, cap);
    return cap;
}

static BOOT_CODE void map_it_pd_cap(cap_t vspace_cap, cap_t pd_cap)
{
    vspace_root_t *vspaceRoot = VSPACE_PTR(pptr_of_cap(vspace_cap));
    pte_t *pud;
    pte_t *pd = PT_PTR(cap_page_table_cap_get_capPTBasePtr(pd_cap));
    vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(pd_cap);

    assert(cap_page_table_cap_get_capPTIsMapped(pd_cap));

#ifdef AARCH64_VSPACE_S2_START_L1
    pud = vspaceRoot;
#else
    vspaceRoot += GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(0));
    assert(pte_pte_table_ptr_get_present(vspaceRoot));
    pud = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(vspaceRoot)));
#endif
    *(pud + GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(1))) = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                                               1, 1, 0, /* Enable capability loads/stores for the root task */
#endif
                                                               pptr_to_paddr(pd)
                                                           );
}

static BOOT_CODE cap_t create_it_pd_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              asid,                   /* capPTMappedASID */
              pptr,                   /* capPTBasePtr */
              1,                      /* capPTIsMapped */
              vptr                    /* capPTMappedAddress */
          );
    map_it_pd_cap(vspace_cap, cap);
    return cap;
}

#ifndef AARCH64_VSPACE_S2_START_L1
static BOOT_CODE void map_it_pud_cap(cap_t vspace_cap, cap_t pud_cap)
{
    pte_t *pgd = PT_PTR(pptr_of_cap(vspace_cap));
    pte_t *pud = PT_PTR(cap_page_table_cap_get_capPTBasePtr(pud_cap));
    vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(pud_cap);

    assert(cap_page_table_cap_get_capPTIsMapped(pud_cap));

    *(pgd + GET_UPT_INDEX(vptr, ULVL_FRM_ARM_PT_LVL(0))) = pte_pte_table_new(
#if defined(CONFIG_HAVE_CHERI)
                                                               1, 1, 0, /* Enable capability loads/stores */
#endif
                                                               pptr_to_paddr(pud));
}

static BOOT_CODE cap_t create_it_pud_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              asid,               /* capPTMappedASID */
              pptr,               /* capPTBasePtr */
              1,                  /* capPTIsMapped */
              vptr                /* capPTMappedAddress */
          );
    map_it_pud_cap(vspace_cap, cap);
    return cap;
}
#endif /* AARCH64_VSPACE_S2_START_L1 */
BOOT_CODE word_t arch_get_n_paging(v_region_t it_v_reg)
{
    return
#ifndef AARCH64_VSPACE_S2_START_L1
        get_n_paging(it_v_reg, GET_ULVL_PGSIZE_BITS(ULVL_FRM_ARM_PT_LVL(0))) +
#endif
        get_n_paging(it_v_reg, GET_ULVL_PGSIZE_BITS(ULVL_FRM_ARM_PT_LVL(1))) +
        get_n_paging(it_v_reg, GET_ULVL_PGSIZE_BITS(ULVL_FRM_ARM_PT_LVL(2)));
}

BOOT_CODE cap_t create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg)
{
    cap_t      vspace_cap;
    vptr_t     vptr;
    seL4_SlotPos slot_pos_before;
    seL4_SlotPos slot_pos_after;

    /* create the PGD */
    vspace_cap = cap_vspace_cap_new(
                     IT_ASID,           /* capVSMappedASID */
                     rootserver.vspace, /* capVSBasePtr    */
                     1                  /* capVSIsMapped   */
#ifdef CONFIG_ARM_SMMU
                     , 0                /* capVSMappedCB   */
#endif
                 );
    slot_pos_before = ndks_boot.slot_pos_cur;
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace), vspace_cap);

#ifndef AARCH64_VSPACE_S2_START_L1
    /* Create any PUDs needed for the user land image */
    for (vptr = ROUND_DOWN(it_v_reg.start, GET_ULVL_PGSIZE_BITS(ULVL_FRM_ARM_PT_LVL(0)));
         vptr < it_v_reg.end;
         vptr += GET_ULVL_PGSIZE(ULVL_FRM_ARM_PT_LVL(0))) {
        if (!provide_cap(root_cnode_cap, create_it_pud_cap(vspace_cap, it_alloc_paging(), vptr, IT_ASID))) {
            return cap_null_cap_new();
        }
    }
#endif
    /* Create any PDs needed for the user land image */
    for (vptr = ROUND_DOWN(it_v_reg.start, GET_ULVL_PGSIZE_BITS(ULVL_FRM_ARM_PT_LVL(1)));
         vptr < it_v_reg.end;
         vptr += GET_ULVL_PGSIZE(ULVL_FRM_ARM_PT_LVL(1))) {
        if (!provide_cap(root_cnode_cap, create_it_pd_cap(vspace_cap, it_alloc_paging(), vptr, IT_ASID))) {
            return cap_null_cap_new();
        }
    }

    /* Create any PTs needed for the user land image */
    for (vptr = ROUND_DOWN(it_v_reg.start, GET_ULVL_PGSIZE_BITS(ULVL_FRM_ARM_PT_LVL(2)));
         vptr < it_v_reg.end;
         vptr += GET_ULVL_PGSIZE(ULVL_FRM_ARM_PT_LVL(2))) {
        if (!provide_cap(root_cnode_cap, create_it_pt_cap(vspace_cap, it_alloc_paging(), vptr, IT_ASID))) {
            return cap_null_cap_new();
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->userImagePaging = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };
    return vspace_cap;
}

BOOT_CODE cap_t create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large)
{
    return create_it_frame_cap(pptr, 0, asidInvalid, use_large);
}

BOOT_CODE cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large,
                                           bool_t executable)
{
    cap_t cap = create_it_frame_cap(pptr, vptr, asid, use_large);
    map_it_frame_cap(pd_cap, cap, executable);
    return cap;
}

BOOT_CODE void activate_kernel_vspace(void)
{
    cleanInvalidateL1Caches();
    setCurrentKernelVSpaceRoot(ttbr_new(0, addrFromKPPtr(armKSGlobalKernelPGD)));

    /* Prevent elf-loader address translation to fill up TLB */
    setCurrentUserVSpaceRoot(ttbr_new(0, addrFromKPPtr(armKSGlobalUserVSpace)));

    invalidateLocalTLB();
    lockTLBEntry(KERNEL_ELF_BASE);
}

BOOT_CODE void write_it_asid_pool(cap_t it_ap_cap, cap_t it_vspace_cap)
{
    asid_pool_t *ap = ASID_POOL_PTR(pptr_of_cap(it_ap_cap));
    asid_map_t asid_map = asid_map_asid_map_vspace_new(
#ifdef CONFIG_ARM_SMMU
                              /* bind_cb: Number of bound context banks */
                              0,
#endif
                              /* vspace_root: reference to vspace root page table object */
                              (pptr_t)cap_vspace_cap_get_capVSBasePtr(it_vspace_cap)
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                              /* stored_hw_vmid, stored_vmid_valid: Assigned hardware VMID for TLB. */
                              , 0, false
#endif
                          );
    ap->array[IT_ASID] = asid_map;
    armKSASIDTable[IT_ASID >> asidLowBits] = ap;
}

/* ==================== BOOT CODE FINISHES HERE ==================== */

asid_map_t findMapForASID(asid_t asid)
{
    asid_pool_t *poolPtr;

    poolPtr = armKSASIDTable[asid >> asidLowBits];
    if (!poolPtr) {
        return asid_map_asid_map_none_new();
    }

    return poolPtr->array[asid & MASK(asidLowBits)];
}

static findVSpaceForASID_ret_t findVSpaceForASID(asid_t asid)
{
    findVSpaceForASID_ret_t ret;
    asid_map_t asid_map;

    asid_map = findMapForASID(asid);
    if (asid_map_get_type(asid_map) != asid_map_asid_map_vspace) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.vspace_root = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ret.vspace_root = VSPACE_PTR(asid_map_asid_map_vspace_get_vspace_root(asid_map));
    ret.status = EXCEPTION_NONE;
    return ret;
}

register_t *PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread)
{
    word_t w_bufferPtr;
    cap_t bufferCap;
    vm_rights_t vm_rights;

    w_bufferPtr = thread->tcbIPCBuffer;
    bufferCap = TCB_PTR_CTE_PTR(thread, tcbBuffer)->cap;

    if (unlikely(cap_get_capType(bufferCap) != cap_frame_cap)) {
        return NULL;
    }
    if (unlikely(cap_frame_cap_get_capFIsDevice(bufferCap))) {
        return NULL;
    }

    vm_rights = cap_frame_cap_get_capFVMRights(bufferCap);
    if (likely(vm_rights == VMReadWrite ||
               (!isReceiver && vm_rights == VMReadOnly))) {
        pptr_t basePtr;
        unsigned int pageBits;

        basePtr = (pptr_t)cap_get_archCapPtr(bufferCap);
        pageBits = pageBitsForSize(cap_frame_cap_get_capFSize(bufferCap));
        return (register_t *)(basePtr + (w_bufferPtr & MASK(pageBits)));
    } else {
        return NULL;
    }
}

exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap)
{
    if (cap_get_capType(cap) != cap_frame_cap) {
        userError("IPC Buffer is an invalid cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_frame_cap_get_capFIsDevice(cap))) {
        userError("Specifying a device frame as an IPC buffer is not permitted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (!IS_ALIGNED(vptr, seL4_IPCBufferSizeBits)) {
        userError("IPC Buffer vaddr 0x%x is not aligned.", (int)vptr);
        current_syscall_error.type = seL4_AlignmentError;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

static lookupPTSlot_ret_t lookupPTSlot(vspace_root_t *vspace, vptr_t vptr)
{
    lookupPTSlot_ret_t ret;

    word_t level = UPT_LEVELS - 1;
    pte_t *pt = vspace;

    /* this is how many bits we potentially have left to decode. Initially we have the
     * full address space to decode, and every time we walk this will be reduced. The
     * final value of this after the walk is the size of the frame that can be inserted,
     * or already exists, in ret.ptSlot. The following formulation is an invariant of
     * the loop: */
    ret.ptBitsLeft = PT_INDEX_BITS * level + seL4_PageBits;
    ret.ptSlot = pt + ((vptr >> ret.ptBitsLeft) & MASK(seL4_VSpaceIndexBits));

    while (pte_pte_table_ptr_get_present(ret.ptSlot) && likely(level > 0)) {
        level--;
        ret.ptBitsLeft -= PT_INDEX_BITS;
        pt = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(ret.ptSlot)));
        ret.ptSlot = pt + ((vptr >> ret.ptBitsLeft) & MASK(PT_INDEX_BITS));
    }

    return ret;
}

/* Note that if the hypervisor support is enabled, the user page tables use
 * stage-2 translation format. Otherwise, they follow the stage-1 translation format.
 */
static pte_t makeUserPagePTE(paddr_t paddr, vm_rights_t vm_rights, vm_attributes_t attributes, vm_page_size_t page_size)
{
    bool_t nonexecutable = vm_attributes_get_armExecuteNever(attributes);
    word_t cacheable = vm_attributes_get_armPageCacheable(attributes);

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    word_t nG = 0; /* not global */
    word_t attridx = cacheable ? S2_NORMAL : S2_DEVICE_nGnRnE;
#else
    word_t nG = 1; /* not global */
    word_t attridx = cacheable ? NORMAL : DEVICE_nGnRnE;
#endif

    /* Inner-shareable if SMP enabled, otherwise unshared (ignored for devices) */
    word_t shareable = cacheable ? SMP_TERNARY(SMP_SHARE, 0) : 0;

    if (page_size == ARMSmallPage) {
        return pte_pte_4k_page_new(
#if defined(CONFIG_HAVE_CHERI)
                                   /* cheriTODO: fine-grain capability permissions per-user */
                                   1, 1, 0, /* Enable capability loads/stores */
#endif
                                   nonexecutable, paddr, nG, 1 /* access flag */,
                                   shareable, APFromVMRights(vm_rights), attridx);
    } else {
        return pte_pte_page_new(
#if defined(CONFIG_HAVE_CHERI)
                                /* cheriTODO: fine-grain capability permissions per-user */
                                1, 1, 0, /* Enable capability loads/stores */
#endif
                                nonexecutable, paddr, nG, 1 /* access flag */,
                                shareable, APFromVMRights(vm_rights), attridx);
    }
}

exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType)
{
    switch (vm_faultType) {
    case ARMDataAbort: {
        word_t addr, fault;

        addr = getFAR();
        fault = getDFSR();

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        /* use the IPA */
        if (ARCH_NODE_STATE(armHSVCPUActive)) {
            addr = GET_PAR_ADDR(addressTranslateS1(addr)) | (addr & MASK(PAGE_BITS));
        }
#endif
        current_fault = seL4_Fault_VMFault_new(addr, fault, false);
        return EXCEPTION_FAULT;
    }

    case ARMPrefetchAbort: {
        word_t pc, fault;

        pc = getRestartPC(thread);
        fault = getIFSR();

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (ARCH_NODE_STATE(armHSVCPUActive)) {
            pc = GET_PAR_ADDR(addressTranslateS1(pc)) | (pc & MASK(PAGE_BITS));
        }
#endif
        current_fault = seL4_Fault_VMFault_new(pc, fault, true);
        return EXCEPTION_FAULT;
    }

    default:
        fail("Invalid VM fault type");
    }
}

bool_t CONST isVTableRoot(cap_t cap)
{
    return cap_get_capType(cap) == cap_vspace_cap;
}

bool_t CONST isValidNativeRoot(cap_t cap)
{
    return isVTableRoot(cap) &&
           cap_vspace_cap_get_capVSIsMapped(cap);
}

bool_t CONST isValidVTableRoot(cap_t cap)
{
    return isValidNativeRoot(cap);
}

void setVMRoot(tcb_t *tcb)
{
    cap_t threadRoot;
    asid_t asid;
    vspace_root_t *vspaceRoot;
    findVSpaceForASID_ret_t find_ret;

    threadRoot = TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap;

    if (!isValidNativeRoot(threadRoot)) {
        setCurrentUserVSpaceRoot(ttbr_new(0, addrFromKPPtr(armKSGlobalUserVSpace)));
        return;
    }

    vspaceRoot = VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(threadRoot));
    asid = cap_vspace_cap_get_capVSMappedASID(threadRoot);
    find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE || find_ret.vspace_root != vspaceRoot)) {
        setCurrentUserVSpaceRoot(ttbr_new(0, addrFromKPPtr(armKSGlobalUserVSpace)));
        return;
    }

    armv_contextSwitch(vspaceRoot, asid);
}

static bool_t setVMRootForFlush(vspace_root_t *vspace, asid_t asid)
{
    cap_t threadRoot;

    threadRoot = TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbVTable)->cap;

    if (cap_get_capType(threadRoot) == cap_vspace_cap &&
        cap_vspace_cap_get_capVSIsMapped(threadRoot) &&
        VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(threadRoot)) == vspace) {
        return false;
    }

    armv_contextSwitch(vspace, asid);
    return true;
}


#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

static inline asid_pool_t *getPoolPtr(asid_t asid)
{
    return armKSASIDTable[asid >> asidLowBits];
}

static inline asid_map_t getASIDMap(asid_pool_t *poolPtr, asid_t asid)
{
    assert(poolPtr != NULL);
    return poolPtr->array[asid & MASK(asidLowBits)];
}

static inline void setASIDMap(asid_pool_t *poolPtr, asid_t asid, asid_map_t asid_map)
{
    assert(poolPtr != NULL);
    poolPtr->array[asid & MASK(asidLowBits)] = asid_map;
}

static void invalidateASID(asid_t asid)
{
    asid_pool_t *poolPtr;
    asid_map_t asid_map;

    poolPtr = getPoolPtr(asid);
    asid_map = getASIDMap(poolPtr, asid);
    assert(asid_map_get_type(asid_map) == asid_map_asid_map_vspace);

    asid_map = asid_map_asid_map_vspace_set_stored_hw_vmid(asid_map, 0);
    asid_map = asid_map_asid_map_vspace_set_stored_vmid_valid(asid_map, false);

    setASIDMap(poolPtr, asid, asid_map);
}

static void storeHWASID(asid_t asid, hw_asid_t hw_asid)
{
    asid_pool_t *poolPtr;
    asid_map_t asid_map;

    poolPtr = getPoolPtr(asid);
    asid_map = getASIDMap(poolPtr, asid);
    assert(asid_map_get_type(asid_map) == asid_map_asid_map_vspace);

    asid_map = asid_map_asid_map_vspace_set_stored_hw_vmid(asid_map, hw_asid);
    asid_map = asid_map_asid_map_vspace_set_stored_vmid_valid(asid_map, true);

    setASIDMap(poolPtr, asid, asid_map);
    armKSHWASIDTable[hw_asid] = asid;
}

static hw_asid_t findFreeHWASID(void)
{
    word_t hw_asid_offset;
    hw_asid_t hw_asid;

    /* Find a free hardware ASID */
    for (hw_asid_offset = 0;
         hw_asid_offset <= (word_t)((hw_asid_t) - 1);
         hw_asid_offset++) {
        hw_asid = armKSNextASID + ((hw_asid_t)hw_asid_offset);
        if (armKSHWASIDTable[hw_asid] == asidInvalid) {
            return hw_asid;
        }
    }

    hw_asid = armKSNextASID;

    /* If we've scanned the table without finding a free ASID */
    invalidateASID(armKSHWASIDTable[hw_asid]);

    /* Flush TLB */
    invalidateTranslationASID(hw_asid);
    armKSHWASIDTable[hw_asid] = asidInvalid;

    /* Increment the NextASID index */
    armKSNextASID++;

    return hw_asid;
}

hw_asid_t getHWASID(asid_t asid)
{
    asid_map_t asid_map;

    asid_map = findMapForASID(asid);
    if (asid_map_asid_map_vspace_get_stored_vmid_valid(asid_map)) {
        return asid_map_asid_map_vspace_get_stored_hw_vmid(asid_map);
    } else {
        hw_asid_t new_hw_asid;

        new_hw_asid = findFreeHWASID();
        storeHWASID(asid, new_hw_asid);
        return new_hw_asid;
    }
}

static void invalidateASIDEntry(asid_t asid)
{
    asid_map_t asid_map;

    asid_map = findMapForASID(asid);
    if (asid_map_asid_map_vspace_get_stored_vmid_valid(asid_map)) {
        armKSHWASIDTable[asid_map_asid_map_vspace_get_stored_hw_vmid(asid_map)] =
            asidInvalid;
    }
    invalidateASID(asid);
}

#endif

#ifdef CONFIG_ARM_SMMU
static word_t getASIDBindCB(asid_t asid)
{
    asid_pool_t *asidPool;

    asidPool = armKSASIDTable[asid >> asidLowBits];
    assert(asidPool);

    asid_map_t asid_map = asidPool->array[asid & MASK(asidLowBits)];
    assert(asid_map_get_type(asid_map) == asid_map_asid_map_vspace);

    return asid_map_asid_map_vspace_get_bind_cb(asid_map);
}

void increaseASIDBindCB(asid_t asid)
{
    asid_pool_t *asidPool;

    asidPool = armKSASIDTable[asid >> asidLowBits];
    assert(asidPool);

    asid_map_t *asid_map = &asidPool->array[asid & MASK(asidLowBits)];
    assert(asid_map_ptr_get_type(asid_map) == asid_map_asid_map_vspace);

    asid_map_asid_map_vspace_ptr_set_bind_cb(asid_map, asid_map_asid_map_vspace_ptr_get_bind_cb(asid_map) + 1);
}

void decreaseASIDBindCB(asid_t asid)
{
    asid_pool_t *asidPool;

    asidPool = armKSASIDTable[asid >> asidLowBits];
    assert(asidPool);

    asid_map_t *asid_map = &asidPool->array[asid & MASK(asidLowBits)];
    assert(asid_map_ptr_get_type(asid_map) == asid_map_asid_map_vspace);

    asid_map_asid_map_vspace_ptr_set_bind_cb(asid_map, asid_map_asid_map_vspace_ptr_get_bind_cb(asid_map) - 1);
}
#endif

static inline void invalidateTLBByASID(asid_t asid)
{
#ifdef CONFIG_ARM_SMMU
    word_t bind_cb = getASIDBindCB(asid);
    if (unlikely(bind_cb)) {
        invalidateSMMUTLBByASID(asid, bind_cb);
    }
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    asid_map_t asid_map;

    asid_map = findMapForASID(asid);
    if (!asid_map_asid_map_vspace_get_stored_vmid_valid(asid_map)) {
        return;
    }
    invalidateTranslationASID(asid_map_asid_map_vspace_get_stored_hw_vmid(asid_map));
#else
    invalidateTranslationASID(asid);
#endif
}

static inline void invalidateTLBByASIDVA(asid_t asid, vptr_t vaddr)
{
#ifdef CONFIG_ARM_SMMU
    word_t bind_cb = getASIDBindCB(asid);
    if (unlikely(bind_cb)) {
        invalidateSMMUTLBByASIDVA(asid, vaddr, bind_cb);
    }
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    asid_map_t asid_map;

    asid_map = findMapForASID(asid);
    if (!asid_map_asid_map_vspace_get_stored_vmid_valid(asid_map)) {
        return;
    }
    uint64_t hw_asid = asid_map_asid_map_vspace_get_stored_hw_vmid(asid_map);
    invalidateTranslationSingle((hw_asid << 48) | vaddr >> seL4_PageBits);
#else
    invalidateTranslationSingle((asid << 48) | vaddr >> seL4_PageBits);
#endif
}


void unmapPageTable(asid_t asid, vptr_t vptr, pte_t *target_pt)
{
    findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE)) {
        /* nothing to do */
        return;
    }
    pte_t *ptSlot = NULL;
    pte_t *pt = (pte_t *)find_ret.vspace_root;

    for (word_t i = 0; i < UPT_LEVELS - 1 && pt != target_pt; i++) {
        ptSlot = pt + GET_UPT_INDEX(vptr, i);
        if (unlikely(!pte_pte_table_ptr_get_present(ptSlot))) {
            /* couldn't find it */
            return;
        }
        pt = PT_PTR(paddr_to_pptr(pte_pte_table_ptr_get_pt_base_address(ptSlot)));
    }

    if (pt != target_pt) {
        /* didn't find it */
        return;
    }
    /* If we found a pt then ptSlot won't be null */
    assert(ptSlot != NULL);
    *ptSlot = pte_pte_invalid_new();
    cleanByVA_PoU((vptr_t)ptSlot, pptr_to_paddr(ptSlot));
    invalidateTLBByASID(asid);
}

void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, pptr_t pptr)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPTSlot_ret_t  lu_ret;
    pte_t pte;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    lu_ret = lookupPTSlot(find_ret.vspace_root, vptr);
    if (unlikely(lu_ret.ptBitsLeft != pageBitsForSize(page_size))) {
        /* Do nothing if the wrong size object was returned */
        return;
    }

    pte = *(lu_ret.ptSlot);
    if (!pte_is_page_type(pte)) {
        /* Do nothing if no page is present */
        return;
    }

    if (pte_get_page_base_address(pte) != pptr_to_paddr((void *)pptr)) {
        /* Do nothing if the mapped page is not the same physical frame */
        return;
    }

    *(lu_ret.ptSlot) = pte_pte_invalid_new();
    cleanByVA_PoU((vptr_t)lu_ret.ptSlot, pptr_to_paddr(lu_ret.ptSlot));
    assert(asid < BIT(16));
    invalidateTLBByASIDVA(asid, vptr);
}

void deleteASID(asid_t asid, vspace_root_t *vspace)
{
    asid_pool_t *poolPtr;

    poolPtr = armKSASIDTable[asid >> asidLowBits];

    if (poolPtr != NULL) {
        asid_map_t asid_map = poolPtr->array[asid & MASK(asidLowBits)];
        if (asid_map_get_type(asid_map) == asid_map_asid_map_vspace &&
            (vspace_root_t *)asid_map_asid_map_vspace_get_vspace_root(asid_map) == vspace) {
            invalidateTLBByASID(asid);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            invalidateASIDEntry(asid);
#endif
            poolPtr->array[asid & MASK(asidLowBits)] = asid_map_asid_map_none_new();
            setVMRoot(NODE_STATE(ksCurThread));
        }
    }
}

void deleteASIDPool(asid_t asid_base, asid_pool_t *pool)
{
    word_t offset;

    assert((asid_base & MASK(asidLowBits)) == 0);

    if (armKSASIDTable[asid_base >> asidLowBits] == pool) {
        for (offset = 0; offset < BIT(asidLowBits); offset++) {
            asid_map_t asid_map = pool->array[offset];
            if (asid_map_get_type(asid_map) == asid_map_asid_map_vspace) {
                invalidateTLBByASID(asid_base + offset);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                invalidateASIDEntry(asid_base + offset);
#endif
            }
        }
        armKSASIDTable[asid_base >> asidLowBits] = NULL;
        setVMRoot(NODE_STATE(ksCurThread));
    }
}

static void doFlush(word_t invLabel, vptr_t start, vptr_t end, paddr_t pstart)
{
    switch (invLabel) {
    case ARMVSpaceClean_Data:
    case ARMPageClean_Data:
        cleanCacheRange_RAM(start, end, pstart);
        break;

    case ARMVSpaceInvalidate_Data:
    case ARMPageInvalidate_Data:
        invalidateCacheRange_RAM(start, end, pstart);
        break;

    case ARMVSpaceCleanInvalidate_Data:
    case ARMPageCleanInvalidate_Data:
        cleanInvalidateCacheRange_RAM(start, end, pstart);
        break;

    case ARMVSpaceUnify_Instruction:
    case ARMPageUnify_Instruction:
        /* First clean data lines to point of unification... */
        cleanCacheRange_PoU(start, end, pstart);
        /* Ensure it's been written. */
        dsb();
        /* ...then invalidate the corresponding instruction lines
           to point of unification... */
        invalidateCacheRange_I(start, end, pstart);
        /* ... and ensure new instructions come from fresh cache lines. */
        isb();
        break;
    default:
        fail("Invalid operation, shouldn't get here.\n");
    }
}

/* ================= INVOCATION HANDLING STARTS HERE ================== */

static exception_t performVSpaceFlush(word_t invLabel, vspace_root_t *vspaceRoot, asid_t asid,
                                      vptr_t start, vptr_t end, paddr_t pstart)
{

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        word_t size = end - start;
        start = (vptr_t)paddr_to_pptr(pstart);
        end = start + size;
        if (start < end) {
            doFlush(invLabel, start, end, pstart);
        }
    } else {
        bool_t root_switched;

        /* Flush if given a non zero range */
        if (start < end) {
            root_switched = setVMRootForFlush(vspaceRoot, asid);
            doFlush(invLabel, start, end, pstart);
            if (root_switched) {
                setVMRoot(NODE_STATE(ksCurThread));
            }
        }
    }
    return EXCEPTION_NONE;
}


static exception_t performPageTableInvocationMap(cap_t cap, cte_t *ctSlot, pte_t pte, pte_t *ptSlot)
{
    ctSlot->cap = cap;
    *ptSlot = pte;
    cleanByVA_PoU((vptr_t)ptSlot, pptr_to_paddr(ptSlot));

    return EXCEPTION_NONE;
}

static exception_t performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        pte_t *pt = PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap));
        unmapPageTable(cap_page_table_cap_get_capPTMappedASID(cap),
                       cap_page_table_cap_get_capPTMappedAddress(cap), pt);
        clearMemory_PT((void *)pt, cap_get_capSizeBits(cap));
    }

    cap_page_table_cap_ptr_set_capPTIsMapped(&(ctSlot->cap), 0);
    return EXCEPTION_NONE;
}

static exception_t performPageInvocationMap(asid_t asid, cap_t cap, cte_t *ctSlot,
                                            pte_t pte, pte_t *ptSlot)
{
    bool_t tlbflush_required = pte_ptr_get_valid(ptSlot);

    ctSlot->cap = cap;
    *ptSlot = pte;

    cleanByVA_PoU((vptr_t)ptSlot, pptr_to_paddr(ptSlot));
    if (unlikely(tlbflush_required)) {
        assert(asid < BIT(16));
        invalidateTLBByASIDVA(asid, cap_frame_cap_get_capFMappedAddress(cap));
    }

    return EXCEPTION_NONE;
}

static exception_t performPageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_frame_cap_get_capFMappedASID(cap) != 0) {

        unmapPage(cap_frame_cap_get_capFSize(cap),
                  cap_frame_cap_get_capFMappedASID(cap),
                  cap_frame_cap_get_capFMappedAddress(cap),
                  cap_frame_cap_get_capFBasePtr(cap));
    }

    cap_t slotCap = ctSlot->cap;
    slotCap = cap_frame_cap_set_capFMappedAddress(slotCap, 0);
    slotCap = cap_frame_cap_set_capFMappedASID(slotCap, asidInvalid);
    ctSlot->cap = slotCap;


    return EXCEPTION_NONE;
}

static exception_t performPageFlush(word_t invLabel, vspace_root_t *vspaceRoot, asid_t asid,
                                    vptr_t start, vptr_t end, paddr_t pstart)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* We flush the cache with kernel virtual addresses since
         * the user virtual addresses are not valid in EL2.
         * Switching VMRoot is not required.
         */
        word_t size = end - start;
        start = (vptr_t)paddr_to_pptr(pstart);
        end = start + size;

        if (start < end) {
            doFlush(invLabel, start, end, pstart);
        }
    } else {
        bool_t root_switched;

        if (start < end) {
            root_switched = setVMRootForFlush(vspaceRoot, asid);
            doFlush(invLabel, start, end, pstart);
            if (root_switched) {
                setVMRoot(NODE_STATE(ksCurThread));
            }
        }
    }
    return EXCEPTION_NONE;
}

static exception_t performPageGetAddress(pptr_t base_ptr, bool_t call)
{
    paddr_t base = pptr_to_paddr((void *)base_ptr);

    tcb_t *thread;
    thread = NODE_STATE(ksCurThread);
    if (call) {
        register_t *ipcBuffer = lookupIPCBuffer(true, thread);
        setRegister(thread, badgeRegister, 0);
        unsigned int length = setMR(thread, ipcBuffer, 0, base);
        setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, length)));
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Running);
    return EXCEPTION_NONE;
}

static exception_t performASIDControlInvocation(void *frame, cte_t *slot,
                                                cte_t *parent, asid_t asid_base)
{
    /** AUXUPD: "(True, typ_region_bytes (ptr_val \<acute>frame) 12)" */
    /** GHOSTUPD: "(True, gs_clear_region (ptr_val \<acute>frame) 12)" */
    cap_untyped_cap_ptr_set_capFreeIndex(&(parent->cap),
                                         MAX_FREE_INDEX(cap_untyped_cap_get_capBlockSize(parent->cap)));

    memzero(frame, BIT(seL4_ASIDPoolBits));
    /** AUXUPD: "(True, ptr_retyps 1 (Ptr (ptr_val \<acute>frame) :: asid_pool_C ptr))" */

    cteInsert(
        cap_asid_pool_cap_new(
            asid_base,      /* capASIDBase  */
            (pptr_t)frame   /* capASIDPool  */
        ), parent, slot);

    assert((asid_base & MASK(asidLowBits)) == 0);
    armKSASIDTable[asid_base >> asidLowBits] = (asid_pool_t *)frame;

    return EXCEPTION_NONE;
}

static exception_t decodeARMVSpaceRootInvocation(word_t invLabel, word_t length,
                                                 cte_t *cte, cap_t cap, register_t *buffer)
{
    vptr_t start, end;
    paddr_t pstart;
    asid_t asid;
    vspace_root_t *vspaceRoot;
    lookupPTSlot_ret_t resolve_ret;
    findVSpaceForASID_ret_t find_ret;
    pte_t pte;

    switch (invLabel) {
    case ARMVSpaceClean_Data:
    case ARMVSpaceInvalidate_Data:
    case ARMVSpaceCleanInvalidate_Data:
    case ARMVSpaceUnify_Instruction:

        if (length < 2) {
            userError("VSpaceRoot Flush: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        start = getSyscallArg(0, buffer);
        end =   getSyscallArg(1, buffer);

        /* Check sanity of arguments */
        if (end <= start) {
            userError("VSpaceRoot Flush: Invalid range.");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Don't let applications flush kernel regions. */
        if (end > USER_TOP) {
            userError("VSpaceRoot Flush: Exceed the user addressable region.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(!isValidNativeRoot(cap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Make sure that the supplied pgd is ok */
        vspaceRoot = VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(cap));
        asid = cap_vspace_cap_get_capVSMappedASID(cap);

        find_ret = findVSpaceForASID(asid);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            userError("VSpaceRoot Flush: No VSpace for ASID");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(find_ret.vspace_root != vspaceRoot)) {
            userError("VSpaceRoot Flush: Invalid VSpace Cap");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Look up the frame containing 'start'. */
        resolve_ret = lookupPTSlot(vspaceRoot, start);
        pte = *resolve_ret.ptSlot;

        /* Check that the returned slot is a page. */
        if (!pte_is_page_type(pte)) {

            /* Fail silently, as there can't be any stale cached data (for the
             * given address space), and getting a syscall error because the
             * relevant page is non-resident would be 'astonishing'. */
            setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
            return EXCEPTION_NONE;
        }

        /* Refuse to cross a page boundary. */
        if (ROUND_DOWN(start, resolve_ret.ptBitsLeft) != ROUND_DOWN(end - 1, resolve_ret.ptBitsLeft)) {
            current_syscall_error.type = seL4_RangeError;
            current_syscall_error.rangeErrorMin = start;
            current_syscall_error.rangeErrorMax = ROUND_DOWN(start, resolve_ret.ptBitsLeft) +
                                                  MASK(resolve_ret.ptBitsLeft);
            return EXCEPTION_SYSCALL_ERROR;
        }

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        /* When in EL1, the mapping must be write-able for ARMVSpaceInvalidate_Data */
        if (invLabel == ARMVSpaceInvalidate_Data && vmRightsFromPTE(pte) != VMReadWrite) {
            userError("ARMVSpaceInvalidate_Data: Cannot call on mapping without write rights.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
#endif

        /* Calculate the physical start address. */
        paddr_t frame_base = pte_get_page_base_address(pte);

        pstart = frame_base + (start & MASK(resolve_ret.ptBitsLeft));

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performVSpaceFlush(invLabel, vspaceRoot, asid, start, end - 1, pstart);

    default:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}


static exception_t decodeARMPageTableInvocation(word_t invLabel, word_t length,
                                                cte_t *cte, cap_t cap, register_t *buffer)
{
    cap_t vspaceRootCap;
    vspace_root_t *vspaceRoot;
    pte_t pte;
    asid_t asid;
    vptr_t vaddr;
    lookupPTSlot_ret_t ptSlot;
    findVSpaceForASID_ret_t find_ret;

    if (invLabel == ARMPageTableUnmap) {
        if (unlikely(!isFinalCapability(cte))) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageTableInvocationUnmap(cap, cte);
    }

    if (unlikely(invLabel != ARMPageTableMap)) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(length < 2 || current_extra_caps.excaprefs[0] == NULL)) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_page_table_cap_get_capPTIsMapped(cap))) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    vspaceRootCap = current_extra_caps.excaprefs[0]->cap;

    if (unlikely(!isValidNativeRoot(vspaceRootCap))) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vspaceRoot = VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(vspaceRootCap));
    asid = cap_vspace_cap_get_capVSMappedASID(vspaceRootCap);

    if (unlikely(vaddr > USER_TOP)) {
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE)) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(find_ret.vspace_root != vspaceRoot)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    ptSlot = lookupPTSlot(vspaceRoot, vaddr);

    if (unlikely(ptSlot.ptBitsLeft == seL4_PageBits || pte_ptr_get_valid(ptSlot.ptSlot))) {
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    pte = pte_pte_table_new(
#if defined(__CHERI_PURE_CAPABILITY__)
                            /* cheriTODO: fine-grain capability permissions per-user */
                            1, 1, 0, /* Enable capability loads/stores */
#endif
                            pptr_to_paddr(PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap))));

    cap = cap_page_table_cap_set_capPTIsMapped(cap, 1);
    cap = cap_page_table_cap_set_capPTMappedASID(cap, asid);
    cap = cap_page_table_cap_set_capPTMappedAddress(cap, (vaddr & ~MASK(ptSlot.ptBitsLeft)));

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performPageTableInvocationMap(cap, cte, pte, ptSlot.ptSlot);
}

static inline bool_t CONST checkVPAlignment(vm_page_size_t sz, word_t w)
{
    return (w & MASK(pageBitsForSize(sz))) == 0;
}

static exception_t decodeARMFrameInvocation(word_t invLabel, word_t length,
                                            cte_t *cte, cap_t cap, bool_t call, register_t *buffer)
{
    switch (invLabel) {
    case ARMPageMap: {
        vptr_t vaddr;
        paddr_t base;
        cap_t vspaceRootCap;
        vspace_root_t *vspaceRoot;
        asid_t asid, frame_asid;
        vm_rights_t vmRights;
        vm_page_size_t frameSize;
        vm_attributes_t attributes;
        findVSpaceForASID_ret_t find_ret;

        if (unlikely(length < 3 || current_extra_caps.excaprefs[0] == NULL)) {
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vaddr = getSyscallArg(0, buffer);
        attributes = vmAttributesFromWord(getSyscallArg(2, buffer));
        vspaceRootCap = current_extra_caps.excaprefs[0]->cap;

        frameSize = cap_frame_cap_get_capFSize(cap);
        vmRights = maskVMRights(cap_frame_cap_get_capFVMRights(cap),
                                rightsFromWord(getSyscallArg(1, buffer)));

        if (unlikely(!isValidNativeRoot(vspaceRootCap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vspaceRoot = VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(vspaceRootCap));
        asid = cap_vspace_cap_get_capVSMappedASID(vspaceRootCap);

        find_ret = findVSpaceForASID(asid);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(find_ret.vspace_root != vspaceRoot)) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(!checkVPAlignment(frameSize, vaddr))) {
            current_syscall_error.type = seL4_AlignmentError;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* In the case of remap, the cap should have a valid asid */
        frame_asid = cap_frame_cap_get_capFMappedASID(cap);

        if (frame_asid != asidInvalid) {
            if (frame_asid != asid) {
                userError("ARMPageMap: Attempting to remap a frame that does not belong to the passed address space");
                current_syscall_error.type = seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;
                return EXCEPTION_SYSCALL_ERROR;

            } else if (cap_frame_cap_get_capFMappedAddress(cap) != vaddr) {
                userError("ARMPageMap: Attempting to map frame into multiple addresses");
                current_syscall_error.type = seL4_InvalidArgument;
                current_syscall_error.invalidArgumentNumber = 0;
                return EXCEPTION_SYSCALL_ERROR;
            }
        } else {
            if (unlikely(vaddr + BIT(pageBitsForSize(frameSize)) - 1 > USER_TOP)) {
                current_syscall_error.type = seL4_InvalidArgument;
                current_syscall_error.invalidArgumentNumber = 0;
                return EXCEPTION_SYSCALL_ERROR;
            }
        }

        cap = cap_frame_cap_set_capFMappedASID(cap, asid);
        cap = cap_frame_cap_set_capFMappedAddress(cap, vaddr);

        base = pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap));

        lookupPTSlot_ret_t lu_ret = lookupPTSlot(vspaceRoot, vaddr);
        if (unlikely(lu_ret.ptBitsLeft != pageBitsForSize(frameSize))) {
            current_lookup_fault = lookup_fault_missing_capability_new(lu_ret.ptBitsLeft);
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageInvocationMap(asid, cap, cte,
                                        makeUserPagePTE(base, vmRights, attributes, frameSize), lu_ret.ptSlot);
    }

    case ARMPageUnmap:
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageInvocationUnmap(cap, cte);

    case ARMPageClean_Data:
    case ARMPageInvalidate_Data:
    case ARMPageCleanInvalidate_Data:
    case ARMPageUnify_Instruction: {
        vptr_t start, end;
        vptr_t vaddr;
        asid_t asid;
        word_t page_size;
        findVSpaceForASID_ret_t find_ret;

        if (length < 2) {
            userError("Page Flush: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(cap_frame_cap_get_capFMappedASID(cap) == 0)) {
            userError("Page Flush: Frame is not mapped.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vaddr = cap_frame_cap_get_capFMappedAddress(cap);
        asid = cap_frame_cap_get_capFMappedASID(cap);

        find_ret = findVSpaceForASID(asid);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            userError("Page Flush: No PGD for ASID");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        start = getSyscallArg(0, buffer);
        end =   getSyscallArg(1, buffer);

        /* check that the range is sane */
        if (end <= start) {
            userError("PageFlush: Invalid range");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* start and end are currently relative inside this page */
        page_size = BIT(pageBitsForSize(cap_frame_cap_get_capFSize(cap)));
        if (start >= page_size || end > page_size) {
            userError("Page Flush: Requested range not inside page");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

#if defined(__CHERI_PURE_CAPABILITY__)
        /* FIXME: cheriTODO We need to re-build a CHERI user capability here as we
         * don't embed CHERI capabilities for user mapped addresses within the seL4
         * capability itself (capFMappedAddress). Only the kernel's address map of
         * this page is embedded in the seL4 capability format.
         */
        vaddr = (vptr_t) cheri_build_user_cap((ptraddr_t)vaddr, page_size, -1);
#endif

        word_t pstart = pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap)) + start;
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        /* Don't let applications flush outside of the kernel window */
        if (pstart < PADDR_BASE || ((end - start) + pstart) > PADDR_TOP) {
            userError("Page Flush: Overlaps kernel region.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
#else
        /* When in EL1, we are using the user page table for flushing and need to make sure
           the mapping info in the cap is not stale. */
        lookupPTSlot_ret_t lu_ret = lookupPTSlot(find_ret.vspace_root, vaddr);
        pte_t pte = *lu_ret.ptSlot;
        void *base_ptr = (void *) cap_frame_cap_get_capFBasePtr(cap);
        if (unlikely(lu_ret.ptBitsLeft != pageBitsForSize(cap_frame_cap_get_capFSize(cap)) ||
                     !pte_is_page_type(pte) ||
                     pte_get_page_base_address(pte) != pptr_to_paddr(base_ptr))) {
            userError("Page Flush: Attempting to use cap with stale mapping information.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* When in EL1, the mapping must be writeable for DC IVAC */
        if (invLabel == ARMPageInvalidate_Data && vmRightsFromPTE(pte) != VMReadWrite) {
            userError("ARMPageInvalidate_Data: Cannot call on mapping without write rights.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
#endif
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageFlush(invLabel, find_ret.vspace_root, asid, vaddr + (word_t)start, vaddr + (word_t)end - 1,
                                pstart);
    }

    case ARMPageGetAddress:
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageGetAddress(cap_frame_cap_get_capFBasePtr(cap), call);

    default:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

exception_t decodeARMMMUInvocation(word_t invLabel, word_t length, cptr_t cptr,
                                   cte_t *cte, cap_t cap, bool_t call, register_t *buffer)
{
    switch (cap_get_capType(cap)) {
    case cap_vspace_cap:
        return decodeARMVSpaceRootInvocation(invLabel, length, cte, cap, buffer);
    case cap_page_table_cap:
        return decodeARMPageTableInvocation(invLabel, length, cte, cap, buffer);

    case cap_frame_cap:
        return decodeARMFrameInvocation(invLabel, length, cte, cap, call, buffer);

    case cap_asid_control_cap: {
        word_t i;
        asid_t asid_base;
        word_t index, depth;
        cap_t untyped, root;
        cte_t *parentSlot, *destSlot;
        lookupSlot_ret_t lu_ret;
        void *frame;
        exception_t status;

        if (unlikely(invLabel != ARMASIDControlMakePool)) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(length < 2 ||
                     current_extra_caps.excaprefs[0] == NULL ||
                     current_extra_caps.excaprefs[1] == NULL)) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        index = getSyscallArg(0, buffer);
        depth = getSyscallArg(1, buffer);
        parentSlot = current_extra_caps.excaprefs[0];
        untyped = parentSlot->cap;
        root = current_extra_caps.excaprefs[1]->cap;

        /* Find first free pool */
        for (i = 0; i < nASIDPools && armKSASIDTable[i]; i++);

        if (unlikely(i == nASIDPools)) { /* If no unallocated pool is found */
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid_base = i << asidLowBits;

        if (unlikely(cap_get_capType(untyped) != cap_untyped_cap ||
                     cap_untyped_cap_get_capBlockSize(untyped) != seL4_ASIDPoolBits ||
                     cap_untyped_cap_get_capIsDevice(untyped))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        status = ensureNoChildren(parentSlot);
        if (unlikely(status != EXCEPTION_NONE)) {
            return status;
        }

        frame = WORD_PTR(cap_untyped_cap_get_capPtr(untyped));

        lu_ret = lookupTargetSlot(root, index, depth);
        if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
            return lu_ret.status;
        }
        destSlot = lu_ret.slot;

        status = ensureEmptySlot(destSlot);
        if (unlikely(status != EXCEPTION_NONE)) {
            return status;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performASIDControlInvocation(frame, destSlot, parentSlot, asid_base);
    }

    case cap_asid_pool_cap: {
        cap_t vspaceCap;
        cte_t *vspaceCapSlot;
        asid_pool_t *pool;
        word_t i;
        asid_t asid;

        if (unlikely(invLabel != ARMASIDPoolAssign)) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(current_extra_caps.excaprefs[0] == NULL)) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vspaceCapSlot = current_extra_caps.excaprefs[0];
        vspaceCap = vspaceCapSlot->cap;

        if (unlikely(!isVTableRoot(vspaceCap) || cap_vspace_cap_get_capVSIsMapped(vspaceCap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pool = armKSASIDTable[cap_asid_pool_cap_get_capASIDBase(cap) >> asidLowBits];

        if (unlikely(!pool)) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            current_lookup_fault = lookup_fault_invalid_root_new();

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(pool != ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap)))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Find first free ASID */
        asid = cap_asid_pool_cap_get_capASIDBase(cap);
        for (i = 0; i < (1 << asidLowBits) && (asid + i == 0
                                               || (asid_map_get_type(pool->array[i]) != asid_map_asid_map_none)); i++);

        if (unlikely(i == 1 << asidLowBits)) {
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid += i;

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performASIDPoolInvocation(asid, pool, vspaceCapSlot);
    }

    default:
        fail("Invalid ARM arch cap type");
    }
}

#ifdef CONFIG_DEBUG_BUILD
void kernelPrefetchAbort(word_t pc) VISIBLE;
void kernelDataAbort(word_t pc) VISIBLE;

void kernelPrefetchAbort(word_t pc)
{
    printf("\n\nKERNEL PREFETCH ABORT!\n");
    printf("Faulting instruction: 0x%"SEL4_PRIx_word"\n", pc);
    printf("ESR (IFSR): 0x%"SEL4_PRIx_word"\n", getIFSR());
    halt();
}

void kernelDataAbort(word_t pc)
{
    printf("\n\nKERNEL DATA ABORT!\n");
    printf("Faulting instruction: 0x%"SEL4_PRIx_word"\n", pc);
    printf("FAR: 0x%"SEL4_PRIx_word" ESR (DFSR): 0x%"SEL4_PRIx_word"\n",
           getFAR(), getDFSR());
    halt();
}
#endif /* CONFIG_DEBUG_BUILD */

#ifdef CONFIG_PRINTING
typedef struct readWordFromVSpace_ret {
    exception_t status;
    word_t value;
} readWordFromVSpace_ret_t;

static readWordFromVSpace_ret_t readWordFromVSpace(vspace_root_t *pd, word_t vaddr)
{
    readWordFromVSpace_ret_t ret;
    word_t offset;
    pptr_t kernel_vaddr;
    word_t *value;

    lookupPTSlot_ret_t lookup_ret = lookupPTSlot(pd, vaddr);

    /* Check that the returned slot is a page. */
    if (!pte_ptr_get_valid(lookup_ret.ptSlot) ||
        (pte_pte_table_ptr_get_present(lookup_ret.ptSlot) && lookup_ret.ptBitsLeft > PAGE_BITS)) {
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    offset = vaddr & MASK(lookup_ret.ptBitsLeft);
    kernel_vaddr = (pptr_t)paddr_to_pptr(pte_page_ptr_get_page_base_address(lookup_ret.ptSlot));
    value = (word_t *)(kernel_vaddr + offset);

    ret.status = EXCEPTION_NONE;
    ret.value = *value;
    return ret;
}

void Arch_userStackTrace(tcb_t *tptr)
{
    cap_t threadRoot;
    vspace_root_t *vspaceRoot;
    word_t sp;

    threadRoot = TCB_PTR_CTE_PTR(tptr, tcbVTable)->cap;

    /* lookup the vspace root */
    if (cap_get_capType(threadRoot) != cap_vspace_cap) {
        printf("Invalid vspace\n");
        return;
    }

    vspaceRoot = VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(threadRoot));
    sp = getRegister(tptr, SP_EL0);

    /* check for alignment so we don't have to worry about accessing
     * words that might be on two different pages */
    if (!IS_ALIGNED(sp, seL4_WordSizeBits)) {
        printf("SP not aligned\n");
        return;
    }

    for (unsigned int i = 0; i < CONFIG_USER_STACK_TRACE_LENGTH; i++) {
        word_t address = sp + (i * sizeof(word_t));
        readWordFromVSpace_ret_t result = readWordFromVSpace(vspaceRoot,
                                                             address);
        if (result.status == EXCEPTION_NONE) {
            printf("0x%"SEL4_PRIx_word": 0x%"SEL4_PRIx_word"\n",
                   address, result.value);
        } else {
            printf("0x%"SEL4_PRIx_word": INVALID\n", address);
        }
    }
}
#endif /* CONFIG_PRINTING */

#if defined(CONFIG_KERNEL_LOG_BUFFER)
exception_t benchmark_arch_map_logBuffer(word_t frame_cptr)
{
    lookupCapAndSlot_ret_t lu_ret;
    vm_page_size_t frameSize;
    pptr_t  frame_pptr;

    /* faulting section */
    lu_ret = lookupCapAndSlot(NODE_STATE(ksCurThread), frame_cptr);

    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        userError("Invalid cap #%lu.", frame_cptr);
        current_fault = seL4_Fault_CapFault_new(frame_cptr, false);

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_get_capType(lu_ret.cap) != cap_frame_cap) {
        userError("Invalid cap. Log buffer should be of a frame cap");
        current_fault = seL4_Fault_CapFault_new(frame_cptr, false);

        return EXCEPTION_SYSCALL_ERROR;
    }

    frameSize = cap_frame_cap_get_capFSize(lu_ret.cap);

    if (frameSize != ARMLargePage) {
        userError("Invalid frame size. The kernel expects 2M log buffer");
        current_fault = seL4_Fault_CapFault_new(frame_cptr, false);

        return EXCEPTION_SYSCALL_ERROR;
    }

    frame_pptr = cap_frame_cap_get_capFBasePtr(lu_ret.cap);

    ksUserLogBuffer = pptr_to_paddr((void *) frame_pptr);

    *armKSGlobalLogPTE = pte_pte_page_new(
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                             0, // XN
#else
                             1, // UXN
#endif
                             ksUserLogBuffer,
                             0,                         /* global */
                             1,                         /* access flag */
                             SMP_TERNARY(SMP_SHARE, 0), /* Inner-shareable if SMP enabled, otherwise unshared */
                             0,                         /* VMKernelOnly */
                             NORMAL_WT);

    cleanByVA_PoU((vptr_t)armKSGlobalLogPTE, addrFromKPPtr(armKSGlobalLogPTE));
    invalidateTranslationSingle(KS_LOG_PPTR);
    return EXCEPTION_NONE;
}
#endif /* CONFIG_KERNEL_LOG_BUFFER */

