/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/boot.h>
#include <model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/api/invocation.h>
#include <arch/kernel/tlb_bitmap.h>
#include <mode/kernel/tlb.h>
#include <mode/kernel/vspace.h>

static exception_t performPageGetAddress(void *vbase_ptr)
{
    paddr_t capFBasePtr;

    /* Get the physical address of this frame. */
    capFBasePtr = pptr_to_paddr(vbase_ptr);

    /* return it in the first message register */
    setRegister(NODE_STATE(ksCurThread), msgRegisters[0], capFBasePtr);
    setRegister(NODE_STATE(ksCurThread), msgInfoRegister,
                wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, 1)));

    return EXCEPTION_NONE;
}


void deleteASIDPool(asid_t asid_base, asid_pool_t *pool)
{
    /* Haskell error: "ASID pool's base must be aligned" */
    assert(IS_ALIGNED(asid_base, asidLowBits));

    if (x86KSASIDTable[asid_base >> asidLowBits] == pool) {
        for (unsigned int offset = 0; offset < BIT(asidLowBits); offset++) {
            asid_map_t asid_map = pool->array[offset];
            if (asid_map_get_type(asid_map) == asid_map_asid_map_vspace) {
                vspace_root_t *vspace = (vspace_root_t *)asid_map_asid_map_vspace_get_vspace_root(asid_map);
                hwASIDInvalidate(asid_base + offset, vspace);
            }
        }
        x86KSASIDTable[asid_base >> asidLowBits] = NULL;
        setVMRoot(NODE_STATE(ksCurThread));
    }
}

exception_t performASIDControlInvocation(void *frame, cte_t *slot, cte_t *parent, asid_t asid_base)
{
    /** AUXUPD: "(True, typ_region_bytes (ptr_val \<acute>frame) 12)" */
    /** GHOSTUPD: "(True, gs_clear_region (ptr_val \<acute>frame) 12)" */
    cap_untyped_cap_ptr_set_capFreeIndex(&(parent->cap),
                                         MAX_FREE_INDEX(cap_untyped_cap_get_capBlockSize(parent->cap)));

    memzero(frame, BIT(pageBitsForSize(X86_SmallPage)));
    /** AUXUPD: "(True, ptr_retyps 1 (Ptr (ptr_val \<acute>frame) :: asid_pool_C ptr))" */

    cteInsert(
        cap_asid_pool_cap_new(
            asid_base,          /* capASIDBase  */
            WORD_REF(frame)     /* capASIDPool  */
        ),
        parent,
        slot
    );
    /* Haskell error: "ASID pool's base must be aligned" */
    assert((asid_base & MASK(asidLowBits)) == 0);
    x86KSASIDTable[asid_base >> asidLowBits] = (asid_pool_t *)frame;

    return EXCEPTION_NONE;
}

void deleteASID(asid_t asid, vspace_root_t *vspace)
{
    asid_pool_t *poolPtr;

    poolPtr = x86KSASIDTable[asid >> asidLowBits];
    if (poolPtr != NULL) {
        asid_map_t asid_map = poolPtr->array[asid & MASK(asidLowBits)];
        if (asid_map_get_type(asid_map) == asid_map_asid_map_vspace &&
            (vspace_root_t *)asid_map_asid_map_vspace_get_vspace_root(asid_map) == vspace) {
            hwASIDInvalidate(asid, vspace);
            poolPtr->array[asid & MASK(asidLowBits)] = asid_map_asid_map_none_new();
            setVMRoot(NODE_STATE(ksCurThread));
        }
    }
}

word_t *PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread)
{
    word_t      w_bufferPtr;
    cap_t       bufferCap;
    vm_rights_t vm_rights;

    w_bufferPtr = thread->tcbIPCBuffer;
    bufferCap = TCB_PTR_CTE_PTR(thread, tcbBuffer)->cap;

    if (cap_get_capType(bufferCap) != cap_frame_cap) {
        return NULL;
    }
    if (unlikely(cap_frame_cap_get_capFIsDevice(bufferCap))) {
        return NULL;
    }

    vm_rights = cap_frame_cap_get_capFVMRights(bufferCap);
    if (vm_rights == VMReadWrite || (!isReceiver && vm_rights == VMReadOnly)) {
        word_t basePtr, pageBits;

        basePtr = cap_frame_cap_get_capFBasePtr(bufferCap);
        pageBits = pageBitsForSize(cap_frame_cap_get_capFSize(bufferCap));
        return (word_t *)(basePtr + (w_bufferPtr & MASK(pageBits)));
    } else {
        return NULL;
    }
}

bool_t CONST isValidVTableRoot(cap_t cap)
{
    return isValidNativeRoot(cap);
}


BOOT_CODE bool_t map_kernel_window_devices(pte_t *pt, uint32_t num_ioapic, paddr_t *ioapic_paddrs, uint32_t num_drhu,
                                           paddr_t *drhu_list)
{
    word_t idx = (KDEV_BASE & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS;
    paddr_t phys;
    pte_t pte;
    unsigned int i;
    /* map kernel devices: APIC */
    phys = apic_get_base_paddr();
    if (!phys) {
        return false;
    }
    if (!reserve_region((p_region_t) {
    phys, phys + 0x1000
})) {
        return false;
    }
    pte = x86_make_device_pte(phys);

    assert(idx == (PPTR_APIC & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
    pt[idx] = pte;
    idx++;
    for (i = 0; i < num_ioapic; i++) {
        phys = ioapic_paddrs[i];
        if (!reserve_region((p_region_t) {
        phys, phys + 0x1000
    })) {
            return false;
        }
        pte = x86_make_device_pte(phys);
        assert(idx == ((PPTR_IOAPIC_START + i * BIT(PAGE_BITS)) & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
        pt[idx] = pte;
        idx++;
        if (idx == BIT(PT_INDEX_BITS)) {
            return false;
        }
    }
    /* put in null mappings for any extra IOAPICs */
    for (; i < CONFIG_MAX_NUM_IOAPIC; i++) {
        pte = x86_make_empty_pte();
        assert(idx == ((PPTR_IOAPIC_START + i * BIT(PAGE_BITS)) & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
        pt[idx] = pte;
        idx++;
    }

    /* map kernel devices: IOMMUs */
    for (i = 0; i < num_drhu; i++) {
        phys = (paddr_t)drhu_list[i];
        if (!reserve_region((p_region_t) {
        phys, phys + 0x1000
    })) {
            return false;
        }
        pte = x86_make_device_pte(phys);

        assert(idx == ((PPTR_DRHU_START + i * BIT(PAGE_BITS)) & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
        pt[idx] = pte;
        idx++;
        if (idx == BIT(PT_INDEX_BITS)) {
            return false;
        }
    }

    /* mark unused kernel-device pages as 'not present' */
    while (idx < BIT(PT_INDEX_BITS)) {
        pte = x86_make_empty_pte();
        pt[idx] = pte;
        idx++;
    }

    /* Check we haven't added too many kernel-device mappings.*/
    assert(idx == BIT(PT_INDEX_BITS));
    return true;
}

BOOT_CODE static void init_idt(idt_entry_t *idt)
{
    init_idt_entry(idt, 0x00, int_00);
    init_idt_entry(idt, 0x01, int_01);
    init_idt_entry(idt, 0x02, int_02);
    init_idt_entry(idt, 0x03, int_03);
    init_idt_entry(idt, 0x04, int_04);
    init_idt_entry(idt, 0x05, int_05);
    init_idt_entry(idt, 0x06, int_06);
    init_idt_entry(idt, 0x07, int_07);
    init_idt_entry(idt, 0x08, int_08);
    init_idt_entry(idt, 0x09, int_09);
    init_idt_entry(idt, 0x0a, int_0a);
    init_idt_entry(idt, 0x0b, int_0b);
    init_idt_entry(idt, 0x0c, int_0c);
    init_idt_entry(idt, 0x0d, int_0d);
    init_idt_entry(idt, 0x0e, int_0e);
    init_idt_entry(idt, 0x0f, int_0f);

    init_idt_entry(idt, 0x10, int_10);
    init_idt_entry(idt, 0x11, int_11);
    init_idt_entry(idt, 0x12, int_12);
    init_idt_entry(idt, 0x13, int_13);
    init_idt_entry(idt, 0x14, int_14);
    init_idt_entry(idt, 0x15, int_15);
    init_idt_entry(idt, 0x16, int_16);
    init_idt_entry(idt, 0x17, int_17);
    init_idt_entry(idt, 0x18, int_18);
    init_idt_entry(idt, 0x19, int_19);
    init_idt_entry(idt, 0x1a, int_1a);
    init_idt_entry(idt, 0x1b, int_1b);
    init_idt_entry(idt, 0x1c, int_1c);
    init_idt_entry(idt, 0x1d, int_1d);
    init_idt_entry(idt, 0x1e, int_1e);
    init_idt_entry(idt, 0x1f, int_1f);

    init_idt_entry(idt, 0x20, int_20);
    init_idt_entry(idt, 0x21, int_21);
    init_idt_entry(idt, 0x22, int_22);
    init_idt_entry(idt, 0x23, int_23);
    init_idt_entry(idt, 0x24, int_24);
    init_idt_entry(idt, 0x25, int_25);
    init_idt_entry(idt, 0x26, int_26);
    init_idt_entry(idt, 0x27, int_27);
    init_idt_entry(idt, 0x28, int_28);
    init_idt_entry(idt, 0x29, int_29);
    init_idt_entry(idt, 0x2a, int_2a);
    init_idt_entry(idt, 0x2b, int_2b);
    init_idt_entry(idt, 0x2c, int_2c);
    init_idt_entry(idt, 0x2d, int_2d);
    init_idt_entry(idt, 0x2e, int_2e);
    init_idt_entry(idt, 0x2f, int_2f);

    init_idt_entry(idt, 0x30, int_30);
    init_idt_entry(idt, 0x31, int_31);
    init_idt_entry(idt, 0x32, int_32);
    init_idt_entry(idt, 0x33, int_33);
    init_idt_entry(idt, 0x34, int_34);
    init_idt_entry(idt, 0x35, int_35);
    init_idt_entry(idt, 0x36, int_36);
    init_idt_entry(idt, 0x37, int_37);
    init_idt_entry(idt, 0x38, int_38);
    init_idt_entry(idt, 0x39, int_39);
    init_idt_entry(idt, 0x3a, int_3a);
    init_idt_entry(idt, 0x3b, int_3b);
    init_idt_entry(idt, 0x3c, int_3c);
    init_idt_entry(idt, 0x3d, int_3d);
    init_idt_entry(idt, 0x3e, int_3e);
    init_idt_entry(idt, 0x3f, int_3f);

    init_idt_entry(idt, 0x40, int_40);
    init_idt_entry(idt, 0x41, int_41);
    init_idt_entry(idt, 0x42, int_42);
    init_idt_entry(idt, 0x43, int_43);
    init_idt_entry(idt, 0x44, int_44);
    init_idt_entry(idt, 0x45, int_45);
    init_idt_entry(idt, 0x46, int_46);
    init_idt_entry(idt, 0x47, int_47);
    init_idt_entry(idt, 0x48, int_48);
    init_idt_entry(idt, 0x49, int_49);
    init_idt_entry(idt, 0x4a, int_4a);
    init_idt_entry(idt, 0x4b, int_4b);
    init_idt_entry(idt, 0x4c, int_4c);
    init_idt_entry(idt, 0x4d, int_4d);
    init_idt_entry(idt, 0x4e, int_4e);
    init_idt_entry(idt, 0x4f, int_4f);

    init_idt_entry(idt, 0x50, int_50);
    init_idt_entry(idt, 0x51, int_51);
    init_idt_entry(idt, 0x52, int_52);
    init_idt_entry(idt, 0x53, int_53);
    init_idt_entry(idt, 0x54, int_54);
    init_idt_entry(idt, 0x55, int_55);
    init_idt_entry(idt, 0x56, int_56);
    init_idt_entry(idt, 0x57, int_57);
    init_idt_entry(idt, 0x58, int_58);
    init_idt_entry(idt, 0x59, int_59);
    init_idt_entry(idt, 0x5a, int_5a);
    init_idt_entry(idt, 0x5b, int_5b);
    init_idt_entry(idt, 0x5c, int_5c);
    init_idt_entry(idt, 0x5d, int_5d);
    init_idt_entry(idt, 0x5e, int_5e);
    init_idt_entry(idt, 0x5f, int_5f);

    init_idt_entry(idt, 0x60, int_60);
    init_idt_entry(idt, 0x61, int_61);
    init_idt_entry(idt, 0x62, int_62);
    init_idt_entry(idt, 0x63, int_63);
    init_idt_entry(idt, 0x64, int_64);
    init_idt_entry(idt, 0x65, int_65);
    init_idt_entry(idt, 0x66, int_66);
    init_idt_entry(idt, 0x67, int_67);
    init_idt_entry(idt, 0x68, int_68);
    init_idt_entry(idt, 0x69, int_69);
    init_idt_entry(idt, 0x6a, int_6a);
    init_idt_entry(idt, 0x6b, int_6b);
    init_idt_entry(idt, 0x6c, int_6c);
    init_idt_entry(idt, 0x6d, int_6d);
    init_idt_entry(idt, 0x6e, int_6e);
    init_idt_entry(idt, 0x6f, int_6f);

    init_idt_entry(idt, 0x70, int_70);
    init_idt_entry(idt, 0x71, int_71);
    init_idt_entry(idt, 0x72, int_72);
    init_idt_entry(idt, 0x73, int_73);
    init_idt_entry(idt, 0x74, int_74);
    init_idt_entry(idt, 0x75, int_75);
    init_idt_entry(idt, 0x76, int_76);
    init_idt_entry(idt, 0x77, int_77);
    init_idt_entry(idt, 0x78, int_78);
    init_idt_entry(idt, 0x79, int_79);
    init_idt_entry(idt, 0x7a, int_7a);
    init_idt_entry(idt, 0x7b, int_7b);
    init_idt_entry(idt, 0x7c, int_7c);
    init_idt_entry(idt, 0x7d, int_7d);
    init_idt_entry(idt, 0x7e, int_7e);
    init_idt_entry(idt, 0x7f, int_7f);

    init_idt_entry(idt, 0x80, int_80);
    init_idt_entry(idt, 0x81, int_81);
    init_idt_entry(idt, 0x82, int_82);
    init_idt_entry(idt, 0x83, int_83);
    init_idt_entry(idt, 0x84, int_84);
    init_idt_entry(idt, 0x85, int_85);
    init_idt_entry(idt, 0x86, int_86);
    init_idt_entry(idt, 0x87, int_87);
    init_idt_entry(idt, 0x88, int_88);
    init_idt_entry(idt, 0x89, int_89);
    init_idt_entry(idt, 0x8a, int_8a);
    init_idt_entry(idt, 0x8b, int_8b);
    init_idt_entry(idt, 0x8c, int_8c);
    init_idt_entry(idt, 0x8d, int_8d);
    init_idt_entry(idt, 0x8e, int_8e);
    init_idt_entry(idt, 0x8f, int_8f);

    init_idt_entry(idt, 0x90, int_90);
    init_idt_entry(idt, 0x91, int_91);
    init_idt_entry(idt, 0x92, int_92);
    init_idt_entry(idt, 0x93, int_93);
    init_idt_entry(idt, 0x94, int_94);
    init_idt_entry(idt, 0x95, int_95);
    init_idt_entry(idt, 0x96, int_96);
    init_idt_entry(idt, 0x97, int_97);
    init_idt_entry(idt, 0x98, int_98);
    init_idt_entry(idt, 0x99, int_99);
    init_idt_entry(idt, 0x9a, int_9a);
    init_idt_entry(idt, 0x9b, int_9b);
    init_idt_entry(idt, 0x9c, int_9c);
    init_idt_entry(idt, 0x9d, int_9d);
    init_idt_entry(idt, 0x9e, int_9e);
    init_idt_entry(idt, 0x9f, int_9f);

    init_idt_entry(idt, 0xa0, int_a0);
    init_idt_entry(idt, 0xa1, int_a1);
    init_idt_entry(idt, 0xa2, int_a2);
    init_idt_entry(idt, 0xa3, int_a3);
    init_idt_entry(idt, 0xa4, int_a4);
    init_idt_entry(idt, 0xa5, int_a5);
    init_idt_entry(idt, 0xa6, int_a6);
    init_idt_entry(idt, 0xa7, int_a7);
    init_idt_entry(idt, 0xa8, int_a8);
    init_idt_entry(idt, 0xa9, int_a9);
    init_idt_entry(idt, 0xaa, int_aa);
    init_idt_entry(idt, 0xab, int_ab);
    init_idt_entry(idt, 0xac, int_ac);
    init_idt_entry(idt, 0xad, int_ad);
    init_idt_entry(idt, 0xae, int_ae);
    init_idt_entry(idt, 0xaf, int_af);

    init_idt_entry(idt, 0xb0, int_b0);
    init_idt_entry(idt, 0xb1, int_b1);
    init_idt_entry(idt, 0xb2, int_b2);
    init_idt_entry(idt, 0xb3, int_b3);
    init_idt_entry(idt, 0xb4, int_b4);
    init_idt_entry(idt, 0xb5, int_b5);
    init_idt_entry(idt, 0xb6, int_b6);
    init_idt_entry(idt, 0xb7, int_b7);
    init_idt_entry(idt, 0xb8, int_b8);
    init_idt_entry(idt, 0xb9, int_b9);
    init_idt_entry(idt, 0xba, int_ba);
    init_idt_entry(idt, 0xbb, int_bb);
    init_idt_entry(idt, 0xbc, int_bc);
    init_idt_entry(idt, 0xbd, int_bd);
    init_idt_entry(idt, 0xbe, int_be);
    init_idt_entry(idt, 0xbf, int_bf);

    init_idt_entry(idt, 0xc0, int_c0);
    init_idt_entry(idt, 0xc1, int_c1);
    init_idt_entry(idt, 0xc2, int_c2);
    init_idt_entry(idt, 0xc3, int_c3);
    init_idt_entry(idt, 0xc4, int_c4);
    init_idt_entry(idt, 0xc5, int_c5);
    init_idt_entry(idt, 0xc6, int_c6);
    init_idt_entry(idt, 0xc7, int_c7);
    init_idt_entry(idt, 0xc8, int_c8);
    init_idt_entry(idt, 0xc9, int_c9);
    init_idt_entry(idt, 0xca, int_ca);
    init_idt_entry(idt, 0xcb, int_cb);
    init_idt_entry(idt, 0xcc, int_cc);
    init_idt_entry(idt, 0xcd, int_cd);
    init_idt_entry(idt, 0xce, int_ce);
    init_idt_entry(idt, 0xcf, int_cf);

    init_idt_entry(idt, 0xd0, int_d0);
    init_idt_entry(idt, 0xd1, int_d1);
    init_idt_entry(idt, 0xd2, int_d2);
    init_idt_entry(idt, 0xd3, int_d3);
    init_idt_entry(idt, 0xd4, int_d4);
    init_idt_entry(idt, 0xd5, int_d5);
    init_idt_entry(idt, 0xd6, int_d6);
    init_idt_entry(idt, 0xd7, int_d7);
    init_idt_entry(idt, 0xd8, int_d8);
    init_idt_entry(idt, 0xd9, int_d9);
    init_idt_entry(idt, 0xda, int_da);
    init_idt_entry(idt, 0xdb, int_db);
    init_idt_entry(idt, 0xdc, int_dc);
    init_idt_entry(idt, 0xdd, int_dd);
    init_idt_entry(idt, 0xde, int_de);
    init_idt_entry(idt, 0xdf, int_df);

    init_idt_entry(idt, 0xe0, int_e0);
    init_idt_entry(idt, 0xe1, int_e1);
    init_idt_entry(idt, 0xe2, int_e2);
    init_idt_entry(idt, 0xe3, int_e3);
    init_idt_entry(idt, 0xe4, int_e4);
    init_idt_entry(idt, 0xe5, int_e5);
    init_idt_entry(idt, 0xe6, int_e6);
    init_idt_entry(idt, 0xe7, int_e7);
    init_idt_entry(idt, 0xe8, int_e8);
    init_idt_entry(idt, 0xe9, int_e9);
    init_idt_entry(idt, 0xea, int_ea);
    init_idt_entry(idt, 0xeb, int_eb);
    init_idt_entry(idt, 0xec, int_ec);
    init_idt_entry(idt, 0xed, int_ed);
    init_idt_entry(idt, 0xee, int_ee);
    init_idt_entry(idt, 0xef, int_ef);

    init_idt_entry(idt, 0xf0, int_f0);
    init_idt_entry(idt, 0xf1, int_f1);
    init_idt_entry(idt, 0xf2, int_f2);
    init_idt_entry(idt, 0xf3, int_f3);
    init_idt_entry(idt, 0xf4, int_f4);
    init_idt_entry(idt, 0xf5, int_f5);
    init_idt_entry(idt, 0xf6, int_f6);
    init_idt_entry(idt, 0xf7, int_f7);
    init_idt_entry(idt, 0xf8, int_f8);
    init_idt_entry(idt, 0xf9, int_f9);
    init_idt_entry(idt, 0xfa, int_fa);
    init_idt_entry(idt, 0xfb, int_fb);
    init_idt_entry(idt, 0xfc, int_fc);
    init_idt_entry(idt, 0xfd, int_fd);
    init_idt_entry(idt, 0xfe, int_fe);
    init_idt_entry(idt, 0xff, int_ff);
}

BOOT_CODE bool_t init_vm_state(void)
{
    word_t cacheLineSize;
    x86KScacheLineSizeBits = getCacheLineSizeBits();
    if (!x86KScacheLineSizeBits) {
        return false;
    }

    cacheLineSize = BIT(x86KScacheLineSizeBits);
    if (cacheLineSize != L1_CACHE_LINE_SIZE) {
        printf("Configured cache line size is %d but detected size is %lu\n",
               L1_CACHE_LINE_SIZE, cacheLineSize);
        SMP_COND_STATEMENT(return false);
    }

    /*
     * Work around -Waddress-of-packed-member. TSS is the first thing
     * in the struct and so it's safe to take its address.
     */
    void *tss_ptr = &x86KSGlobalState[CURRENT_CPU_INDEX()].x86KStss.tss;
    init_tss(tss_ptr);
    init_gdt(x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSgdt, tss_ptr);
    init_idt(x86KSGlobalState[CURRENT_CPU_INDEX()].x86KSidt);
    return true;
}

BOOT_CODE bool_t init_pat_msr(void)
{
    x86_pat_msr_t pat_msr;
    /* First verify PAT is supported by the machine.
     *      See section 11.12.1 of Volume 3 of the Intel manual */
    if ((x86_cpuid_edx(0x1, 0x0) & BIT(16)) == 0) {
        printf("PAT support not found\n");
        return false;
    }
    pat_msr.words[0] = x86_rdmsr_low(IA32_PAT_MSR);
    pat_msr.words[1] = x86_rdmsr_high(IA32_PAT_MSR);
    /* Set up the PAT MSR to the Intel defaults, just in case
     * they have been changed but a bootloader somewhere along the way */
    pat_msr = x86_pat_msr_set_pa0(pat_msr, IA32_PAT_MT_WRITE_BACK);
    pat_msr = x86_pat_msr_set_pa1(pat_msr, IA32_PAT_MT_WRITE_THROUGH);
    pat_msr = x86_pat_msr_set_pa2(pat_msr, IA32_PAT_MT_UNCACHED);
    pat_msr = x86_pat_msr_set_pa3(pat_msr, IA32_PAT_MT_UNCACHEABLE);
    /* Add the WriteCombining cache type to the PAT */
    pat_msr = x86_pat_msr_set_pa4(pat_msr, IA32_PAT_MT_WRITE_COMBINING);
    x86_wrmsr(IA32_PAT_MSR, ((uint64_t)pat_msr.words[1]) << 32 | pat_msr.words[0]);
    return true;
}

BOOT_CODE void write_it_asid_pool(cap_t it_ap_cap, cap_t it_vspace_cap)
{
    asid_pool_t *ap = ASID_POOL_PTR(pptr_of_cap(it_ap_cap));
    ap->array[IT_ASID] = asid_map_asid_map_vspace_new(pptr_of_cap(it_vspace_cap));
    x86KSASIDTable[IT_ASID >> asidLowBits] = ap;
}

asid_map_t findMapForASID(asid_t asid)
{
    asid_pool_t        *poolPtr;

    poolPtr = x86KSASIDTable[asid >> asidLowBits];
    if (!poolPtr) {
        return asid_map_asid_map_none_new();
    }

    return poolPtr->array[asid & MASK(asidLowBits)];
}

findVSpaceForASID_ret_t findVSpaceForASID(asid_t asid)
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

    ret.vspace_root = (vspace_root_t *)asid_map_asid_map_vspace_get_vspace_root(asid_map);
    ret.status = EXCEPTION_NONE;
    return ret;
}

exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType)
{
    word_t addr;
    uint32_t fault;

    addr = getFaultAddr();
    fault = getRegister(thread, Error);

    switch (vm_faultType) {
    case X86DataFault:
        current_fault = seL4_Fault_VMFault_new(addr, fault, false);
        return EXCEPTION_FAULT;

    case X86InstructionFault:
        current_fault = seL4_Fault_VMFault_new(addr, fault, true);
        return EXCEPTION_FAULT;

    default:
        fail("Invalid VM fault type");
    }
}

uint32_t CONST WritableFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMReadOnly:
        return 0;

    case VMKernelOnly:
    case VMReadWrite:
        return 1;

    default:
        fail("Invalid VM rights");
    }
}

uint32_t CONST SuperUserFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMKernelOnly:
        return 0;

    case VMReadOnly:
    case VMReadWrite:
        return 1;

    default:
        fail("Invalid VM rights");
    }
}

lookupPTSlot_ret_t lookupPTSlot(vspace_root_t *vspace, vptr_t vptr)
{
    lookupPTSlot_ret_t ret;
    lookupPDSlot_ret_t pdSlot;

    pdSlot = lookupPDSlot(vspace, vptr);
    if (pdSlot.status != EXCEPTION_NONE) {
        ret.ptSlot = NULL;
        ret.status = pdSlot.status;
        return ret;
    }
    if ((pde_ptr_get_page_size(pdSlot.pdSlot) != pde_pde_pt) ||
        !pde_pde_pt_ptr_get_present(pdSlot.pdSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(PAGE_BITS + PT_INDEX_BITS);
        ret.ptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        pte_t *pt;
        pte_t *ptSlot;
        word_t ptIndex;

        pt = paddr_to_pptr(pde_pde_pt_ptr_get_pt_base_address(pdSlot.pdSlot));
        ptIndex = (vptr >> PAGE_BITS) & MASK(PT_INDEX_BITS);
        ptSlot = pt + ptIndex;

        ret.ptSlot = ptSlot;
        ret.status = EXCEPTION_NONE;
        return ret;
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

vm_rights_t CONST maskVMRights(vm_rights_t vm_rights, seL4_CapRights_t cap_rights_mask)
{
    if (vm_rights == VMReadOnly && seL4_CapRights_get_capAllowRead(cap_rights_mask)) {
        return VMReadOnly;
    }
    if (vm_rights == VMReadWrite && seL4_CapRights_get_capAllowRead(cap_rights_mask)) {
        if (!seL4_CapRights_get_capAllowWrite(cap_rights_mask)) {
            return VMReadOnly;
        } else {
            return VMReadWrite;
        }
    }
    return VMKernelOnly;
}

void flushTable(vspace_root_t *vspace, word_t vptr, pte_t *pt, asid_t asid)
{
    word_t i;
    cap_t        threadRoot;

    assert(IS_ALIGNED(vptr, PT_INDEX_BITS + PAGE_BITS));

    /* check if page table belongs to current address space */
    threadRoot = TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbVTable)->cap;
    /* find valid mappings */
    for (i = 0; i < BIT(PT_INDEX_BITS); i++) {
        if (pte_get_present(pt[i])) {
            if (config_set(CONFIG_SUPPORT_PCID) || (isValidNativeRoot(threadRoot)
                                                    && (vspace_root_t *)pptr_of_cap(threadRoot) == vspace)) {
                invalidateTranslationSingleASID(vptr + (i << PAGE_BITS), asid,
                                                SMP_TERNARY(tlb_bitmap_get(vspace), 0));
            }
        }
    }
}


void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPTSlot_ret_t  lu_ret;
    lookupPDSlot_ret_t  pd_ret;
    pde_t               *pde;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    switch (page_size) {
    case X86_SmallPage:
        lu_ret = lookupPTSlot(find_ret.vspace_root, vptr);
        if (lu_ret.status != EXCEPTION_NONE) {
            return;
        }
        if (!(pte_ptr_get_present(lu_ret.ptSlot)
              && (pte_ptr_get_page_base_address(lu_ret.ptSlot)
                  == pptr_to_paddr(pptr)))) {
            return;
        }
        *lu_ret.ptSlot = makeUserPTEInvalid();
        break;

    case X86_LargePage:
        pd_ret = lookupPDSlot(find_ret.vspace_root, vptr);
        if (pd_ret.status != EXCEPTION_NONE) {
            return;
        }
        pde = pd_ret.pdSlot;
        if (!(pde_ptr_get_page_size(pde) == pde_pde_large
              && pde_pde_large_ptr_get_present(pde)
              && (pde_pde_large_ptr_get_page_base_address(pde)
                  == pptr_to_paddr(pptr)))) {
            return;
        }
        *pde = makeUserPDEInvalid();
        break;

    default:
        if (!modeUnmapPage(page_size, find_ret.vspace_root, vptr, pptr)) {
            return;
        }
        break;
    }

    invalidateTranslationSingleASID(vptr, asid,
                                    SMP_TERNARY(tlb_bitmap_get(find_ret.vspace_root), 0));
}

void unmapPageTable(asid_t asid, vptr_t vaddr, pte_t *pt)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPDSlot_ret_t    lu_ret;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    lu_ret = lookupPDSlot(find_ret.vspace_root, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }

    /* check if the PD actually refers to the PT */
    if (!(pde_ptr_get_page_size(lu_ret.pdSlot) == pde_pde_pt &&
          pde_pde_pt_ptr_get_present(lu_ret.pdSlot) &&
          (pde_pde_pt_ptr_get_pt_base_address(lu_ret.pdSlot) == pptr_to_paddr(pt)))) {
        return;
    }

    flushTable(find_ret.vspace_root, vaddr, pt, asid);

    *lu_ret.pdSlot = makeUserPDEInvalid();

    invalidatePageStructureCacheASID(pptr_to_paddr(find_ret.vspace_root), asid,
                                     SMP_TERNARY(tlb_bitmap_get(find_ret.vspace_root), 0));
}

static exception_t performX86PageInvocationMapPTE(cap_t cap, cte_t *ctSlot, pte_t *ptSlot, pte_t pte,
                                                  vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *ptSlot = pte;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_frame_cap_get_capFMappedASID(cap),
                                     SMP_TERNARY(tlb_bitmap_get(vspace), 0));
    return EXCEPTION_NONE;
}

static exception_t performX86PageInvocationMapPDE(cap_t cap, cte_t *ctSlot, pde_t *pdSlot, pde_t pde,
                                                  vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *pdSlot = pde;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_frame_cap_get_capFMappedASID(cap),
                                     SMP_TERNARY(tlb_bitmap_get(vspace), 0));
    return EXCEPTION_NONE;
}


static exception_t performX86PageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    assert(cap_frame_cap_get_capFMappedASID(cap));
    assert(cap_frame_cap_get_capFMapType(cap) == X86_MappingVSpace);
    // We have this `if` for something we just asserted to be true for simplicity of verification
    // This has no performance implications as when this function is inlined this `if` will be
    // inside an identical `if` and will therefore be elided
    if (cap_frame_cap_get_capFMappedASID(cap)) {
        unmapPage(
            cap_frame_cap_get_capFSize(cap),
            cap_frame_cap_get_capFMappedASID(cap),
            cap_frame_cap_get_capFMappedAddress(cap),
            (void *)cap_frame_cap_get_capFBasePtr(cap)
        );
    }

    cap_frame_cap_ptr_set_capFMappedAddress(&ctSlot->cap, 0);
    cap_frame_cap_ptr_set_capFMappedASID(&ctSlot->cap, asidInvalid);
    cap_frame_cap_ptr_set_capFMapType(&ctSlot->cap, X86_MappingNone);

    return EXCEPTION_NONE;
}

static exception_t performX86FrameInvocationUnmap(cap_t cap, cte_t *cte)
{
    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        switch (cap_frame_cap_get_capFMapType(cap)) {
        case X86_MappingVSpace:
            return performX86PageInvocationUnmap(cap, cte);
#ifdef CONFIG_IOMMU
        case X86_MappingIOSpace:
            return performX86IOUnMapInvocation(cap, cte);
#endif
#ifdef CONFIG_VTX
        case X86_MappingEPT:
            return performX86EPTPageInvocationUnmap(cap, cte);
#endif
        case X86_MappingNone:
            fail("Mapped frame cap was not mapped");
            break;
        }
    }

    return EXCEPTION_NONE;
}

struct create_mapping_pte_return {
    exception_t status;
    pte_t pte;
    pte_t *ptSlot;
};
typedef struct create_mapping_pte_return create_mapping_pte_return_t;

static create_mapping_pte_return_t createSafeMappingEntries_PTE(paddr_t base, word_t vaddr, vm_rights_t vmRights,
                                                                vm_attributes_t attr,
                                                                vspace_root_t *vspace)
{
    create_mapping_pte_return_t ret;
    lookupPTSlot_ret_t          lu_ret;

    lu_ret = lookupPTSlot(vspace, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        ret.status = EXCEPTION_SYSCALL_ERROR;
        /* current_lookup_fault will have been set by lookupPTSlot */
        return ret;
    }

    ret.pte = makeUserPTE(base, attr, vmRights);
    ret.ptSlot = lu_ret.ptSlot;
    ret.status = EXCEPTION_NONE;
    return ret;
}

struct create_mapping_pde_return {
    exception_t status;
    pde_t pde;
    pde_t *pdSlot;
};
typedef struct create_mapping_pde_return create_mapping_pde_return_t;

static create_mapping_pde_return_t createSafeMappingEntries_PDE(paddr_t base, word_t vaddr, vm_rights_t vmRights,
                                                                vm_attributes_t attr,
                                                                vspace_root_t *vspace)
{
    create_mapping_pde_return_t ret;
    lookupPDSlot_ret_t          lu_ret;

    lu_ret = lookupPDSlot(vspace, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        ret.status = EXCEPTION_SYSCALL_ERROR;
        /* current_lookup_fault will have been set by lookupPDSlot */
        return ret;
    }
    ret.pdSlot = lu_ret.pdSlot;

    /* check for existing page table */
    if ((pde_ptr_get_page_size(ret.pdSlot) == pde_pde_pt) &&
        (pde_pde_pt_ptr_get_present(ret.pdSlot))) {
        current_syscall_error.type = seL4_DeleteFirst;
        ret.status = EXCEPTION_SYSCALL_ERROR;
        return ret;
    }


    ret.pde = makeUserPDELargePage(base, attr, vmRights);
    ret.status = EXCEPTION_NONE;
    return ret;
}


exception_t decodeX86FrameInvocation(
    word_t invLabel,
    word_t length,
    cte_t *cte,
    cap_t cap,
    word_t *buffer
)
{
    switch (invLabel) {
    case X86PageMap: { /* Map */
        word_t          vaddr;
        word_t          vtop;
        word_t          w_rightsMask;
        paddr_t         paddr;
        cap_t           vspaceCap;
        vspace_root_t  *vspace;
        vm_rights_t     capVMRights;
        vm_rights_t     vmRights;
        vm_attributes_t vmAttr;
        vm_page_size_t  frameSize;
        asid_t          asid;

        if (length < 3 || current_extra_caps.excaprefs[0] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        frameSize = cap_frame_cap_get_capFSize(cap);
        vaddr = getSyscallArg(0, buffer);
        w_rightsMask = getSyscallArg(1, buffer);
        vmAttr = vmAttributesFromWord(getSyscallArg(2, buffer));
        vspaceCap = current_extra_caps.excaprefs[0]->cap;

        capVMRights = cap_frame_cap_get_capFVMRights(cap);

        if (!isValidNativeRoot(vspaceCap)) {
            userError("X86FrameMap: Attempting to map frame into invalid page directory cap.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
        vspace = (vspace_root_t *)pptr_of_cap(vspaceCap);
        asid = cap_get_capMappedASID(vspaceCap);

        if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
            if (cap_frame_cap_get_capFMappedASID(cap) != asid) {
                current_syscall_error.type = seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (cap_frame_cap_get_capFMapType(cap) != X86_MappingVSpace) {
                userError("X86Frame: Attempting to remap frame with different mapping type");
                current_syscall_error.type = seL4_IllegalOperation;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (cap_frame_cap_get_capFMappedAddress(cap) != vaddr) {
                userError("X86Frame: Attempting to map frame into multiple addresses");
                current_syscall_error.type = seL4_InvalidArgument;
                current_syscall_error.invalidArgumentNumber = 0;

                return EXCEPTION_SYSCALL_ERROR;
            }
        } else {
            vtop = vaddr + BIT(pageBitsForSize(frameSize));

            /* check vaddr and vtop against USER_TOP to catch case where vaddr + frame_size wrapped around */
            if (vaddr > USER_TOP || vtop > USER_TOP) {
                userError("X86Frame: Mapping address too high.");
                current_syscall_error.type = seL4_InvalidArgument;
                current_syscall_error.invalidArgumentNumber = 0;

                return EXCEPTION_SYSCALL_ERROR;
            }
        }

        {
            findVSpaceForASID_ret_t find_ret;

            find_ret = findVSpaceForASID(asid);
            if (find_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (find_ret.vspace_root != vspace) {
                current_syscall_error.type = seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;

                return EXCEPTION_SYSCALL_ERROR;
            }
        }

        vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

        if (!checkVPAlignment(frameSize, vaddr)) {
            current_syscall_error.type = seL4_AlignmentError;

            return EXCEPTION_SYSCALL_ERROR;
        }

        paddr = pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap));

        cap = cap_frame_cap_set_capFMappedASID(cap, asid);
        cap = cap_frame_cap_set_capFMappedAddress(cap, vaddr);
        cap = cap_frame_cap_set_capFMapType(cap, X86_MappingVSpace);

        switch (frameSize) {
        /* PTE mappings */
        case X86_SmallPage: {
            create_mapping_pte_return_t map_ret;

            map_ret = createSafeMappingEntries_PTE(paddr, vaddr, vmRights, vmAttr, vspace);
            if (map_ret.status != EXCEPTION_NONE) {
                return map_ret.status;
            }

            setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
            return performX86PageInvocationMapPTE(cap, cte, map_ret.ptSlot, map_ret.pte, vspace);
        }

        /* PDE mappings */
        case X86_LargePage: {
            create_mapping_pde_return_t map_ret;

            map_ret = createSafeMappingEntries_PDE(paddr, vaddr, vmRights, vmAttr, vspace);
            if (map_ret.status != EXCEPTION_NONE) {
                return map_ret.status;
            }

            setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
            return performX86PageInvocationMapPDE(cap, cte, map_ret.pdSlot, map_ret.pde, vspace);
        }

        default: {
            return decodeX86ModeMapPage(invLabel, frameSize, cte, cap, vspace, vaddr, paddr, vmRights, vmAttr);
        }
        }

        return EXCEPTION_SYSCALL_ERROR;
    }

    case X86PageUnmap: { /* Unmap */
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performX86FrameInvocationUnmap(cap, cte);
    }

#ifdef CONFIG_IOMMU
    case X86PageMapIO: { /* MapIO */
        return decodeX86IOMapInvocation(length, cte, cap, buffer);
    }
#endif

#ifdef CONFIG_VTX
    case X86PageMapEPT:
        return decodeX86EPTPageMap(invLabel, length, cte, cap, buffer);
#endif

    case X86PageGetAddress: {
        /* Return it in the first message register. */
        assert(n_msgRegisters >= 1);

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageGetAddress((void *)cap_frame_cap_get_capFBasePtr(cap));
    }

    default:
        current_syscall_error.type = seL4_IllegalOperation;

        return EXCEPTION_SYSCALL_ERROR;
    }
}

static exception_t performX86PageTableInvocationUnmap(cap_t cap, cte_t *ctSlot)
{

    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        pte_t *pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap));
        unmapPageTable(
            cap_page_table_cap_get_capPTMappedASID(cap),
            cap_page_table_cap_get_capPTMappedAddress(cap),
            pt
        );
        clearMemory((void *)pt, cap_get_capSizeBits(cap));
    }
    cap_page_table_cap_ptr_set_capPTIsMapped(&(ctSlot->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t performX86PageTableInvocationMap(cap_t cap, cte_t *ctSlot, pde_t pde, pde_t *pdSlot,
                                                    vspace_root_t *root)
{
    ctSlot->cap = cap;
    *pdSlot = pde;
    invalidatePageStructureCacheASID(pptr_to_paddr(root), cap_page_table_cap_get_capPTMappedASID(cap),
                                     SMP_TERNARY(tlb_bitmap_get(root), 0));
    return EXCEPTION_NONE;
}

static exception_t decodeX86PageTableInvocation(
    word_t invLabel,
    word_t length,
    cte_t *cte, cap_t cap,
    word_t *buffer
)
{
    word_t          vaddr;
    vm_attributes_t attr;
    lookupPDSlot_ret_t pdSlot;
    cap_t           vspaceCap;
    vspace_root_t  *vspace;
    pde_t           pde;
    paddr_t         paddr;
    asid_t          asid;

    if (invLabel == X86PageTableUnmap) {
        if (! isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            userError("X86PageTable: Cannot unmap if more than one cap exists.");
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performX86PageTableInvocationUnmap(cap, cte);
    }

    if (invLabel != X86PageTableMap) {
        userError("X86PageTable: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || current_extra_caps.excaprefs[0] == NULL) {
        userError("X86PageTable: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        userError("X86PageTable: Page table is already mapped to a page directory.");
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer) & (~MASK(PT_INDEX_BITS + PAGE_BITS));
    attr = vmAttributesFromWord(getSyscallArg(1, buffer));
    vspaceCap = current_extra_caps.excaprefs[0]->cap;

    if (!isValidNativeRoot(vspaceCap)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vspace = (vspace_root_t *)pptr_of_cap(vspaceCap);
    asid = cap_get_capMappedASID(vspaceCap);

    if (vaddr > USER_TOP) {
        userError("X86PageTable: Mapping address too high.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    {
        findVSpaceForASID_ret_t find_ret;

        find_ret = findVSpaceForASID(asid);
        if (find_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (find_ret.vspace_root != vspace) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    pdSlot = lookupPDSlot(vspace, vaddr);
    if (pdSlot.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (((pde_ptr_get_page_size(pdSlot.pdSlot) == pde_pde_pt) && pde_pde_pt_ptr_get_present(pdSlot.pdSlot)) ||
        ((pde_ptr_get_page_size(pdSlot.pdSlot) == pde_pde_large) && pde_pde_large_ptr_get_present(pdSlot.pdSlot))) {
        current_syscall_error.type = seL4_DeleteFirst;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
    pde = makeUserPDEPageTable(paddr, attr);

    cap = cap_page_table_cap_set_capPTIsMapped(cap, 1);
    cap = cap_page_table_cap_set_capPTMappedASID(cap, asid);
    cap = cap_page_table_cap_set_capPTMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performX86PageTableInvocationMap(cap, cte, pde, pdSlot.pdSlot, vspace);
}

exception_t decodeX86MMUInvocation(
    word_t invLabel,
    word_t length,
    cptr_t cptr,
    cte_t *cte,
    cap_t cap,
    word_t *buffer
)
{
    switch (cap_get_capType(cap)) {

    case cap_frame_cap:
        return decodeX86FrameInvocation(invLabel, length, cte, cap, buffer);

    case cap_page_table_cap:
        return decodeX86PageTableInvocation(invLabel, length, cte, cap, buffer);

    case cap_asid_control_cap: {
        word_t     i;
        asid_t           asid_base;
        word_t           index;
        word_t           depth;
        cap_t            untyped;
        cap_t            root;
        cte_t           *parentSlot;
        cte_t           *destSlot;
        lookupSlot_ret_t lu_ret;
        void            *frame;
        exception_t      status;

        if (invLabel != X86ASIDControlMakePool) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (length < 2 || current_extra_caps.excaprefs[0] == NULL
            || current_extra_caps.excaprefs[1] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        index = getSyscallArg(0, buffer);
        depth = getSyscallArg(1, buffer);
        parentSlot = current_extra_caps.excaprefs[0];
        untyped = parentSlot->cap;
        root = current_extra_caps.excaprefs[1]->cap;

        /* Find first free pool */
        for (i = 0; i < nASIDPools && x86KSASIDTable[i]; i++);

        if (i == nASIDPools) {
            /* no unallocated pool is found */
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid_base = i << asidLowBits;


        if (cap_get_capType(untyped) != cap_untyped_cap ||
            cap_untyped_cap_get_capBlockSize(untyped) != seL4_ASIDPoolBits ||
            cap_untyped_cap_get_capIsDevice(untyped)) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        status = ensureNoChildren(parentSlot);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        frame = WORD_PTR(cap_untyped_cap_get_capPtr(untyped));

        lu_ret = lookupTargetSlot(root, index, depth);
        if (lu_ret.status != EXCEPTION_NONE) {
            return lu_ret.status;
        }
        destSlot = lu_ret.slot;

        status = ensureEmptySlot(destSlot);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performASIDControlInvocation(frame, destSlot, parentSlot, asid_base);
    }

    case cap_asid_pool_cap: {
        cap_t        vspaceCap;
        cte_t       *vspaceCapSlot;
        asid_pool_t *pool;
        word_t i;
        asid_t       asid;

        if (invLabel != X86ASIDPoolAssign) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }
        if (current_extra_caps.excaprefs[0] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vspaceCapSlot = current_extra_caps.excaprefs[0];
        vspaceCap = vspaceCapSlot->cap;

        if (!(isVTableRoot(vspaceCap) || VTX_TERNARY(cap_get_capType(vspaceCap) == cap_ept_pml4_cap, 0))
            || cap_get_capMappedASID(vspaceCap) != asidInvalid) {
            userError("X86ASIDPool: Invalid vspace root.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pool = x86KSASIDTable[cap_asid_pool_cap_get_capASIDBase(cap) >> asidLowBits];
        if (!pool) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            current_lookup_fault = lookup_fault_invalid_root_new();
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (pool != ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Find first free ASID */
        asid = cap_asid_pool_cap_get_capASIDBase(cap);
        for (i = 0; i < BIT(asidLowBits) && (asid + i == 0
                                             || asid_map_get_type(pool->array[i]) != asid_map_asid_map_none); i++);

        if (i == BIT(asidLowBits)) {
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid += i;

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performASIDPoolInvocation(asid, pool, vspaceCapSlot);
    }

    default:
        return decodeX86ModeMMUInvocation(invLabel, length, cptr, cte, cap, buffer);
    }
}
