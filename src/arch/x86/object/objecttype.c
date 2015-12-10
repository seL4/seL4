/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <kernel/vspace.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/model/statedata.h>
#include <arch/machine/fpu.h>
#include <arch/object/objecttype.h>
#include <arch/object/ioport.h>
#include <machine.h>
#include <kernel/vspace.h>
#include <object/structures.h>
#include <arch/object/ipi.h>

#ifdef CONFIG_IOMMU
#include <arch/object/iospace.h>
#include <plat/machine/intel-vtd.h>
#endif

#ifdef CONFIG_VTX
#include <arch/object/vtx.h>
#include <arch/object/vcpu.h>
#endif

deriveCap_ret_t Arch_deriveCap(cte_t* slot, cap_t cap)
{
    deriveCap_ret_t ret;

    switch (cap_get_capType(cap)) {
    case cap_page_table_cap:
        ret.cap = cap_page_table_cap_set_capPTMappedObject(cap, 0);
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_page_directory_cap:
        ret.cap = cap_page_directory_cap_set_capPDMappedObject(cap, 0);
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_pdpt_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_frame_cap:
        ret.cap = cap_frame_cap_set_capFMappedObject(cap, 0);
        ret.status = EXCEPTION_NONE;
        return ret;
    case cap_io_port_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
    case cap_io_page_table_cap:
        ret.cap = cap_io_page_table_cap_set_capIOPTMappedObject(cap, 0);
        ret.status = EXCEPTION_NONE;
        return ret;
#endif
    case cap_ipi_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
    case cap_ept_page_directory_cap:
        ret.cap = cap_ept_page_directory_cap_set_capPDMappedObject(cap, 0);
        ret.status = EXCEPTION_NONE;
        return ret;
    case cap_ept_page_table_cap:
        ret.cap = cap_ept_page_table_cap_set_capPTMappedObject(cap, 0);
        ret.status = EXCEPTION_NONE;
        return ret;
#endif
#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
#endif

    default:
        /* This assert has no equivalent in haskell,
         * as the options are restricted by type */
        fail("Invalid arch cap type");
    }
}

cap_t CONST Arch_updateCapData(bool_t preserve, word_t data, cap_t cap)
{
    switch (cap_get_capType(cap)) {
#ifdef CONFIG_IOMMU
    case cap_io_space_cap: {
        io_space_capdata_t w = { { data } };
        uint16_t PCIDevice = io_space_capdata_get_PCIDevice(w);
        uint16_t domainID = io_space_capdata_get_domainID(w);
        if (!preserve && cap_io_space_cap_get_capPCIDevice(cap) == 0 &&
                domainID >= ia32KSFirstValidIODomain &&
                domainID != 0                        &&
                domainID <= MASK(ia32KSnumIODomainIDBits)) {
            return cap_io_space_cap_new(domainID, PCIDevice);
        } else {
            return cap_null_cap_new();
        }
    }
#endif
    case cap_io_port_cap: {
        io_port_capdata_t w = { .words = { data } };
        uint16_t firstPort = io_port_capdata_get_firstPort(w);
        uint16_t lastPort = io_port_capdata_get_lastPort(w);
        uint16_t capFirstPort = cap_io_port_cap_get_capIOPortFirstPort(cap);
        uint16_t capLastPort = cap_io_port_cap_get_capIOPortLastPort(cap);
        assert(capFirstPort <= capLastPort);

        /* Ensure input data is ordered correctly. */
        if (firstPort > lastPort) {
            return cap_null_cap_new();
        }

        /* Allow the update if the new cap has range no larger than the old
         * cap. */
        if ((firstPort >= capFirstPort) && (lastPort <= capLastPort)) {
            return cap_io_port_cap_new(firstPort, lastPort);
        } else {
            return cap_null_cap_new();
        }
    }

    default:
        return cap;
    }
}

cap_t CONST Arch_maskCapRights(cap_rights_t cap_rights_mask, cap_t cap)
{
    if (cap_get_capType(cap) == cap_frame_cap) {
        vm_rights_t vm_rights;

        vm_rights = vmRightsFromWord(cap_frame_cap_get_capFVMRights(cap));
        vm_rights = maskVMRights(vm_rights, cap_rights_mask);
        return cap_frame_cap_set_capFVMRights(cap, wordFromVMRights(vm_rights));
    } else {
        return cap;
    }
}

static void finalisePDMappedFrame(cap_t cap)
{
    void *object = (void*)cap_frame_cap_get_capFMappedObject(cap);
    uint32_t index = cap_frame_cap_get_capFMappedIndex(cap);
    switch (cap_frame_cap_get_capFSize(cap)) {
    case IA32_SmallPage:
        unmapPageSmall(PT_PTR(object), index);
        flushPageSmall(PT_PTR(object), index);
        break;
    case IA32_LargePage:
        unmapPageLarge(PD_PTR(object), index);
        flushPageLarge(PD_PTR(object), index);
        break;
    default:
        fail("Unknown frame size");
    }
}

cap_t Arch_finaliseCap(cap_t cap, bool_t final)
{
    switch (cap_get_capType(cap)) {
    case cap_pdpt_cap:
        if (final) {
            pdpte_t *capPtr = PDPTE_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap));
            unmapAllPageDirectories(capPtr);
            flushAllPageDirectories(capPtr);
            clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
            copyGlobalMappings(capPtr);
        }
        break;

    case cap_page_directory_cap:
        if (cap_page_directory_cap_get_capPDMappedObject(cap)) {
            unmapPageDirectory(
                PDPT_PTR(cap_page_directory_cap_get_capPDMappedObject(cap)),
                cap_page_directory_cap_get_capPDMappedIndex(cap),
                PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap))
            );
        }
        if (final) {
            unmapAllPageTables(
                PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap))
            );
            flushAllPageTables(PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap)));
            clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
#ifndef CONFIG_PAE_PAGING
            copyGlobalMappings((void*)cap_get_capPtr(cap));
#endif
        }
        if (cap_page_directory_cap_get_capPDMappedObject(cap) || final) {
            invalidateTLB();
            invalidatePageStructureCache();
        }
        break;

    case cap_page_table_cap:
        if (cap_page_table_cap_get_capPTMappedObject(cap)) {
            unmapPageTable(
                PD_PTR(cap_page_table_cap_get_capPTMappedObject(cap)),
                cap_page_table_cap_get_capPTMappedIndex(cap)
            );
        }
        if (final) {
            unmapAllPages(
                PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap))
            );
            clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        }
        if (cap_page_table_cap_get_capPTMappedObject(cap) || final) {
            flushTable(
                PD_PTR(cap_page_table_cap_get_capPTMappedObject(cap)),
                cap_page_table_cap_get_capPTMappedIndex(cap),
                PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap))
            );
        }
        break;

    case cap_frame_cap:
        if (cap_frame_cap_get_capFMappedObject(cap)) {
            switch (cap_frame_cap_get_capFMappedType(cap)) {
            case IA32_MAPPING_PD:
                finalisePDMappedFrame(cap);
                break;
#ifdef CONFIG_VTX
            case IA32_MAPPING_EPT:
                /* The unmap function for EPT will unmap and flush */
                IA32PageUnmapEPT(cap);
                break;
#endif
#ifdef CONFIG_IOMMU
            case IA32_MAPPING_IO:
                unmapIOPage(cap);
                break;
#endif
            default:
                fail("Unknown mapping type for frame");
            }
        }
        break;

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        if (final) {
            vcpu_finalise(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)));
        }
        break;
#endif

    case cap_io_port_cap:
        break;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        if (final) {
            /* Unmap any first level page table. */
            unmapVTDContextEntry(cap);
        }
        break;
    case cap_io_page_table_cap: {
        unmapIOPTCap(cap);
        if (final) {
            unmapAllIOPT(VTD_PTE_PTR(cap_get_capPtr(cap)),
                         cap_io_page_table_cap_get_capIOPTLevel(cap));
            memzero((void*)cap_get_capPtr(cap), BIT(cap_get_capSizeBits(cap)));
            flushCacheRange(cap_get_capPtr(cap), cap_get_capSizeBits(cap));
            invalidate_iotlb();
        }
    }
    break;
#endif
    case cap_ipi_cap:
        break;

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
        if (final) {
            ept_pdpte_t *pdpt = EPT_PDPT_PTR(cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(cap));
            unmapAllEPTPD(pdpt);
            memzero(pdpt, BIT(EPT_PDPT_SIZE_BITS));
            invept((void*) ((uint32_t)pdpt - EPT_PDPT_OFFSET));
        }
        break;

    case cap_ept_page_directory_cap: {
        ept_pdpte_t *pdpt = EPT_PDPT_PTR(cap_ept_page_directory_cap_get_capPDMappedObject(cap));
        int index = cap_ept_page_directory_cap_get_capPDMappedIndex(cap);
        ept_pde_t *pd = EPT_PD_PTR(cap_get_capPtr(cap));
        if (pdpt) {
            unmapEPTPD(pdpt, index, pd);
        }
        if (final) {
            unmapAllEPTPT(pd);
            memzero((void *)cap_get_capPtr(cap), BIT(cap_get_capSizeBits(cap)));
        }
        if (pdpt && final) {
            invept((void*) ((uint32_t)pdpt - EPT_PDPT_OFFSET));
        }
        break;
    }

    case cap_ept_page_table_cap: {
        ept_pde_t *pd = EPT_PD_PTR(cap_ept_page_table_cap_get_capPTMappedObject(cap));
        int index = cap_ept_page_table_cap_get_capPTMappedIndex(cap);
        ept_pte_t *pt = EPT_PT_PTR(cap_get_capPtr(cap));
        if (pd) {
            unmapEPTPT(pd, index, pt);
        }
        if (final) {
            unmapAllEPTPages(pt);
            memzero((void *)cap_get_capPtr(cap), BIT(cap_get_capSizeBits(cap)));
        }
        if (pd && final) {
            ept_pdpte_t *pdpt;
            pdpt = lookupEPTPDPTFromPD(pd);
            if (pdpt) {
                invept((void*) ((uint32_t)pdpt - EPT_PDPT_OFFSET));
            }
        }
        break;
    }
#endif /* VTX */

    default:
        fail("Invalid arch cap type");
    }

    return cap_null_cap_new();
}

static cap_t CONST
resetMemMapping(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        return cap_frame_cap_set_capFMappedObject(cap, 0);
    case cap_page_table_cap:
        return cap_page_table_cap_set_capPTMappedObject(cap, 0);
    case cap_page_directory_cap:
        return cap_page_directory_cap_set_capPDMappedObject(cap, 0);
#ifdef CONFIG_VTX
    case cap_ept_page_directory_cap:
        return cap_ept_page_directory_cap_set_capPDMappedObject(cap, 0);
    case cap_ept_page_table_cap:
        return cap_ept_page_table_cap_set_capPTMappedObject(cap, 0);
#endif
#ifdef CONFIG_IOMMU
    case cap_io_page_table_cap:
        return cap_io_page_table_cap_set_capIOPTMappedObject(cap, 0);
#endif
    default:
        break;
    }

    return cap;
}

cap_t Arch_recycleCap(bool_t is_final, cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        Arch_finaliseCap(cap, is_final);
        if (inKernelWindow((void *)cap_get_capPtr(cap))) {
            clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        }
        return resetMemMapping(cap);

    case cap_page_table_cap:
        Arch_finaliseCap(cap, true);
        return resetMemMapping(cap);

    case cap_page_directory_cap:
        Arch_finaliseCap(cap, true);
        return resetMemMapping(cap);

    case cap_pdpt_cap:
        Arch_finaliseCap(cap, true);
        return cap;

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        vcpu_finalise(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)));
        vcpu_init(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)));
        return cap;
#endif


    case cap_io_port_cap:
        return cap;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        Arch_finaliseCap(cap, true);
        return cap;

    case cap_io_page_table_cap:
        Arch_finaliseCap(cap, true);
        return resetMemMapping(cap);
#endif
    case cap_ipi_cap:
        return cap;

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
        Arch_finaliseCap(cap, true);
        return cap;

    case cap_ept_page_directory_cap:
    case cap_ept_page_table_cap:
        Arch_finaliseCap(cap, true);
        return resetMemMapping(cap);
#endif /* VTX */
    default:
        fail("Invalid arch cap type");
    }
}


bool_t CONST
Arch_hasRecycleRights(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        return cap_frame_cap_get_capFVMRights(cap) == VMReadWrite;

    default:
        return true;
    }
}


bool_t CONST Arch_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {
    case cap_frame_cap:
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            word_t botA, botB, topA, topB;
            botA = cap_frame_cap_get_capFBasePtr(cap_a);
            botB = cap_frame_cap_get_capFBasePtr(cap_b);
            topA = botA + MASK (pageBitsForSize(cap_frame_cap_get_capFSize(cap_a)));
            topB = botB + MASK (pageBitsForSize(cap_frame_cap_get_capFSize(cap_b)));
            return ((botA <= botB) && (topA >= topB) && (botB <= topB));
        }
        break;

    case cap_page_table_cap:
        if (cap_get_capType(cap_b) == cap_page_table_cap) {
            return cap_page_table_cap_get_capPTBasePtr(cap_a) ==
                   cap_page_table_cap_get_capPTBasePtr(cap_b);
        }
        break;

    case cap_page_directory_cap:
        if (cap_get_capType(cap_b) == cap_page_directory_cap) {
            return cap_page_directory_cap_get_capPDBasePtr(cap_a) ==
                   cap_page_directory_cap_get_capPDBasePtr(cap_b);
        }
        break;
    case cap_pdpt_cap:
        if (cap_get_capType(cap_b) == cap_pdpt_cap) {
            return cap_pdpt_cap_get_capPDPTBasePtr(cap_a) ==
                   cap_pdpt_cap_get_capPDPTBasePtr(cap_b);
        }
        break;

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        if (cap_get_capType(cap_b) == cap_vcpu_cap) {
            return cap_vcpu_cap_get_capVCPUPtr(cap_a) ==
                   cap_vcpu_cap_get_capVCPUPtr(cap_b);
        }
        break;
#endif

    case cap_io_port_cap:
        if (cap_get_capType(cap_b) == cap_io_port_cap) {
            return true;
        }
        break;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        if (cap_get_capType(cap_b) == cap_io_space_cap) {
            return cap_io_space_cap_get_capPCIDevice(cap_a) ==
                   cap_io_space_cap_get_capPCIDevice(cap_b);
        }
        break;

    case cap_io_page_table_cap:
        if (cap_get_capType(cap_b) == cap_io_page_table_cap) {
            return cap_io_page_table_cap_get_capIOPTBasePtr(cap_a) ==
                   cap_io_page_table_cap_get_capIOPTBasePtr(cap_b);
        }
        break;
#endif
    case cap_ipi_cap:
        if (cap_get_capType(cap_b) == cap_ipi_cap) {
            return true;
        }
        break;

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
        if (cap_get_capType(cap_b) == cap_ept_page_directory_pointer_table_cap) {
            return cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(cap_a) ==
                   cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(cap_b);
        }
        break;
    case cap_ept_page_directory_cap:
        if (cap_get_capType(cap_b) == cap_ept_page_directory_cap) {
            return cap_ept_page_directory_cap_get_capPDBasePtr(cap_a) ==
                   cap_ept_page_directory_cap_get_capPDBasePtr(cap_b);
        }
        break;
    case cap_ept_page_table_cap:
        if (cap_get_capType(cap_b) == cap_ept_page_table_cap) {
            return cap_ept_page_table_cap_get_capPTBasePtr(cap_a) ==
                   cap_ept_page_table_cap_get_capPTBasePtr(cap_b);
        }
        break;
#endif /* VTX */
    }

    return false;
}

bool_t CONST Arch_sameObjectAs(cap_t cap_a, cap_t cap_b)
{
    if (cap_get_capType(cap_a) == cap_frame_cap) {
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            return ((cap_frame_cap_get_capFBasePtr(cap_a) ==
                     cap_frame_cap_get_capFBasePtr(cap_b)) &&
                    (cap_frame_cap_get_capFSize(cap_a) ==
                     cap_frame_cap_get_capFSize(cap_b)));
        }
    }
    return Arch_sameRegionAs(cap_a, cap_b);
}

word_t
Arch_getObjectSize(word_t t)
{
    switch (t) {
    case seL4_IA32_4K:
        return pageBitsForSize(IA32_SmallPage);
    case seL4_IA32_LargePage:
        return pageBitsForSize(IA32_LargePage);
    case seL4_IA32_PageTableObject:
        return PTE_SIZE_BITS + PT_BITS;
    case seL4_IA32_PageDirectoryObject:
        return PDE_SIZE_BITS + PD_BITS;
    case seL4_IA32_PDPTObject:
        return PDPTE_SIZE_BITS + PDPT_BITS;
#ifdef CONFIG_IOMMU
    case seL4_IA32_IOPageTableObject:
        return VTD_PT_SIZE_BITS;
#endif
#ifdef CONFIG_VTX
    case seL4_IA32_VCPUObject:
        return VTX_VCPU_BITS;
    case seL4_IA32_EPTPageDirectoryPointerTableObject:
        return EPT_PDPTE_SIZE_BITS + EPT_PDPT_BITS + 1;
    case seL4_IA32_EPTPageDirectoryObject:
        return EPT_PDE_SIZE_BITS + EPT_PD_BITS;
    case seL4_IA32_EPTPageTableObject:
        return EPT_PTE_SIZE_BITS + EPT_PT_BITS;
#endif
    default:
        fail("Invalid object type");
        return 0;
    }
}

bool_t
Arch_isFrameType(word_t t)
{
    switch (t) {
    case seL4_IA32_4K:
        return true;
    case seL4_IA32_LargePage:
        return true;
    default:
        return false;
    }
}

cap_t
Arch_createObject(object_t t, void *regionBase, unsigned int userSize, bool_t deviceMemory)
{
    switch (t) {
    case seL4_IA32_4K:
        if (!deviceMemory) {
            memzero(regionBase, 1 << pageBitsForSize(IA32_SmallPage));
        }
        return cap_frame_cap_new(
                   IA32_SmallPage,         /* capFSize             */
                   0,                      /* capFMappedObject     */
                   0,                      /* capFMappedIndex      */
                   IA32_MAPPING_PD,        /* capFMappedType       */
                   VMReadWrite,            /* capFVMRights         */
                   (word_t)regionBase      /* capFBasePtr          */
               );

    case seL4_IA32_LargePage:
        if (!deviceMemory) {
            memzero(regionBase, 1 << pageBitsForSize(IA32_LargePage));
        }
        return cap_frame_cap_new(
                   IA32_LargePage,         /* capFSize             */
                   0,                      /* capFMappedObject     */
                   0,                      /* capFMappedIndex      */
                   IA32_MAPPING_PD,        /* capFMappedType       */
                   VMReadWrite,            /* capFVMRights         */
                   (word_t)regionBase      /* capFBasePtr          */
               );

    case seL4_IA32_PageTableObject:
        memzero(regionBase, 1 << PT_SIZE_BITS);
        return cap_page_table_cap_new(
                   0,                  /* capPTMappedObject    */
                   0,                  /* capPTMappedIndex     */
                   (word_t)regionBase  /* capPTBasePtr         */
               );

    case seL4_IA32_PageDirectoryObject:
        memzero(regionBase, 1 << PD_SIZE_BITS);
#ifndef CONFIG_PAE_PAGING
        copyGlobalMappings(regionBase);
#endif
        return cap_page_directory_cap_new(
                   0,                  /* capPDmappedObject */
                   0,                  /* capPDMappedIndex  */
                   (word_t)regionBase  /* capPDBasePtr      */
               );

#ifdef CONFIG_PAE_PAGING
    case seL4_IA32_PDPTObject:
        memzero(regionBase, 1 << PDPT_SIZE_BITS);
        copyGlobalMappings(regionBase);

        return cap_pdpt_cap_new(
                   (word_t)regionBase  /* capPDPTBasePtr */
               );
#endif

#ifdef CONFIG_IOMMU
    case seL4_IA32_IOPageTableObject:
        memzero(regionBase, 1 << VTD_PT_SIZE_BITS);
        return cap_io_page_table_cap_new(
                   0,  /* CapIOPTMappedLevel   */
                   0,  /* capIOPTMappedObject  */
                   0,  /* capIOPTMappedIndex   */
                   (word_t)regionBase  /* capIOPTBasePtr */
               );
#endif
#ifdef CONFIG_VTX
    case seL4_IA32_VCPUObject: {
        vcpu_t *vcpu;
        if (!vtx_enabled) {
            fail("vtx not enabled");
        }
        memzero(regionBase, 1 << VTX_VCPU_BITS);
        vcpu = VCPU_PTR((word_t)regionBase);
        vcpu_init(vcpu);
        return cap_vcpu_cap_new(VCPU_REF(vcpu));
    }
    case seL4_IA32_EPTPageDirectoryPointerTableObject: {
        ept_pml4e_t *pml4;
        memzero(regionBase, 1 << (EPT_PDPTE_SIZE_BITS + EPT_PDPT_BITS + 1));
        pml4 = (ept_pml4e_t*)((word_t)regionBase);
        IA32EptPdpt_Init(pml4);
        return cap_ept_page_directory_pointer_table_cap_new(
                   (word_t)regionBase + EPT_PDPT_OFFSET /* capPTBasePtr   */);
    }
    case seL4_IA32_EPTPageDirectoryObject:
        memzero(regionBase, 1 << (EPT_PDE_SIZE_BITS + EPT_PD_BITS));

        return cap_ept_page_directory_cap_new(
                   0,                  /* capPDMappedObject    */
                   0,                  /* capPDMappedIndex     */
                   (word_t)regionBase  /* capPTBasePtr         */
               );
    case seL4_IA32_EPTPageTableObject:
        memzero(regionBase, 1 << (EPT_PTE_SIZE_BITS + EPT_PT_BITS));

        return cap_ept_page_table_cap_new(
                   0,                  /* capPTMappedObject    */
                   0,                  /* capPTMappedIndex     */
                   (word_t)regionBase  /* capPTBasePtr         */
               );
#endif /* VTX */

    default:
        /*
         * This is a conflation of the haskell error: "Arch.createNewCaps
         * got an API type" and the case where an invalid object type is
         * passed (which is impossible in haskell).
         */
        fail("Arch_createObject got an API type or invalid object type");
    }
}

exception_t
Arch_decodeInvocation(
    word_t label,
    word_t length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_pdpt_cap:
    case cap_page_directory_cap:
    case cap_page_table_cap:
    case cap_frame_cap:
        return decodeIA32MMUInvocation(label, length, cptr, slot, cap, extraCaps, buffer);

    case cap_io_port_cap:
        return decodeIA32PortInvocation(label, length, cptr, slot, cap, extraCaps, buffer);
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    case cap_io_page_table_cap:
        return decodeIA32IOPTInvocation(label, length, slot, cap, extraCaps, buffer);
#endif
    case cap_ipi_cap:
        return decodeIA32IPIInvocation(label, length, cptr, slot, cap, extraCaps, buffer);

#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
    case cap_ept_page_directory_cap:
    case cap_ept_page_table_cap:
        return decodeIA32EPTInvocation(label, length, cptr, slot, cap, extraCaps, buffer);
#endif

#ifdef CONFIG_VTX
    case cap_vcpu_cap:
        return decodeIA32VCPUInvocation(label, length, cptr, slot, cap, extraCaps, buffer);
#endif
    default:
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

void
Arch_prepareThreadDelete(tcb_t *thread)
{
    /* Notify the lazy FPU module about this thread's deletion. */
    Arch_fpuThreadDelete(thread);

#ifdef CONFIG_VTX
    if (thread->tcbArch.vcpu) {
        dissociateVcpuTcb(thread, thread->tcbArch.vcpu);
    }
#endif
}
