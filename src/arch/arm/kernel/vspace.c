/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <benchmark.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <kernel/boot.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <kernel/cdt.h>
#include <machine/io.h>
#include <model/statedata.h>
#include <object/cnode.h>
#include <object/untyped.h>
#include <arch/api/invocation.h>
#include <arch/kernel/vspace.h>
#include <arch/linker.h>
#include <arch/object/tcb.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <armv/context_switch.h>

/* ARM uses multiple identical mappings in a page table / page directory to construct
 * large mappings. In both cases it happens to be 16 entries, which can be calculated by
 * looking at the size difference of the mappings, and is done this way to remove magic
 * numbers littering this code and make it clear what is going on */
#define SECTIONS_PER_SUPER_SECTION BIT(ARMSuperSectionBits - ARMSectionBits)
#define PAGES_PER_LARGE_PAGE BIT(ARMLargePageBits - ARMSmallPageBits)

/* helper stuff to avoid fencepost errors when
 * getting the last byte of a PTE or PDE */
#define LAST_BYTE_PTE(PTE,LENGTH) ((word_t)&(PTE)[(LENGTH)-1] + (BIT(PTE_SIZE_BITS)-1))
#define LAST_BYTE_PDE(PDE,LENGTH) ((word_t)&(PDE)[(LENGTH)-1] + (BIT(PDE_SIZE_BITS)-1))

/* need a fake array to get the pointer from the linker script */
extern char arm_vector_table[1];

/* This is the ARM kernel stack. It is accessed from a remapped address. */
char arm_kernel_stack[4096] ALIGN_BSS(4096) VISIBLE;

struct resolve_ret {
    paddr_t frameBase;
    vm_page_size_t frameSize;
    bool_t valid;
};
typedef struct resolve_ret resolve_ret_t;

void doFlush(int label, vptr_t start, vptr_t end, paddr_t pstart);
static pte_t *lookupPTSlot_nofail(pte_t *pt, vptr_t vptr);
static resolve_ret_t resolveVAddr(pde_t *pd, vptr_t vaddr);
static exception_t performPDFlush(int label, pde_t *pd,
                                  vptr_t start, vptr_t end, paddr_t pstart);
static exception_t performPageFlush(int label, pde_t *pd,
                                    vptr_t start, vptr_t end, paddr_t pstart);
static exception_t performPageGetAddress(void *vbase_ptr);
static exception_t decodeARMPageDirectoryInvocation(word_t label,
                                                    unsigned int length, cptr_t cptr, cte_t *cte, cap_t cap,
                                                    extra_caps_t extraCaps, word_t *buffer);
static pde_t PURE loadHWASID(pde_t *pd);

static word_t CONST
APFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMNoAccess:
        return 0;

    case VMKernelOnly:
        return 1;

    case VMReadOnly:
        return 2;

    case VMReadWrite:
        return 3;

    default:
        fail("Invalid VM rights");
    }
}

BOOT_CODE void
map_it_pt_cap(cap_t pt_cap)
{
    pde_t* pd   = PDE_PTR(cap_page_table_cap_get_capPTMappedObject(pt_cap));
    pte_t* pt   = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(pt_cap));
    uint32_t pdIndex = cap_page_table_cap_get_capPTMappedIndex(pt_cap);
    pde_t* targetSlot = pd + pdIndex;

    *targetSlot = pde_pde_coarse_new(
                      addrFromPPtr(pt), /* address */
                      true,             /* P       */
                      0                 /* Domain  */
                  );
}

BOOT_CODE void
map_it_frame_cap(cap_t frame_cap)
{
    pte_t* pt;
    pte_t* targetSlot;
    uint32_t index;
    void*  frame = (void*)cap_frame_cap_get_capFBasePtr(frame_cap);

    pt = PT_PTR(cap_frame_cap_get_capFMappedObject(frame_cap));
    index = cap_frame_cap_get_capFMappedIndex(frame_cap);
    targetSlot = pt + index;
    *targetSlot = pte_pte_small_new(
                      addrFromPPtr(frame),
                      1, /* not global */
                      0, /* not shared */
                      0, /* APX = 0, privileged full access */
                      0, /* TEX = 0 */
                      APFromVMRights(VMReadWrite),
                      1, /* cacheable */
                      1, /* write-back caching */
                      0  /* executable */
                  );
}

BOOT_CODE void
map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights, vm_attributes_t attributes)
{
    uint32_t idx = (vaddr & MASK(pageBitsForSize(ARMSection))) >> pageBitsForSize(ARMSmallPage);

    assert(vaddr >= PPTR_TOP); /* vaddr lies in the region the global PT covers */

    armKSGlobalPT[idx] =
        pte_pte_small_new(
            paddr,
            0, /* global */
            0, /* Not shared */
            0, /* APX = 0, privileged full access */
            0, /* TEX = 0 */
            APFromVMRights(vm_rights),
            vm_attributes_get_armPageCacheable(attributes),
            1, /* Write-back caching */
            0  /* executable */
        );
}

BOOT_CODE void
map_kernel_window(void)
{
    paddr_t  phys;
    uint32_t idx;
    pde_t    pde;

    /* mapping of kernelBase (virtual address) to kernel's physBase  */
    /* up to end of virtual address space minus 16M using 16M frames */
    phys = physBase;
    idx = kernelBase >> pageBitsForSize(ARMSection);

    while (idx < BIT(PD_BITS) - SECTIONS_PER_SUPER_SECTION) {
        uint32_t idx2;

        pde = pde_pde_section_new(
                  phys,
                  1, /* SuperSection */
                  0, /* global */
                  0, /* Not shared */
                  0, /* APX = 0, privileged full access */
                  0, /* TEX = 0 */
                  1, /* VMKernelOnly */
                  1, /* Parity enabled */
                  0, /* Domain 0 */
                  0, /* XN not set */
                  1, /* Cacheable */
                  1  /* Write-back */
              );
        for (idx2 = idx; idx2 < idx + SECTIONS_PER_SUPER_SECTION; idx2++) {
            armKSGlobalPD[idx2] = pde;
        }
        phys += BIT(pageBitsForSize(ARMSuperSection));
        idx += SECTIONS_PER_SUPER_SECTION;
    }
#ifdef CONFIG_BENCHMARK
    /* steal the last MB for logging */
    while (idx < BIT(PD_BITS) - 2) {
#else
    /* mapping of the next 15M using 1M frames */
    while (idx < BIT(PD_BITS) - 1) {
#endif /* CONFIG_BENCHMARK */
        pde = pde_pde_section_new(
                  phys,
                  0, /* Section */
                  0, /* global */
                  0, /* Not shared */
                  0, /* APX = 0, privileged full access */
                  0, /* TEX = 0 */
                  1, /* VMKernelOnly */
                  1, /* Parity enabled */
                  0, /* Domain 0 */
                  0, /* XN not set */
                  1, /* Cacheable */
                  1  /* Write-back */
              );
        armKSGlobalPD[idx] = pde;
        phys += BIT(pageBitsForSize(ARMSection));
        idx++;
    }

#ifdef CONFIG_BENCHMARK
    /* allocate a 1M buffer for logging */
    pde = pde_pde_section_new(
              phys,
              0, /* Section */
              0, /* global */
              0, /* Not Shared */
              0, /* APX = 0, privileged full access */
              0, /* TEX = 0 */
              1, /* VMKernelOnly */
              1, /* Parity Enabled */
              0, /* Domain 0 */
              0, /* XN not set */
              1, /* Cacheable */
              0  /* Write-through to minimise perf hit */
          );
    armKSGlobalPD[idx] = pde;
    ksLog = (word_t *) ptrFromPAddr(phys);

    /* we remove the address PADDR_TOP - 1MB from the
     * available physical memory for the sabre.
     *
     * if you are using a different platform this may need
     * adjusting or you may need to do something completely different
     * to get a 1mb, write through buffer*/
    assert(ksLog == ((word_t *) KS_LOG_PADDR));
    phys += BIT(pageBitsForSize(ARMSection));
    idx++;
#endif /* CONFIG_BENCHMARK */

    /* crosscheck whether we have mapped correctly so far */
    assert(phys == PADDR_TOP);

    /* map page table covering last 1M of virtual address space to page directory */
    armKSGlobalPD[idx] =
        pde_pde_coarse_new(
            addrFromPPtr(armKSGlobalPT), /* address */
            true,                        /* P       */
            0                            /* Domain  */
        );

    /* now start initialising the page table */
    memzero(armKSGlobalPT, 1 << PT_SIZE_BITS);

    /* map vector table */
    map_kernel_frame(
        addrFromPPtr(arm_vector_table),
        PPTR_VECTOR_TABLE,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            true,  /* armParityEnabled */
            true   /* armPageCacheable */
        )
    );

    /* map globals frame */
    map_kernel_frame(
        addrFromPPtr(armKSGlobalsFrame),
        PPTR_GLOBALS_PAGE,
        VMReadOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            true,  /* armParityEnabled */
            true   /* armPageCacheable */
        )
    );

    /* map stack frame */
    map_kernel_frame(
        addrFromPPtr(arm_kernel_stack),
        PPTR_KERNEL_STACK,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            true,  /* armParityEnabled */
            true   /* armPageCacheable */
        )
    );

    map_kernel_devices();
}

BOOT_CODE void
activate_global_pd(void)
{
    /* Ensure that there's nothing stale in newly-mapped regions, and
       that everything we've written (particularly the kernel page tables)
       is committed. */
    cleanInvalidateL1Caches();
    setCurrentPD(addrFromPPtr(armKSGlobalPD));
    invalidateTLB();
    lockTLBEntry(kernelBase);
    lockTLBEntry(PPTR_VECTOR_TABLE);
}

/* ==================== BOOT CODE FINISHES HERE ==================== */

void
copyGlobalMappings(pde_t *newPD)
{
    unsigned int i;
    pde_t *global_pd = armKSGlobalPD;

    for (i = kernelBase >> ARMSectionBits; i < BIT(PD_BITS); i++) {
        if (i != PD_ASID_SLOT) {
            newPD[i] = global_pd[i];
        }
    }
}

word_t * PURE
lookupIPCBuffer(bool_t isReceiver, tcb_t *thread)
{
    word_t w_bufferPtr;
    cap_t bufferCap;
    vm_rights_t vm_rights;

    w_bufferPtr = thread->tcbIPCBuffer;
    bufferCap = TCB_PTR_CTE_PTR(thread, tcbBuffer)->cap;

    if (unlikely(cap_get_capType(bufferCap) != cap_frame_cap)) {
        return NULL;
    }

    vm_rights = cap_frame_cap_get_capFVMRights(bufferCap);
    if (likely(vm_rights == VMReadWrite ||
               (!isReceiver && vm_rights == VMReadOnly))) {
        word_t basePtr;
        unsigned int pageBits;

        basePtr = cap_frame_cap_get_capFBasePtr(bufferCap);
        pageBits = pageBitsForSize(cap_frame_cap_get_capFSize(bufferCap));
        return (word_t *)(basePtr + (w_bufferPtr & MASK(pageBits)));
    } else {
        return NULL;
    }
}

static unsigned int CONST
makePDIndex(vptr_t vptr)
{
    return vptr >> 20;
}

lookupPTSlot_ret_t
lookupPTSlot(pde_t *pd, vptr_t vptr)
{
    lookupPTSlot_ret_t ret;
    pde_t *pdSlot;

    pdSlot = pd + makePDIndex(vptr);

    if (unlikely(pde_ptr_get_pdeType(pdSlot) != pde_pde_coarse)) {
        current_lookup_fault = lookup_fault_missing_capability_new(20);

        ret.pt = NULL;
        ret.ptIndex = 0;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        pte_t *pt;
        unsigned int ptIndex;

        pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pdSlot));
        ptIndex = (vptr >> 12) & 0xff;

        ret.pt = pt;
        ret.ptIndex = ptIndex;
        ret.status = EXCEPTION_NONE;
        return ret;
    }
}

static pte_t *
lookupPTSlot_nofail(pte_t *pt, vptr_t vptr)
{
    unsigned int ptIndex;

    ptIndex = (vptr >> 12) & MASK(8);
    return pt + ptIndex;
}

exception_t
handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType)
{
    switch (vm_faultType) {
    case ARMDataAbort: {
        word_t addr, fault;

        addr = getFAR();
        fault = getDFSR();
        current_fault = fault_vm_fault_new(addr, fault, false);
        return EXCEPTION_FAULT;
    }

    case ARMPrefetchAbort: {
        word_t pc, fault;

        pc = getRestartPC(thread);
        fault = getIFSR();

        current_fault = fault_vm_fault_new(pc, fault, true);
        return EXCEPTION_FAULT;
    }

    default:
        fail("Invalid VM fault type");
    }
}

static void
invalidateASID(pde_t *pd)
{
    pd[PD_ASID_SLOT] = pde_pde_invalid_new(0, false);
}

#if 0

static void
invalidateASIDEntry(pde_t *pd)
{
    pde_t stored_hw_asid;

    stored_hw_asid = loadHWASID(asid);
    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        armKSHWASIDTable[pde_pde_invalid_get_stored_hw_asid(stored_hw_asid)] =
            asidInvalid;
    }
    invalidateASID(asid);
}

#endif

void
unmapPageTable(pde_t *pd, uint32_t pdIndex, pte_t* pt)
{
    assert(pd);
    assert(pt);
    assert(pde_get_pdeType(pd[pdIndex]) == pde_pde_coarse);
    assert(ptrFromPAddr (pde_pde_coarse_get_address(pd[pdIndex])) == pt);

    pd[pdIndex] = pde_pde_invalid_new(0, 0);
    cleanByVA_PoU((word_t)&pd[pdIndex], addrFromPPtr(&pd[pdIndex]));
    flushTable(pd, pdIndex << 20, pt);
}

void
unmapAllPageTables(pde_t *pd)
{
    unsigned int i;

    for (i = 0; i < kernelBase >> ARMSectionBits;) {
        switch (pde_get_pdeType(pd[i])) {
        case pde_pde_coarse: {
            cap_t ptCap;
            cte_t *ptCte;
            ptCte = cdtFind(cap_page_table_cap_new(PD_REF(pd), i, (uint32_t)paddr_to_pptr(pde_pde_coarse_get_address(pd[i]))));
            assert(ptCte);
            ptCap = cap_page_table_cap_set_capPTMappedObject(ptCte->cap, 0);
            cdtUpdate(ptCte, ptCap);
            unmapPageTable(pd, i, PT_PTR(cap_page_table_cap_get_capPTBasePtr(ptCap)));
            i++;
            break;
        }
        case pde_pde_section:
            if (pde_pde_section_get_size(pd[i])) {
                cap_t frameCap;
                cte_t *frameCte;
                frameCte = cdtFind(cap_frame_cap_new(FMAPPED_OBJECT_HIGH(pd), i, ARMSuperSection, 0, FMAPPED_OBJECT_LOW(pd), (uint32_t)paddr_to_pptr(pde_pde_section_get_address(pd[i]))));
                assert(frameCte);
                frameCap = cap_frame_cap_set_capFMappedObject(frameCte->cap, 0);
                cdtUpdate(frameCte, frameCap);
                unmapPagePDE(ARMSuperSection, pd, i, (void *)cap_frame_cap_get_capFBasePtr(frameCap));
                i++;
            } else {
                cap_t frameCap;
                cte_t *frameCte;
                frameCte = cdtFind(cap_frame_cap_new(FMAPPED_OBJECT_HIGH(pd), i, ARMSection, 0, FMAPPED_OBJECT_LOW(pd), (uint32_t)paddr_to_pptr(pde_pde_section_get_address(pd[i]))));
                assert(frameCte);
                frameCap = cap_frame_cap_set_capFMappedObject(frameCte->cap, 0);
                cdtUpdate(frameCte, frameCap);
                unmapPagePDE(ARMSection, pd, i, (void *)cap_frame_cap_get_capFBasePtr(frameCap));
                i++;
            }
            break;
        case pde_pde_invalid:
            i++;
            break;
        }
    }
}

static pte_t pte_pte_invalid_new(void)
{
    /* Invalid as every PTE must have bit 0 set (large PTE) or bit 1 set (small
     * PTE). 0 == 'translation fault' in ARM parlance.
     */
    return (pte_t) {
        {
            0
        }
    };
}

void unmapPagePTE(vm_page_size_t page_size, pte_t *pt, unsigned int ptIndex, void *pptr)
{
    paddr_t addr = addrFromPPtr(pptr);
    cte_t *ptCte;
    pde_t *pd;
    unsigned int pdIndex;

    (void)addr;

    ptCte = cdtFindWithExtra(cap_page_table_cap_new(0, 0, PT_REF(pt)));
    assert(ptCte);
    pd = PD_PTR(cap_page_table_cap_get_capPTMappedObject(ptCte->cap));
    pdIndex = cap_page_table_cap_get_capPTMappedIndex(ptCte->cap);

    switch (page_size) {
    case ARMSmallPage: {
        /* When recycling a cap the finaliseCap function gets called twice. Unfortunately
         * there is no way to distinguish when this is going to be a duplicate call, vs
         * a single call from delete. Therefore we need to handle attempting to remove
         * a mapping that has already been removed, so just silently succeed */
        if (pt[ptIndex].words[0] == pte_pte_invalid_new().words[0]) {
            return;
        }
        assert(pte_get_pteType(pt[ptIndex]) == pte_pte_small);
        assert(pte_pte_small_get_address(pt[ptIndex]) == addr);

        pt[ptIndex] = pte_pte_invalid_new();
        cleanByVA_PoU((word_t)&pt[ptIndex], addrFromPPtr(&pt[ptIndex]));

        break;
    }

    case ARMLargePage: {
        unsigned int i;

        /* When recycling a cap the finaliseCap function gets called twice. Unfortunately
         * there is no way to distinguish when this is going to be a duplicate call, vs
         * a single call from delete. Therefore we need to handle attempting to remove
         * a mapping that has already been removed, so just silently succeed */
        if (pt[ptIndex].words[0] == pte_pte_invalid_new().words[0]) {
            return;
        }
        assert(pte_get_pteType(pt[ptIndex]) == pte_pte_large);
        assert(pte_pte_large_get_address(pt[ptIndex]) == addr);

        for (i = 0; i < PAGES_PER_LARGE_PAGE; i++) {
            pt[ptIndex + i] = pte_pte_invalid_new();
        }
        cleanCacheRange_PoU((word_t)&pt[ptIndex],
                            LAST_BYTE_PTE(&pt[ptIndex], PAGES_PER_LARGE_PAGE),
                            addrFromPPtr(&pt[ptIndex]));

        break;
    }
    default:
        fail("Invalid page size");
    }

    if (pd) {
        flushPage(page_size, pd, (pdIndex << 20) |  (ptIndex << 12));
    }
}
void
unmapPagePDE(vm_page_size_t page_size, pde_t *pd, unsigned int pdIndex, void *pptr)
{
    paddr_t addr = addrFromPPtr(pptr);

    (void)addr;

    switch (page_size) {
    case ARMSection: {

        /* When recycling a cap the finaliseCap function gets called twice. Unfortunately
         * there is no way to distinguish when this is going to be a duplicate call, vs
         * a single call from delete. Therefore we need to handle attempting to remove
         * a mapping that has already been removed, so just silently succeed */
        if (pd[pdIndex].words[0] == pde_pde_invalid_new(0, 0).words[0]) {
            return;
        }

        assert(pde_get_pdeType(pd[pdIndex]) == pde_pde_section);
        assert(pde_pde_section_get_size(pd[pdIndex]) == 0);
        assert(pde_pde_section_get_address(pd[pdIndex]) == addr);

        pd[pdIndex] = pde_pde_invalid_new(0, 0);
        cleanByVA_PoU((word_t)&pd[pdIndex], addrFromPPtr(&pd[pdIndex]));

        break;
    }

    case ARMSuperSection: {
        unsigned int i;

        /* When recycling a cap the finaliseCap function gets called twice. Unfortunately
         * there is no way to distinguish when this is going to be a duplicate call, vs
         * a single call from delete. Therefore we need to handle attempting to remove
         * a mapping that has already been removed, so just silently succeed */
        if (pd[pdIndex].words[0] == pde_pde_invalid_new(0, 0).words[0]) {
            return;
        }

        assert(pde_get_pdeType(pd[pdIndex]) == pde_pde_section);
        assert(pde_pde_section_get_size(pd[pdIndex]) == 1);
        assert(pde_pde_section_get_address(pd[pdIndex]) == addr);

        for (i = 0; i < SECTIONS_PER_SUPER_SECTION; i++) {
            pd[pdIndex + i] = pde_pde_invalid_new(0, 0);
        }
        cleanCacheRange_PoU((word_t)&pd[pdIndex], LAST_BYTE_PDE(&pd[pdIndex], SECTIONS_PER_SUPER_SECTION),
                            addrFromPPtr(&pd[pdIndex]));

        break;
    }

    default:
        fail("Invalid ARM page type");
        break;
    }

    /* Flush the page now that the mapping has been updated */
    flushPage(page_size, pd, pdIndex << 20);
}

void unmapAllPages(pde_t *pd, uint32_t pdIndex, pte_t *pt)
{
    uint32_t i;
    for (i = 0; i < BIT(PT_BITS);) {
        switch (pte_get_pteType(pt[i])) {
        case pte_pte_small: {
            cte_t *frameCte;
            cap_t frameCap;
            frameCte = cdtFind(cap_frame_cap_new(FMAPPED_OBJECT_HIGH(pt), i, ARMSmallPage, 0, FMAPPED_OBJECT_LOW(pt), (uint32_t)paddr_to_pptr(pte_pte_small_ptr_get_address(pt + i))));
            assert(frameCte);
            frameCap = cap_frame_cap_set_capFMappedObject(frameCte->cap, 0);
            cdtUpdate(frameCte, frameCap);
            unmapPagePTE(ARMSmallPage, pt, i, (void *)cap_frame_cap_get_capFBasePtr(frameCap));
            i++;
            break;
        }
        case pte_pte_large: {
            cte_t *frameCte;
            cap_t frameCap;
            frameCte = cdtFind(cap_frame_cap_new(FMAPPED_OBJECT_HIGH(pt), i, ARMLargePage, 0, FMAPPED_OBJECT_LOW(pt), (uint32_t)paddr_to_pptr(pte_pte_large_ptr_get_address(pt + i))));
            assert(frameCte);
            frameCap = cap_frame_cap_set_capFMappedObject(frameCte->cap, 0);
            cdtUpdate(frameCte, frameCap);
            unmapPagePTE(ARMLargePage, pt, i, (void *)cap_frame_cap_get_capFBasePtr(frameCap));
            i += 16;
            break;
        }
        case pte_pte_invalid:
            i++;
            break;
        }
    }
}

void
setVMRoot(tcb_t *tcb)
{
    cap_t threadRoot;
    pde_t *pd;

    threadRoot = TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap;

    if (cap_get_capType(threadRoot) != cap_page_directory_cap) {
        setCurrentPD(addrFromPPtr(armKSGlobalPD));
        return;
    }

    pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(threadRoot));

    armv_contextSwitch(pd);
}

static bool_t
setVMRootForFlush(pde_t* pd)
{
    cap_t threadRoot;

    threadRoot = TCB_PTR_CTE_PTR(ksCurThread, tcbVTable)->cap;

    if (cap_get_capType(threadRoot) == cap_page_directory_cap &&
            PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(threadRoot)) == pd) {
        return false;
    }

    armv_contextSwitch(pd);

    return true;
}

bool_t CONST
isValidVTableRoot(cap_t cap)
{
    return cap_get_capType(cap) == cap_page_directory_cap;
}

exception_t
checkValidIPCBuffer(vptr_t vptr, cap_t cap)
{
    if (unlikely(cap_get_capType(cap) != cap_frame_cap)) {
        userError("Requested IPC Buffer is not a frame cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(vptr & MASK(9))) {
        userError("Requested IPC Buffer location 0x%x is not aligned.",
                  (int)vptr);
        current_syscall_error.type = seL4_AlignmentError;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

vm_rights_t CONST
maskVMRights(vm_rights_t vm_rights, cap_rights_t cap_rights_mask)
{
    if (vm_rights == VMNoAccess) {
        return VMNoAccess;
    }
    if (vm_rights == VMReadOnly &&
            cap_rights_get_capAllowRead(cap_rights_mask)) {
        return VMReadOnly;
    }
    if (vm_rights == VMReadWrite &&
            cap_rights_get_capAllowRead(cap_rights_mask)) {
        if (!cap_rights_get_capAllowWrite(cap_rights_mask)) {
            return VMReadOnly;
        } else {
            return VMReadWrite;
        }
    }
    return VMKernelOnly;
}

/* ARM Hardware ASID allocation */

static void
storeHWASID(pde_t *pd, hw_asid_t hw_asid)
{
    /* Store HW ASID in the last entry
       Masquerade as an invalid PDE */
    pd[PD_ASID_SLOT] = pde_pde_invalid_new(hw_asid, true);

    armKSHWASIDTable[hw_asid] = pd;
}

static pde_t PURE
loadHWASID(pde_t *pd)
{
    return pd[PD_ASID_SLOT];
}

hw_asid_t
findFreeHWASID(void)
{
    word_t hw_asid_offset;
    hw_asid_t hw_asid;

    /* Find a free hardware ASID */
    for (hw_asid_offset = 0;
            hw_asid_offset <= (word_t)((hw_asid_t) - 1);
            hw_asid_offset ++) {
        hw_asid = armKSNextASID + ((hw_asid_t)hw_asid_offset);
        if (!armKSHWASIDTable[hw_asid]) {
            return hw_asid;
        }
    }

    hw_asid = armKSNextASID;

    /* If we've scanned the table without finding a free ASID */
    invalidateASID(armKSHWASIDTable[hw_asid]);

    /* Flush TLB */
    invalidateTLB_ASID(hw_asid);
    armKSHWASIDTable[hw_asid] = NULL;

    /* Increment the NextASID index */
    armKSNextASID++;

    return hw_asid;
}

hw_asid_t
getHWASID(pde_t *pd)
{
    pde_t stored_hw_asid;

    stored_hw_asid = loadHWASID(pd);
    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        return pde_pde_invalid_get_stored_hw_asid(stored_hw_asid);
    } else {
        hw_asid_t new_hw_asid;

        new_hw_asid = findFreeHWASID();
        storeHWASID(pd, new_hw_asid);
        return new_hw_asid;
    }
}

/* Cache and TLB consistency */

void
flushPage(vm_page_size_t page_size, pde_t* pd, word_t vptr)
{
    pde_t stored_hw_asid;
    word_t base_addr;
    bool_t root_switched;

    assert((vptr & MASK(pageBitsForSize(page_size))) == 0);

    /* Switch to the address space to allow a cache clean by VA */
    root_switched = setVMRootForFlush(pd);
    stored_hw_asid = loadHWASID(pd);

    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        base_addr = vptr & ~MASK(12);

        /* Do the TLB flush */
        invalidateTLB_VAASID(base_addr | pde_pde_invalid_get_stored_hw_asid(stored_hw_asid));

        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }
}

void
flushTable(pde_t* pd, word_t vptr, pte_t* pt)
{
    pde_t stored_hw_asid;
    bool_t root_switched;

    assert((vptr & MASK(PT_BITS + ARMSmallPageBits)) == 0);

    /* Switch to the address space to allow a cache clean by VA */
    root_switched = setVMRootForFlush(pd);
    stored_hw_asid = loadHWASID(pd);

    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        invalidateTLB_ASID(pde_pde_invalid_get_stored_hw_asid(stored_hw_asid));
        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }
}

/* The rest of the file implements the ARM object invocations */

static pte_t CONST
makeUserPTE(vm_page_size_t page_size, paddr_t paddr,
            bool_t cacheable, bool_t nonexecutable, vm_rights_t vm_rights)
{
    pte_t pte;
    word_t ap;

    ap = APFromVMRights(vm_rights);

    switch (page_size) {
    case ARMSmallPage: {
        if (cacheable) {
            pte = pte_pte_small_new(paddr,
                                    1, /* not global */
                                    0, /* not shared */
                                    0, /* APX = 0, privileged full access */
                                    5, /* TEX = 0b101, outer write-back, write-allocate */
                                    ap,
                                    0, 1, /* Inner write-back, write-allocate (except on ARM11) */
                                    nonexecutable);
        } else {
            pte = pte_pte_small_new(paddr,
                                    1, /* not global */
                                    1, /* shared */
                                    0, /* APX = 0, privileged full access */
                                    0, /* TEX = 0b000, strongly-ordered. */
                                    ap,
                                    0, 0,
                                    nonexecutable);
        }
        break;
    }

    case ARMLargePage: {
        if (cacheable) {
            pte = pte_pte_large_new(paddr,
                                    nonexecutable,
                                    5, /* TEX = 0b101, outer write-back, write-allocate */
                                    1, /* not global */
                                    0, /* not shared */
                                    0, /* APX = 0, privileged full access */
                                    ap,
                                    0, 1, /* Inner write-back, write-allocate (except on ARM11) */
                                    1 /* reserved */);
        } else {
            pte = pte_pte_large_new(paddr,
                                    nonexecutable,
                                    0, /* TEX = 0b000, strongly-ordered */
                                    1, /* not global */
                                    1, /* shared */
                                    0, /* APX = 0, privileged full access */
                                    ap,
                                    0, 0,
                                    1 /* reserved */);
        }
        break;
    }

    default:
        fail("Invalid PTE frame type");
    }

    return pte;
}

static pde_t CONST
makeUserPDE(vm_page_size_t page_size, paddr_t paddr, bool_t parity,
            bool_t cacheable, bool_t nonexecutable, word_t domain,
            vm_rights_t vm_rights)
{
    word_t ap, size2;

    ap = APFromVMRights(vm_rights);

    switch (page_size) {
    case ARMSection:
        size2 = 0;
        break;

    case ARMSuperSection:
        size2 = 1;
        break;

    default:
        fail("Invalid PDE frame type");
    }

    if (cacheable) {
        return pde_pde_section_new(paddr, size2,
                                   1, /* not global */
                                   0, /* not shared */
                                   0, /* APX = 0, privileged full access */
                                   5, /* TEX = 0b101, outer write-back, write-allocate */
                                   ap, parity, domain, nonexecutable,
                                   0, 1 /* Inner write-back, write-allocate (except on ARM11) */);
    } else {
        return pde_pde_section_new(paddr, size2,
                                   1, /* not global */
                                   1, /* shared */
                                   0, /* APX = 0, privileged full access */
                                   0, /* TEX = 0b000, strongly-ordered */
                                   ap, parity, domain, nonexecutable,
                                   0, 0);
    }
}

static inline bool_t CONST
checkVPAlignment(vm_page_size_t sz, word_t w)
{
    return (w & MASK(pageBitsForSize(sz))) == 0;
}

static exception_t
decodeARMPageTableInvocation(word_t label, unsigned int length,
                             cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                             word_t *buffer)
{
    word_t vaddr, pdIndex;
    vm_attributes_t attr;
    cap_t pdCap;
    pde_t *pd, *pdSlot;
    pde_t pde;
    paddr_t paddr;

    if (label == ARMPageTableUnmap) {
        if (unlikely(! cdtIsFinal(cte))) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageTableInvocationUnmap (cap, cte);
    }

    if (unlikely(label != ARMPageTableMap)) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(length < 2 || extraCaps.excaprefs[0] == NULL)) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cdtFindWithExtra(cap))) {
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    attr = vmAttributesFromWord(getSyscallArg(1, buffer));
    pdCap = extraCaps.excaprefs[0]->cap;

    if (unlikely(cap_get_capType(pdCap) != cap_page_directory_cap)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(pdCap));

    if (unlikely(vaddr >= kernelBase)) {
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pdIndex = vaddr >> 20;
    pdSlot = &pd[pdIndex];
    if (unlikely(pde_ptr_get_pdeType(pdSlot) != pde_pde_invalid)) {
        current_syscall_error.type = seL4_DeleteFirst;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = addrFromPPtr(
                PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
    pde = pde_pde_coarse_new(
              paddr,
              vm_attributes_get_armParityEnabled(attr),
              0 /* Domain */
          );

    cap = cap_page_table_cap_set_capPTMappedObject(cap, PD_REF(pd));
    cap = cap_page_table_cap_set_capPTMappedIndex(cap, pdIndex);

    setThreadState(ksCurThread, ThreadState_Restart);
    return performPageTableInvocationMap(cap, cte, pde, pdSlot);
}

struct create_mappings_pte_return {
    exception_t status;
    pte_t pte;
    pte_range_t pte_entries;
};
typedef struct create_mappings_pte_return create_mappings_pte_return_t;

struct create_mappings_pde_return {
    exception_t status;
    pde_t pde;
    pde_range_t pde_entries;
};
typedef struct create_mappings_pde_return create_mappings_pde_return_t;

static create_mappings_pte_return_t
createSafeMappingEntries_PTE
(paddr_t base, word_t vaddr, vm_page_size_t frameSize,
 vm_rights_t vmRights, vm_attributes_t attr, pde_t *pd)
{

    create_mappings_pte_return_t ret;
    lookupPTSlot_ret_t lu_ret;
    unsigned int i;

    switch (frameSize) {

    case ARMSmallPage:

        ret.pte_entries.pt = NULL; /* to avoid uninitialised warning */
        ret.pte_entries.start = 0;
        ret.pte_entries.length = 1;

        ret.pte = makeUserPTE(ARMSmallPage, base,
                              vm_attributes_get_armPageCacheable(attr),
                              vm_attributes_get_armExecuteNever(attr),
                              vmRights);

        lu_ret = lookupPTSlot(pd, vaddr);
        if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
            current_syscall_error.type =
                seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource =
                false;
            ret.status = EXCEPTION_SYSCALL_ERROR;
            /* current_lookup_fault will have been set by
             * lookupPTSlot */
            return ret;
        }

        ret.pte_entries.pt = lu_ret.pt;
        ret.pte_entries.start = lu_ret.ptIndex;

        if (unlikely(pte_get_pteType(ret.pte_entries.pt[ret.pte_entries.start]) !=
                     pte_pte_invalid)) {
            current_syscall_error.type =
                seL4_DeleteFirst;

            ret.status = EXCEPTION_SYSCALL_ERROR;
            return ret;
        }

        ret.status = EXCEPTION_NONE;
        return ret;

    case ARMLargePage:

        ret.pte_entries.pt = NULL; /* to avoid uninitialised warning */
        ret.pte_entries.start = 0;
        ret.pte_entries.length = PAGES_PER_LARGE_PAGE;

        ret.pte = makeUserPTE(ARMLargePage, base,
                              vm_attributes_get_armPageCacheable(attr),
                              vm_attributes_get_armExecuteNever(attr),
                              vmRights);

        lu_ret = lookupPTSlot(pd, vaddr);
        if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
            current_syscall_error.type =
                seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource =
                false;
            ret.status = EXCEPTION_SYSCALL_ERROR;
            /* current_lookup_fault will have been set by
             * lookupPTSlot */
            return ret;
        }

        ret.pte_entries.pt = lu_ret.pt;
        ret.pte_entries.start = lu_ret.ptIndex;

        for (i = 0; i < PAGES_PER_LARGE_PAGE; i++) {
            if (unlikely(pte_get_pteType(ret.pte_entries.pt[ret.pte_entries.start + i]) !=
                         pte_pte_invalid)) {
                current_syscall_error.type =
                    seL4_DeleteFirst;

                ret.status = EXCEPTION_SYSCALL_ERROR;
                return ret;
            }
        }

        ret.status = EXCEPTION_NONE;
        return ret;

    default:
        fail("Invalid or unexpected ARM page type.");

    }
}

static create_mappings_pde_return_t
createSafeMappingEntries_PDE
(paddr_t base, word_t vaddr, vm_page_size_t frameSize,
 vm_rights_t vmRights, vm_attributes_t attr, pde_t *pd)
{

    create_mappings_pde_return_t ret;
    pde_tag_t currentPDEType;
    unsigned int i;

    switch (frameSize) {

        /* PDE mappings */
    case ARMSection:
        ret.pde_entries.pd = pd;
        ret.pde_entries.start = makePDIndex(vaddr);
        ret.pde_entries.length = 1;

        ret.pde = makeUserPDE(ARMSection, base,
                              vm_attributes_get_armParityEnabled(attr),
                              vm_attributes_get_armPageCacheable(attr),
                              vm_attributes_get_armExecuteNever(attr),
                              0,
                              vmRights);

        currentPDEType =
            pde_get_pdeType(ret.pde_entries.pd[ret.pde_entries.start]);
        if (unlikely(currentPDEType != pde_pde_invalid)) {
            current_syscall_error.type =
                seL4_DeleteFirst;
            ret.status = EXCEPTION_SYSCALL_ERROR;

            return ret;
        }

        ret.status = EXCEPTION_NONE;
        return ret;

    case ARMSuperSection:
        ret.pde_entries.pd = pd;
        ret.pde_entries.start = makePDIndex(vaddr);
        ret.pde_entries.length = SECTIONS_PER_SUPER_SECTION;

        ret.pde = makeUserPDE(ARMSuperSection, base,
                              vm_attributes_get_armParityEnabled(attr),
                              vm_attributes_get_armPageCacheable(attr),
                              vm_attributes_get_armExecuteNever(attr),
                              0,
                              vmRights);

        for (i = 0; i < SECTIONS_PER_SUPER_SECTION; i++) {
            currentPDEType =
                pde_get_pdeType(ret.pde_entries.pd[ret.pde_entries.start + i]);
            if (unlikely(currentPDEType != pde_pde_invalid)) {
                current_syscall_error.type =
                    seL4_DeleteFirst;
                ret.status = EXCEPTION_SYSCALL_ERROR;

                return ret;
            }
        }

        ret.status = EXCEPTION_NONE;
        return ret;

    default:
        fail("Invalid or unexpected ARM page type.");

    }
}

static exception_t
decodeARMFrameInvocation(word_t label, unsigned int length,
                         cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                         word_t *buffer)
{
    switch (label) {
    case ARMPageMap: {
        word_t vaddr, vtop, w_rightsMask;
        paddr_t capFBasePtr;
        cap_t pdCap;
        pde_t *pd;
        vm_rights_t capVMRights, vmRights;
        vm_page_size_t frameSize;
        vm_attributes_t attr;

        if (unlikely(length < 3 || extraCaps.excaprefs[0] == NULL)) {
            current_syscall_error.type =
                seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vaddr = getSyscallArg(0, buffer);
        w_rightsMask = getSyscallArg(1, buffer);
        attr = vmAttributesFromWord(getSyscallArg(2, buffer));
        pdCap = extraCaps.excaprefs[0]->cap;

        frameSize = cap_frame_cap_get_capFSize(cap);
        capVMRights = cap_frame_cap_get_capFVMRights(cap);

        if (unlikely(cap_frame_cap_get_capFMappedObject(cap))) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(cap_get_capType(pdCap) != cap_page_directory_cap)) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
        pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(
                         pdCap));

        vtop = vaddr + BIT(pageBitsForSize(frameSize)) - 1;

        if (unlikely(vtop >= kernelBase)) {
            current_syscall_error.type =
                seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vmRights =
            maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

        if (unlikely(!checkVPAlignment(frameSize, vaddr))) {
            current_syscall_error.type =
                seL4_AlignmentError;

            return EXCEPTION_SYSCALL_ERROR;
        }

        capFBasePtr = addrFromPPtr((void *)
                                   cap_frame_cap_get_capFBasePtr(cap));

        if (frameSize == ARMSmallPage || frameSize == ARMLargePage) {
            create_mappings_pte_return_t map_ret;
            map_ret = createSafeMappingEntries_PTE(capFBasePtr, vaddr,
                                                   frameSize, vmRights,
                                                   attr, pd);
            if (unlikely(map_ret.status != EXCEPTION_NONE)) {
                return map_ret.status;
            }

            setThreadState(ksCurThread, ThreadState_Restart);
            return performPageInvocationMapPTE(cap, cte,
                                               map_ret.pte,
                                               map_ret.pte_entries);
        } else {
            create_mappings_pde_return_t map_ret;
            map_ret = createSafeMappingEntries_PDE(capFBasePtr, vaddr,
                                                   frameSize, vmRights,
                                                   attr, pd);
            if (unlikely(map_ret.status != EXCEPTION_NONE)) {
                return map_ret.status;
            }

            setThreadState(ksCurThread, ThreadState_Restart);
            return performPageInvocationMapPDE(cap, cte,
                                               map_ret.pde,
                                               map_ret.pde_entries);
        }
    }

    case ARMPageUnmap: {
        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageInvocationUnmap(cap, cte);
    }

    case ARMPageClean_Data:
    case ARMPageInvalidate_Data:
    case ARMPageCleanInvalidate_Data:
    case ARMPageUnify_Instruction: {
        vptr_t vaddr;
        vptr_t start, end;
        paddr_t pstart;
        word_t page_size;
        word_t page_base;
        vm_page_size_t frameSize;
        pte_t *pt;
        unsigned int ptIndex;
        unsigned int pdIndex;
        pde_t *pd;
        cte_t *ptCte;

        if (length < 2) {
            userError("Page Flush: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        frameSize = cap_frame_cap_get_capFSize(cap);

        if (frameSize == ARMSmallPage || frameSize == ARMLargePage) {
            pt = PT_PTR(cap_frame_cap_get_capFMappedObject(cap));
            if (unlikely(!pt)) {
                userError("Page Flush: Frame is not mapped.");
                current_syscall_error.type = seL4_IllegalOperation;
                return EXCEPTION_SYSCALL_ERROR;
            }
            ptIndex = cap_frame_cap_get_capFMappedIndex(cap);

            ptCte = cdtFindWithExtra(cap_page_table_cap_new(0, 0, PT_REF(pt)));
            assert(ptCte);
            pd = PD_PTR(cap_page_table_cap_get_capPTMappedObject(ptCte->cap));
            if (unlikely(!pd)) {
                userError("Page Flush: Page Table is not mapped.");
                current_syscall_error.type = seL4_IllegalOperation;
                return EXCEPTION_SYSCALL_ERROR;
            }

            pdIndex = cap_page_table_cap_get_capPTMappedIndex(ptCte->cap);

            vaddr = (pdIndex << 20) |  (ptIndex << 12);
        } else {
            pd = PD_PTR(cap_frame_cap_get_capFMappedObject(cap));
            if (unlikely(!pd)) {
                userError("Page Flush: Frame is not mapped.");
                current_syscall_error.type = seL4_IllegalOperation;
                return EXCEPTION_SYSCALL_ERROR;
            }
            pdIndex = cap_frame_cap_get_capFMappedIndex(cap);

            vaddr = (pdIndex << 20);
        }

        /* start and end are currently relative inside this page */
        start = getSyscallArg(0, buffer);
        end =   getSyscallArg(1, buffer);

        /* check that the range is sane */
        if (end <= start) {
            userError("PageFlush: Invalid range");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        page_size = 1 << pageBitsForSize(frameSize);
        page_base = addrFromPPtr((void*)cap_frame_cap_get_capFBasePtr(cap));

        if (start >= page_size || end > page_size) {
            userError("Page Flush: Requested range not inside page");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* turn start and end into absolute addresses */
        pstart = page_base + start;
        start += vaddr;
        end += vaddr;

        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageFlush(label, pd, start, end - 1, pstart);
    }

    case ARMPageGetAddress: {


        /* Check that there are enough message registers */
        assert(n_msgRegisters >= 1);

        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageGetAddress((void*)cap_frame_cap_get_capFBasePtr(cap));
    }

    default:
        current_syscall_error.type = seL4_IllegalOperation;

        return EXCEPTION_SYSCALL_ERROR;
    }
}

static const resolve_ret_t default_resolve_ret_t;

static resolve_ret_t
resolveVAddr(pde_t *pd, vptr_t vaddr)
{
    pde_t *pde = pd + (vaddr >> 20);
    resolve_ret_t ret = default_resolve_ret_t;

    ret.valid = true;

    switch (pde_ptr_get_pdeType(pde)) {
    case pde_pde_section:
        ret.frameBase = pde_pde_section_ptr_get_address(pde);
        if (pde_pde_section_ptr_get_size(pde)) {
            ret.frameSize = ARMSuperSection;
        } else {
            ret.frameSize = ARMSection;
        }
        return ret;

    case pde_pde_coarse: {
        pte_t *pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pde));
        pte_t *pte = lookupPTSlot_nofail(pt, vaddr);

        switch (pte_ptr_get_pteType(pte)) {
        case pte_pte_large:
            ret.frameBase = pte_pte_large_ptr_get_address(pte);
            ret.frameSize = ARMLargePage;
            return ret;

        case pte_pte_small:
            ret.frameBase = pte_pte_small_ptr_get_address(pte);
            ret.frameSize = ARMSmallPage;
            return ret;
        }
        break;
    }
    }

    ret.valid = false;
    return ret;
}

static inline vptr_t
pageBase(vptr_t vaddr, vm_page_size_t size)
{
    return vaddr & ~MASK(pageBitsForSize(size));
}

static exception_t
decodeARMPageDirectoryInvocation(word_t label, unsigned int length,
                                 cptr_t cptr, cte_t *cte, cap_t cap,
                                 extra_caps_t extraCaps, word_t *buffer)
{
    switch (label) {
    case ARMPDClean_Data:
    case ARMPDInvalidate_Data:
    case ARMPDCleanInvalidate_Data:
    case ARMPDUnify_Instruction: {
        vptr_t start, end;
        paddr_t pstart;
        pde_t *pd;
        resolve_ret_t resolve_ret;

        if (length < 2) {
            userError("PD Flush: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        start = getSyscallArg(0, buffer);
        end =   getSyscallArg(1, buffer);

        /* Check sanity of arguments */
        if (end <= start) {
            userError("PD Flush: Invalid range");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Don't let applications flush kernel regions. */
        if (start >= kernelBase || end > kernelBase) {
            userError("PD Flush: Overlaps kernel region.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

        /* Look up the frame containing 'start'. */
        resolve_ret = resolveVAddr(pd, start);

        /* Check that there's actually something there. */
        if (!resolve_ret.valid) {
            /* Fail silently, as there can't be any stale cached data (for the
             * given address space), and getting a syscall error because the
             * relevant page is non-resident would be 'astonishing'. */
            setThreadState(ksCurThread, ThreadState_Restart);
            return EXCEPTION_NONE;
        }

        /* Refuse to cross a page boundary. */
        if (pageBase(start, resolve_ret.frameSize) !=
                pageBase(end - 1, resolve_ret.frameSize)) {
            current_syscall_error.type = seL4_RangeError;
            current_syscall_error.rangeErrorMin = start;
            current_syscall_error.rangeErrorMax =
                pageBase(start, resolve_ret.frameSize) +
                MASK(pageBitsForSize(resolve_ret.frameSize));
            return EXCEPTION_SYSCALL_ERROR;
        }


        /* Calculate the physical start address. */
        pstart = resolve_ret.frameBase
                 + (start & MASK(pageBitsForSize(resolve_ret.frameSize)));


        setThreadState(ksCurThread, ThreadState_Restart);
        return performPDFlush(label, pd, start, end - 1, pstart);
    }

    default:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

}

exception_t
decodeARMMMUInvocation(word_t label, unsigned int length, cptr_t cptr,
                       cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                       word_t *buffer)
{
    switch (cap_get_capType(cap)) {
    case cap_page_directory_cap:
        return decodeARMPageDirectoryInvocation(label, length, cptr, cte,
                                                cap, extraCaps, buffer);

    case cap_page_table_cap:
        return decodeARMPageTableInvocation (label, length, cte,
                                             cap, extraCaps, buffer);

    case cap_frame_cap:
        return decodeARMFrameInvocation (label, length, cte,
                                         cap, extraCaps, buffer);
    default:
        fail("Invalid ARM arch cap type");
    }
}

exception_t
performPageTableInvocationMap(cap_t cap, cte_t *ctSlot,
                              pde_t pde, pde_t *pdSlot)
{
    cdtUpdate(ctSlot, cap);
    *pdSlot = pde;
    cleanByVA_PoU((word_t)pdSlot, addrFromPPtr(pdSlot));

    return EXCEPTION_NONE;
}

exception_t
performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_page_table_cap_get_capPTMappedObject(cap)) {
        unmapPageTable(
            PD_PTR(cap_page_table_cap_get_capPTMappedObject(cap)),
            cap_page_table_cap_get_capPTMappedIndex(cap),
            PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
    }
    cdtUpdate(ctSlot, cap_page_table_cap_set_capPTMappedObject(cap, 0));

    return EXCEPTION_NONE;
}

static exception_t
performPageGetAddress(void *vbase_ptr)
{
    paddr_t capFBasePtr;

    /* Get the physical address of this frame. */
    capFBasePtr = addrFromPPtr(vbase_ptr);

    /* return it in the first message register */
    setRegister(ksCurThread, msgRegisters[0], capFBasePtr);
    setRegister(ksCurThread, msgInfoRegister,
                wordFromMessageInfo(message_info_new(0, 0, 0, 1)));

    return EXCEPTION_NONE;
}

exception_t
performPageInvocationMapPTE(cap_t cap, cte_t *ctSlot, pte_t pte,
                            pte_range_t pte_entries)
{
    unsigned int i, j UNUSED;

    cap = cap_frame_cap_set_capFMappedObject(cap, PT_REF(pte_entries.pt));
    cap = cap_frame_cap_set_capFMappedIndex(cap, pte_entries.start);
    cdtUpdate(ctSlot, cap);

    j = pte_entries.length;
    /** GHOSTUPD: "(\<acute>j <= 16, id)" */

    j = pte_entries.length;
    /** GHOSTUPD: "(\<acute>j <= 16, id)" */

    for (i = 0; i < pte_entries.length; i++) {
        pte_entries.pt[pte_entries.start + i] = pte;
    }
    cleanCacheRange_PoU((word_t)(pte_entries.pt + pte_entries.start),
                        ((word_t)LAST_BYTE_PTE(pte_entries.pt + pte_entries.start, pte_entries.length)),
                        addrFromPPtr(pte_entries.pt + pte_entries.start));

    return EXCEPTION_NONE;
}

exception_t
performPageInvocationMapPDE(cap_t cap, cte_t *ctSlot, pde_t pde,
                            pde_range_t pde_entries)
{
    unsigned int i;

    cap = cap_frame_cap_set_capFMappedObject(cap, PD_REF(pde_entries.pd));
    cap = cap_frame_cap_set_capFMappedIndex(cap, pde_entries.start);
    cdtUpdate(ctSlot, cap);

    for (i = 0; i < pde_entries.length; i++) {
        pde_entries.pd[pde_entries.start + i] = pde;
    }
    cleanCacheRange_PoU((word_t)&pde_entries.pd[pde_entries.start],
                        ((word_t)LAST_BYTE_PDE(pde_entries.pd + pde_entries.start, pde_entries.length)),
                        addrFromPPtr(&pde_entries.pd[pde_entries.start]));

    return EXCEPTION_NONE;
}

exception_t
performPageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_frame_cap_get_capFMappedObject(cap)) {
        switch (cap_frame_cap_get_capFSize(cap)) {
        case ARMSmallPage:
        case ARMLargePage:
            unmapPagePTE(cap_frame_cap_get_capFSize(cap),
                         PT_PTR(cap_frame_cap_get_capFMappedObject(cap)),
                         cap_frame_cap_get_capFMappedIndex(cap),
                         (void *)cap_frame_cap_get_capFBasePtr(cap));
            break;
        case ARMSection:
        case ARMSuperSection:
            unmapPagePDE(cap_frame_cap_get_capFSize(cap),
                         PD_PTR(cap_frame_cap_get_capFMappedObject(cap)),
                         cap_frame_cap_get_capFMappedIndex(cap),
                         (void *)cap_frame_cap_get_capFBasePtr(cap));
            break;
        default:
            fail("Invalid ARM page type");
            break;
        }
    }

    cdtUpdate(ctSlot, cap_frame_cap_set_capFMappedObject(cap, 0));

    return EXCEPTION_NONE;
}

void
doFlush(int label, vptr_t start, vptr_t end, paddr_t pstart)
{
    /** GHOSTUPD: "((gs_get_assn cap_get_capSizeBits_'proc \<acute>ghost'state = 0
            \<or> \<acute>end - \<acute>start <= gs_get_assn cap_get_capSizeBits_'proc \<acute>ghost'state)
        \<and> \<acute>start <= \<acute>end, id)" */

    switch (label) {
    case ARMPDClean_Data:
    case ARMPageClean_Data:
        cleanCacheRange_RAM(start, end, pstart);
        break;
    case ARMPDInvalidate_Data:
    case ARMPageInvalidate_Data:
        invalidateCacheRange_RAM(start, end, pstart);
        break;
    case ARMPDCleanInvalidate_Data:
    case ARMPageCleanInvalidate_Data:
        cleanInvalidateCacheRange_RAM(start, end, pstart);
        break;
    case ARMPDUnify_Instruction:
    case ARMPageUnify_Instruction:
        /* First clean data lines to point of unification
           (L2 cache)... */
        cleanCacheRange_PoU(start, end, pstart);
        /* Ensure it's been written. */
        dsb();
        /* ...then invalidate the corresponding instruction lines
           to point of unification... */
        invalidateCacheRange_I(start, end, pstart);
        /* ...then invalidate branch predictors. */
        branchFlushRange(start, end, pstart);
        /* Ensure new instructions come from fresh cache lines. */
        isb();
        break;
    default:
        fail("Invalid operation, shouldn't get here.\n");
    }
}

static exception_t
performPageFlush(int label, pde_t *pd, vptr_t start,
                 vptr_t end, paddr_t pstart)
{
    bool_t root_switched;

    /* now we can flush. But only if we were given a non zero range */
    if (start < end) {
        root_switched = setVMRootForFlush(pd);

        doFlush(label, start, end, pstart);

        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }

    return EXCEPTION_NONE;
}

static exception_t
performPDFlush(int label, pde_t *pd, vptr_t start,
               vptr_t end, paddr_t pstart)
{
    bool_t root_switched;

    /* Flush if given a non zero range */
    if (start < end) {
        root_switched = setVMRootForFlush(pd);

        doFlush(label, start, end, pstart);

        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }

    return EXCEPTION_NONE;
}

#ifdef DEBUG
void kernelPrefetchAbort(word_t pc) VISIBLE;
void kernelDataAbort(word_t pc) VISIBLE;

void
kernelPrefetchAbort(word_t pc)
{
    word_t ifsr = getIFSR();

    printf("\n\nKERNEL PREFETCH ABORT!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("IFSR: 0x%x\n", (unsigned int)ifsr);

    halt();
}

void
kernelDataAbort(word_t pc)
{
    word_t dfsr = getDFSR();
    word_t far = getFAR();

    printf("\n\nKERNEL DATA ABORT!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("FAR: 0x%x DFSR: 0x%x\n", (unsigned int)far, (unsigned int)dfsr);

    halt();
}
#endif
