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
#include <benchmark.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <kernel/boot.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
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
#include <arch/object/iospace.h>
#include <arch/object/vcpu.h>

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
#include <benchmark_track.h>
#endif

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

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
/* Stage 2 */
#define MEMATTR_CACHEABLE    0xf /* Inner and Outer write-back */
#define MEMATTR_NONCACHEABLE 0x0 /* Strongly ordered or device memory */

/* STage 1 hyp */
#define ATTRINDX_CACHEABLE    0xff /* Inner and Outer RW write-back non-transient */
#define ATTRINDX_NONCACHEABLE 0x0  /* strongly ordered or device memory */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */


/* This is the ARM kernel stack. It is accessed from a remapped address. */
char arm_kernel_stack[4096] ALIGN_BSS(4096) VISIBLE;

struct resolve_ret {
    paddr_t frameBase;
    vm_page_size_t frameSize;
    bool_t valid;
};
typedef struct resolve_ret resolve_ret_t;

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
static bool_t PURE pteCheckIfMapped(pte_t *pte);
static bool_t PURE pdeCheckIfMapped(pde_t *pde);

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

#else
/* AP encoding slightly different. AP only used for kernel mappings which are fixed after boot time */
BOOT_CODE
static word_t CONST
APFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMKernelOnly:
        return 0;
    case VMReadWrite:
        return 1;
    case VMNoAccess:
        /* RO at PL1 only */
        return 2;
    case VMReadOnly:
        return 3;
    default:
        fail("Invalid VM rights");
    }
}

static word_t CONST
HAPFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMKernelOnly:
    case VMNoAccess:
        return 0;
    case VMReadOnly:
        return 1;
        /*
        case VMWriteOnly:
            return 2;
        */
    case VMReadWrite:
        return 3;
    default:
        fail("Invalid VM rights");
    }
}

#endif

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

/* ==================== BOOT CODE STARTS HERE ==================== */

BOOT_CODE void
map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights, vm_attributes_t attributes)
{
    word_t idx = (vaddr & MASK(pageBitsForSize(ARMSection))) >> pageBitsForSize(ARMSmallPage);

    assert(vaddr >= PPTR_TOP); /* vaddr lies in the region the global PT covers */
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
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
#else
    armHSGlobalPT[idx] =
        pteS1_pteS1_small_new(
            0, /* Executeable */
            0, /* Executeable at PL1 */
            0, /* Not contiguous */
            paddr,
            0, /* global */
            1, /* AF -- always set */
            0, /* Not shared */
            APFromVMRights(vm_rights),
            0, /* non secure */
            vm_attributes_get_armPageCacheable(attributes)
        );
#endif
}

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
BOOT_CODE void
map_kernel_window(void)
{
    paddr_t  phys;
    word_t idx;
    pde_t    pde;

    /* mapping of kernelBase (virtual address) to kernel's physBase  */
    /* up to end of virtual address space minus 16M using 16M frames */
    phys = physBase;
    idx = kernelBase >> pageBitsForSize(ARMSection);

    while (idx < BIT(PD_BITS) - SECTIONS_PER_SUPER_SECTION) {
        word_t idx2;

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
#ifdef CONFIG_ENABLE_BENCHMARKS
    /* steal the last MB for logging */
    while (idx < BIT(PD_BITS) - 2) {
#else
    /* mapping of the next 15M using 1M frames */
    while (idx < BIT(PD_BITS) - 1) {
#endif /* CONFIG_ENABLE_BNECHMARKS */
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

#ifdef CONFIG_ENABLE_BENCHMARKS
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
    ksLog = (void *) ptrFromPAddr(phys);

    /* we remove the address PADDR_TOP - 1MB from the
     * available physical memory for the sabre.
     *
     * if you are using a different platform this may need
     * adjusting or you may need to do something completely different
     * to get a 1mb, write through buffer*/
    assert(ksLog == ((void *) KS_LOG_PADDR));
    phys += BIT(pageBitsForSize(ARMSection));
    idx++;
#endif /* CONFIG_ENABLE_BENCHMARKS */

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
    memzero(armKSGlobalPT, 1 << seL4_PageTableBits);

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
        seL4_GlobalsFrame,
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

#else

BOOT_CODE void
map_kernel_window(void)
{
    paddr_t    phys;
    uint32_t   idx;
    pdeS1_t pde;
    pte_t pteS2;

    /* Initialise PGD */
    for (idx = 0; idx < 3; idx++) {
        pde = pdeS1_pdeS1_invalid_new();
        armHSGlobalPGD[idx] = pde;
    }
    pde = pdeS1_pdeS1_coarse_new(0, 0, 0, 0, addrFromPPtr(armHSGlobalPD));
    armHSGlobalPGD[3] = pde;

    /* Initialise PMD */
    /* Invalidate up until kernelBase */
    for (idx = 0; idx < (kernelBase - 0xC0000000) >> (PT_BITS + PAGE_BITS); idx++) {
        pde = pdeS1_pdeS1_invalid_new();
        armHSGlobalPD[idx] = pde;
    }
    /* mapping of kernelBase (virtual address) to kernel's physBase  */
    /* up to end of virtual address space minus 2M using 2M frames */
    phys = physBase;
    for (; idx < BIT(PT_BITS) - 1; idx++) {
        pde = pdeS1_pdeS1_section_new(
                  0, /* Executable */
                  0, /* Executable in PL1 */
                  0, /* Not contiguous */
                  phys, /* Address */
                  0, /* global */
                  1, /* AF -- always set to 1 */
                  0, /* Not Shareable */
                  0, /* AP: WR at PL1 only */
                  0, /* Not secure */
                  1  /* outer write-back Cacheable */
              );
        armHSGlobalPD[idx] = pde;
        phys += BIT(PT_BITS + PAGE_BITS);
    }
    /* map page table covering last 2M of virtual address space */
    pde = pdeS1_pdeS1_coarse_new(0, 0, 0, 0, addrFromPPtr(armHSGlobalPT));
    armHSGlobalPD[idx] = pde;

    /* now start initialising the page table */
    memzero(armHSGlobalPT, 1 << seL4_PageTableBits);
    for (idx = 0; idx < 256; idx++) {
        pteS1_t pte;
        pte = pteS1_pteS1_small_new(
                  0, /* Executable */
                  0, /* Executable in PL1 */
                  0, /* Not contiguous */
                  phys, /* Address */
                  0, /* global */
                  1, /* AF -- always set to 1 */
                  0, /* Not Shareable */
                  0, /* AP: WR at PL1 only */
                  0, /* Not secure */
                  1  /* outer write-back Cacheable */
              );
        armHSGlobalPT[idx] = pte;
        phys += BIT(PAGE_BITS);
    }
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
        seL4_GlobalsFrame,
        VMReadOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            true,  /* armParityEnabled */
            true   /* armPageCacheable */
        )
    );
    /* map globals into user global PT */
    pteS2 = pte_pte_small_new(
                1, /* Not Executeable */
                0, /* Not contiguous */
                addrFromPPtr(armKSGlobalsFrame),
                1, /* AF -- always set */
                0, /* Not shared */
                HAPFromVMRights(VMReadOnly),
                MEMATTR_CACHEABLE  /* Cacheable */
            );
    memzero(armUSGlobalPT, 1 << seL4_PageTableBits);
    idx = (seL4_GlobalsFrame >> PAGE_BITS) & (MASK(PT_BITS));
    armUSGlobalPT[idx] = pteS2;

    /* map stack frame */
    map_kernel_frame(
        addrFromPPtr(arm_kernel_stack),
        PPTR_KERNEL_STACK,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            true,  /* armParityEnabled */
            true   /* armPageCacheable */
        )
    );

    map_kernel_devices();
}

#endif

static BOOT_CODE void
map_it_frame_cap(cap_t pd_cap, cap_t frame_cap, bool_t executable)
{
    pte_t* pt;
    pte_t* targetSlot;
    pde_t* pd    = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(pd_cap));
    void*  frame = (void*)generic_frame_cap_get_capFBasePtr(frame_cap);
    vptr_t vptr  = generic_frame_cap_get_capFMappedAddress(frame_cap);

    assert(generic_frame_cap_get_capFMappedASID(frame_cap) != 0);

    pd += (vptr >> pageBitsForSize(ARMSection));
    pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pd));
    targetSlot = pt + ((vptr & MASK(pageBitsForSize(ARMSection)))
                       >> pageBitsForSize(ARMSmallPage));
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    *targetSlot = pte_pte_small_new(
                      addrFromPPtr(frame),
                      1, /* not global */
                      0, /* not shared */
                      0, /* APX = 0, privileged full access */
                      0, /* TEX = 0 */
                      APFromVMRights(VMReadWrite),
                      1, /* cacheable */
                      1, /* write-back caching */
                      !executable
                  );
#else
    *targetSlot = pte_pte_small_new(
                      0, /* Executeable */
                      0, /* Not contiguous */
                      addrFromPPtr(frame),
                      1, /* AF -- always set */
                      0, /* Not shared */
                      HAPFromVMRights(VMReadWrite),
                      MEMATTR_CACHEABLE  /* Cacheable */
                  );
#endif
}

/* Create a frame cap for the initial thread. */

static BOOT_CODE cap_t
create_it_frame_cap(pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large)
{
    if (use_large)
        return
            cap_frame_cap_new(
                ARMSection,                    /* capFSize           */
                ASID_LOW(asid),                /* capFMappedASIDLow  */
                wordFromVMRights(VMReadWrite), /* capFVMRights       */
                vptr,                          /* capFMappedAddress  */
                ASID_HIGH(asid),               /* capFMappedASIDHigh */
                pptr                           /* capFBasePtr        */
            );
    else
        return
            cap_small_frame_cap_new(
                ASID_LOW(asid),                /* capFMappedASIDLow  */
                wordFromVMRights(VMReadWrite), /* capFVMRights       */
                vptr,                          /* capFMappedAddress  */
#ifdef CONFIG_ARM_SMMU
                0,                             /* IOSpace            */
#endif
                ASID_HIGH(asid),               /* capFMappedASIDHigh */
                pptr                           /* capFBasePtr        */
            );
}

static BOOT_CODE void
map_it_pt_cap(cap_t pd_cap, cap_t pt_cap)
{
    pde_t* pd   = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(pd_cap));
    pte_t* pt   = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(pt_cap));
    vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(pt_cap);
    pde_t* targetSlot = pd + (vptr >> pageBitsForSize(ARMSection));

    assert(cap_page_table_cap_get_capPTIsMapped(pt_cap));

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    *targetSlot = pde_pde_coarse_new(
                      addrFromPPtr(pt), /* address */
                      true,             /* P       */
                      0                 /* Domain  */
                  );
#else
    *targetSlot = pde_pde_coarse_new(addrFromPPtr(pt));
#endif
}

/* Create a page table for the initial thread */

static BOOT_CODE cap_t
create_it_page_table_cap(cap_t pd, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              1,    /* capPTIsMapped      */
              asid, /* capPTMappedASID    */
              vptr, /* capPTMappedAddress */
              pptr  /* capPTBasePtr       */
          );
    if (asid != asidInvalid) {
        map_it_pt_cap(pd, cap);
    }
    return cap;
}

/* Create an address space for the initial thread.
 * This includes page directory and page tables */
BOOT_CODE cap_t
create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg)
{
    cap_t      pd_cap;
    vptr_t     pt_vptr;
    pptr_t     pt_pptr;
    seL4_SlotPos slot_pos_before;
    seL4_SlotPos slot_pos_after;
    pptr_t pd_pptr;

    /* create PD obj and cap */
    pd_pptr = alloc_region(seL4_PageDirBits);
    if (!pd_pptr) {
        return cap_null_cap_new();
    }
    memzero(PDE_PTR(pd_pptr), 1 << seL4_PageDirBits);
    copyGlobalMappings(PDE_PTR(pd_pptr));
    cleanCacheRange_PoU(pd_pptr, pd_pptr + (1 << seL4_PageDirBits) - 1,
                        addrFromPPtr((void *)pd_pptr));
    pd_cap =
        cap_page_directory_cap_new(
            true,    /* capPDIsMapped   */
            IT_ASID, /* capPDMappedASID */
            pd_pptr  /* capPDBasePtr    */
        );
    slot_pos_before = ndks_boot.slot_pos_cur;
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace), pd_cap);

    /* create all PT objs and caps necessary to cover userland image */

    for (pt_vptr = ROUND_DOWN(it_v_reg.start, PT_BITS + PAGE_BITS);
            pt_vptr < it_v_reg.end;
            pt_vptr += BIT(PT_BITS + PAGE_BITS)) {
        pt_pptr = alloc_region(seL4_PageTableBits);
        if (!pt_pptr) {
            return cap_null_cap_new();
        }
        memzero(PTE_PTR(pt_pptr), 1 << seL4_PageTableBits);
        if (!provide_cap(root_cnode_cap,
                         create_it_page_table_cap(pd_cap, pt_pptr, pt_vptr, IT_ASID))
           ) {
            return cap_null_cap_new();
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->userImagePaging = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };

    return pd_cap;
}

BOOT_CODE bool_t
create_device_frames(cap_t root_cnode_cap)
{
    seL4_SlotPos     slot_pos_before;
    seL4_SlotPos     slot_pos_after;
    vm_page_size_t frame_size;
    region_t       dev_reg;
    seL4_DeviceRegion   bi_dev_reg;
    cap_t          frame_cap;
    word_t         i;
    pptr_t         f;

    ndks_boot.bi_frame->numDeviceRegions = get_num_dev_p_regs();
    if (ndks_boot.bi_frame->numDeviceRegions > CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS) {
        printf("Kernel init: Too many device regions for boot info\n");
        ndks_boot.bi_frame->numDeviceRegions = CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS;
    }

    for (i = 0; i < ndks_boot.bi_frame->numDeviceRegions; i++) {
        /* write the frame caps of this device region into the root CNode and update the bootinfo */
        dev_reg = paddr_to_pptr_reg(get_dev_p_reg(i));
        /* use 1M frames if possible, otherwise use 4K frames */
        if (IS_ALIGNED(dev_reg.start, pageBitsForSize(ARMSection)) &&
                IS_ALIGNED(dev_reg.end,   pageBitsForSize(ARMSection))) {
            frame_size = ARMSection;
        } else {
            frame_size = ARMSmallPage;
        }

        slot_pos_before = ndks_boot.slot_pos_cur;

        /* create/provide frame caps covering the region */
        for (f = dev_reg.start; f < dev_reg.end; f += BIT(pageBitsForSize(frame_size))) {
            frame_cap = create_it_frame_cap(f, 0, asidInvalid, frame_size == ARMSection);
            if (!provide_cap(root_cnode_cap, frame_cap)) {
                return false;
            }
        }

        slot_pos_after = ndks_boot.slot_pos_cur;

        /* add device-region entry to bootinfo */
        bi_dev_reg.basePaddr = pptr_to_paddr((void*)dev_reg.start);
        bi_dev_reg.frameSizeBits = pageBitsForSize(frame_size);
        bi_dev_reg.frames = (seL4_SlotRegion) {
            slot_pos_before, slot_pos_after
        };
        ndks_boot.bi_frame->deviceRegions[i] = bi_dev_reg;
    }

    return true;
}

BOOT_CODE cap_t
create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large)
{
    return create_it_frame_cap(pptr, 0, asidInvalid, use_large);
}

BOOT_CODE cap_t
create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large, bool_t executable)
{
    cap_t cap = create_it_frame_cap(pptr, vptr, asid, use_large);
    map_it_frame_cap(pd_cap, cap, executable);
    return cap;
}

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT

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

#else

BOOT_CODE void
activate_global_pd(void)
{
    uint32_t r;
    /* Ensure that there's nothing stale in newly-mapped regions, and
       that everything we've written (particularly the kernel page tables)
       is committed. */
    cleanInvalidateL1Caches();
    /* Setup the memory attributes: We use 2 indicies (cachable/non-cachable) */
    setHMAIR((ATTRINDX_NONCACHEABLE << 0) | (ATTRINDX_CACHEABLE << 8), 0);
    setCurrentHypPD(addrFromPPtr(armHSGlobalPGD));
    invalidateHypTLB();
#if 0 /* Can't lock entries on A15 */
    lockTLBEntry(kernelBase);
    lockTLBEntry(PPTR_VECTOR_TABLE);
#endif
    /* TODO find a better place to init the VMMU */
    r = 0;
    /* Translation range */
    r |= (0x0 << 0);     /* 2^(32 -(0)) input range. */
    r |= (r & 0x8) << 1; /* Sign bit */
    /* starting level */
    r |= (0x0 << 6);     /* Start at second level */
    /* Sharability of tables */
    r |= (1 << 8);       /* Inner write-back, write-allocate */
    r |= (1 << 10);      /* Outer write-back, write-allocate */
    /* Long descriptor format (not that we have a choice) */
    r |= (1 << 31);
    setVTCR(r);
}

#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

BOOT_CODE void
write_it_asid_pool(cap_t it_ap_cap, cap_t it_pd_cap)
{
    asid_pool_t* ap = ASID_POOL_PTR(pptr_of_cap(it_ap_cap));
    ap->array[IT_ASID] = PDE_PTR(pptr_of_cap(it_pd_cap));
    armKSASIDTable[IT_ASID >> asidLowBits] = ap;
}

/* ==================== BOOT CODE FINISHES HERE ==================== */

findPDForASID_ret_t
findPDForASID(asid_t asid)
{
    findPDForASID_ret_t ret;
    asid_pool_t *poolPtr;
    pde_t *pd;

    poolPtr = armKSASIDTable[asid >> asidLowBits];
    if (unlikely(!poolPtr)) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.pd = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    pd = poolPtr->array[asid & MASK(asidLowBits)];
    if (unlikely(!pd)) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.pd = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ret.pd = pd;
    ret.status = EXCEPTION_NONE;
    return ret;
}

word_t * PURE
lookupIPCBuffer(bool_t isReceiver, tcb_t *thread)
{
    word_t w_bufferPtr;
    cap_t bufferCap;
    vm_rights_t vm_rights;

    w_bufferPtr = thread->tcbIPCBuffer;
    bufferCap = TCB_PTR_CTE_PTR(thread, tcbBuffer)->cap;

    if (unlikely(cap_get_capType(bufferCap) != cap_small_frame_cap &&
                 cap_get_capType(bufferCap) != cap_frame_cap)) {
        return NULL;
    }

    vm_rights = generic_frame_cap_get_capFVMRights(bufferCap);
    if (likely(vm_rights == VMReadWrite ||
               (!isReceiver && vm_rights == VMReadOnly))) {
        word_t basePtr;
        unsigned int pageBits;

        basePtr = generic_frame_cap_get_capFBasePtr(bufferCap);
        pageBits = pageBitsForSize(generic_frame_cap_get_capFSize(bufferCap));
        return (word_t *)(basePtr + (w_bufferPtr & MASK(pageBits)));
    } else {
        return NULL;
    }
}

exception_t
checkValidIPCBuffer(vptr_t vptr, cap_t cap)
{
    if (unlikely(cap_get_capType(cap) != cap_small_frame_cap &&
                 cap_get_capType(cap) != cap_frame_cap)) {
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

pde_t * CONST
lookupPDSlot(pde_t *pd, vptr_t vptr)
{
    unsigned int pdIndex;

    pdIndex = vptr >> (PAGE_BITS + PT_BITS);
    return pd + pdIndex;
}

lookupPTSlot_ret_t
lookupPTSlot(pde_t *pd, vptr_t vptr)
{
    lookupPTSlot_ret_t ret;
    pde_t *pdSlot;

    pdSlot = lookupPDSlot(pd, vptr);

    if (unlikely(pde_ptr_get_pdeType(pdSlot) != pde_pde_coarse)) {
        current_lookup_fault = lookup_fault_missing_capability_new(PT_BITS + PAGE_BITS);

        ret.ptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        pte_t *pt, *ptSlot;
        unsigned int ptIndex;

        pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pdSlot));
        ptIndex = (vptr >> PAGE_BITS) & MASK(PT_BITS);
        ptSlot = pt + ptIndex;

        ret.ptSlot = ptSlot;
        ret.status = EXCEPTION_NONE;
        return ret;
    }
}

static pte_t *
lookupPTSlot_nofail(pte_t *pt, vptr_t vptr)
{
    unsigned int ptIndex;

    ptIndex = (vptr >> PAGE_BITS) & MASK(PT_BITS);
    return pt + ptIndex;
}

static const resolve_ret_t default_resolve_ret_t;

static resolve_ret_t
resolveVAddr(pde_t *pd, vptr_t vaddr)
{
    pde_t *pde = lookupPDSlot(pd, vaddr);
    resolve_ret_t ret = default_resolve_ret_t;

    ret.valid = true;

    switch (pde_ptr_get_pdeType(pde)) {
    case pde_pde_section:
        ret.frameBase = pde_pde_section_ptr_get_address(pde);
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (pde_pde_section_ptr_get_size(pde)) {
            ret.frameSize = ARMSuperSection;
        } else {
            ret.frameSize = ARMSection;
        }
#else
        if (pde_pde_section_ptr_get_contiguous_hint(pde)) {
            ret.frameSize = ARMSuperSection;
        } else {
            ret.frameSize = ARMSection;
        }
#endif
        return ret;

    case pde_pde_coarse: {
        pte_t *pt = ptrFromPAddr(pde_pde_coarse_ptr_get_address(pde));
        pte_t *pte = lookupPTSlot_nofail(pt, vaddr);
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
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
#else
        if (pte_pte_small_ptr_get_contiguous_hint(pte)) {
            ret.frameBase = pte_pte_small_ptr_get_address(pte);
            /* Entries are represented as 16 contiguous small frames. We need to mask to get the large frame base */
            ret.frameBase &= ~MASK(pageBitsForSize(ARMLargePage));
            ret.frameSize = ARMLargePage;
            return ret;
        } else {
            ret.frameBase = pte_pte_small_ptr_get_address(pte);
            ret.frameSize = ARMSmallPage;
            return ret;
        }
#endif
        break;
    }
    }

    ret.valid = false;
    return ret;
}

static pte_t CONST
makeUserPTE(vm_page_size_t page_size, paddr_t paddr,
            bool_t cacheable, bool_t nonexecutable, vm_rights_t vm_rights)
{
    pte_t pte;
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
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

#else

    word_t hap;

    hap = HAPFromVMRights(vm_rights);

    switch (page_size) {
    case ARMSmallPage: {
        if (cacheable) {
            pte = pte_pte_small_new(
                      0,      /* Executable */
                      0,      /* Not contiguous */
                      paddr,
                      1,      /* AF - Always set */
                      0,      /* not shared */
                      hap,    /* HAP - access */
                      MEMATTR_CACHEABLE /* Cacheable */);
        } else {
            pte = pte_pte_small_new(
                      0,      /* Executable */
                      0,      /* Not contiguous */
                      paddr,
                      1,      /* AF - Always set */
                      0,      /* not shared */
                      hap,    /* HAP - access */
                      MEMATTR_NONCACHEABLE /* Not cacheable */);
        }
        break;
    }

    case ARMLargePage: {
        if (cacheable) {
            pte = pte_pte_small_new(
                      0,   /* Executable */
                      1,   /* 16 contiguous */
                      paddr,
                      1,   /* AF - Always set */
                      0,   /* not shared */
                      hap, /* HAP - access */
                      MEMATTR_CACHEABLE  /* Cacheable */);
        } else {
            pte = pte_pte_small_new(
                      0,   /* Executable */
                      1,   /* 16 contiguous */
                      paddr,
                      1,   /* AF - Always set */
                      0,   /* not shared */
                      hap, /* HAP - access */
                      MEMATTR_NONCACHEABLE /* Not cacheable */);
        }
        break;
    }
    default:
        fail("Invalid PTE frame type");
    }
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

    return pte;
}

static pde_t CONST
makeUserPDE(vm_page_size_t page_size, paddr_t paddr, bool_t parity,
            bool_t cacheable, bool_t nonexecutable, word_t domain,
            vm_rights_t vm_rights)
{
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    word_t ap, size2;

    ap = APFromVMRights(vm_rights);
#else
    word_t hap, size2;

    (void)domain;
    hap = HAPFromVMRights(vm_rights);
#endif

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

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
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
#else
    if (cacheable) {
        return pde_pde_section_new(
                   0, /* Executable */
                   size2, /* contiguous */
                   paddr,
                   1, /* AF - Always set */
                   0, /* not shared */
                   hap,
                   MEMATTR_CACHEABLE /* Cacheable */);
    } else {
        return pde_pde_section_new(
                   0, /* Executable */
                   size2, /* contiguous */
                   paddr,
                   1, /* AF - Always set */
                   0, /* not shared */
                   hap,
                   MEMATTR_NONCACHEABLE /* Not cacheable */);
    }
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
}

bool_t CONST
isValidVTableRoot(cap_t cap)
{
    return cap_get_capType(cap) == cap_page_directory_cap &&
           cap_page_directory_cap_get_capPDIsMapped(cap);
}

bool_t CONST
isIOSpaceFrame(cap_t cap)
{
#ifdef CONFIG_ARM_SMMU
    return cap_get_capType(cap) == cap_small_frame_cap && cap_small_frame_cap_get_capFIsIOSpace(cap);
#else
    return false;
#endif
}

void
setVMRoot(tcb_t *tcb)
{
    cap_t threadRoot;
    asid_t asid;
    pde_t *pd;
    findPDForASID_ret_t find_ret;

    threadRoot = TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap;

    if (cap_get_capType(threadRoot) != cap_page_directory_cap ||
            !cap_page_directory_cap_get_capPDIsMapped(threadRoot)) {
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            setCurrentPD(addrFromPPtr(0));
        } else {
            setCurrentPD(addrFromPPtr(armKSGlobalPD));
        }
        return;
    }

    pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(threadRoot));
    asid = cap_page_directory_cap_get_capPDMappedASID(threadRoot);
    find_ret = findPDForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE || find_ret.pd != pd)) {
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            setCurrentPD(addrFromPPtr(0));
        } else {
            setCurrentPD(addrFromPPtr(armKSGlobalPD));
        }
        return;
    }

    armv_contextSwitch(pd, asid);
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_switch(tcb->tcbArch.vcpu);
    }
}

static bool_t
setVMRootForFlush(pde_t* pd, asid_t asid)
{
    cap_t threadRoot;

    threadRoot = TCB_PTR_CTE_PTR(ksCurThread, tcbVTable)->cap;

    if (cap_get_capType(threadRoot) == cap_page_directory_cap &&
            cap_page_directory_cap_get_capPDIsMapped(threadRoot) &&
            PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(threadRoot)) == pd) {
        return false;
    }

    armv_contextSwitch(pd, asid);

    return true;
}

pde_t *
pageTableMapped(asid_t asid, vptr_t vaddr, pte_t* pt)
{
    findPDForASID_ret_t find_ret;
    pde_t pde;
    unsigned int pdIndex;

    find_ret = findPDForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE)) {
        return NULL;
    }

    pdIndex = vaddr >> (PAGE_BITS + PT_BITS);
    pde = find_ret.pd[pdIndex];

    if (likely(pde_get_pdeType(pde) == pde_pde_coarse
               && ptrFromPAddr (pde_pde_coarse_get_address(pde)) == pt)) {
        return find_ret.pd;
    } else {
        return NULL;
    }
}

static void
invalidateASID(asid_t asid)
{
    asid_pool_t *asidPool;
    pde_t *pd;

    asidPool = armKSASIDTable[asid >> asidLowBits];
    assert(asidPool);

    pd = asidPool->array[asid & MASK(asidLowBits)];
    assert(pd);

    pd[PD_ASID_SLOT] = pde_pde_invalid_new(0, false);
}

static pde_t PURE
loadHWASID(asid_t asid)
{
    asid_pool_t *asidPool;
    pde_t *pd;

    asidPool = armKSASIDTable[asid >> asidLowBits];
    assert(asidPool);

    pd = asidPool->array[asid & MASK(asidLowBits)];
    assert(pd);

    return pd[PD_ASID_SLOT];
}

static void
storeHWASID(asid_t asid, hw_asid_t hw_asid)
{
    asid_pool_t *asidPool;
    pde_t *pd;

    asidPool = armKSASIDTable[asid >> asidLowBits];
    assert(asidPool);

    pd = asidPool->array[asid & MASK(asidLowBits)];
    assert(pd);

    /* Store HW ASID in the last entry
       Masquerade as an invalid PDE */
    pd[PD_ASID_SLOT] = pde_pde_invalid_new(hw_asid, true);

    armKSHWASIDTable[hw_asid] = asid;
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
        if (armKSHWASIDTable[hw_asid] == asidInvalid) {
            return hw_asid;
        }
    }

    hw_asid = armKSNextASID;

    /* If we've scanned the table without finding a free ASID */
    invalidateASID(armKSHWASIDTable[hw_asid]);

    /* Flush TLB */
    invalidateTLB_ASID(hw_asid);
    armKSHWASIDTable[hw_asid] = asidInvalid;

    /* Increment the NextASID index */
    armKSNextASID++;

    return hw_asid;
}

hw_asid_t
getHWASID(asid_t asid)
{
    pde_t stored_hw_asid;

    stored_hw_asid = loadHWASID(asid);
    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        return pde_pde_invalid_get_stored_hw_asid(stored_hw_asid);
    } else {
        hw_asid_t new_hw_asid;

        new_hw_asid = findFreeHWASID();
        storeHWASID(asid, new_hw_asid);
        return new_hw_asid;
    }
}

static void
invalidateASIDEntry(asid_t asid)
{
    pde_t stored_hw_asid;

    stored_hw_asid = loadHWASID(asid);
    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        armKSHWASIDTable[pde_pde_invalid_get_stored_hw_asid(stored_hw_asid)] =
            asidInvalid;
    }
    invalidateASID(asid);
}

void
unmapPageTable(asid_t asid, vptr_t vaddr, pte_t* pt)
{
    pde_t *pd, *pdSlot;
    unsigned int pdIndex;

    pd = pageTableMapped (asid, vaddr, pt);

    if (likely(pd != NULL)) {
        pdIndex = vaddr >> (PT_BITS + PAGE_BITS);
        pdSlot = pd + pdIndex;

        *pdSlot = pde_pde_invalid_new(0, 0);
        cleanByVA_PoU((word_t)pdSlot, addrFromPPtr(pdSlot));
        flushTable(pd, asid, vaddr, pt);
    }
}

void
copyGlobalMappings(pde_t *newPD)
{
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    word_t i;
    pde_t *global_pd = armKSGlobalPD;

    for (i = kernelBase >> ARMSectionBits; i < BIT(PD_BITS); i++) {
        if (i != PD_ASID_SLOT) {
            newPD[i] = global_pd[i];
        }
    }
#else
    /* Kernel and user MMUs are completely independent, however,
     * we still need to share the globals page. */
    pde_t pde;
    pde = pde_pde_coarse_new(addrFromPPtr(armUSGlobalPT));
    newPD[BIT(PD_BITS) - 1] = pde;
#endif
}

exception_t
handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType)
{
    switch (vm_faultType) {
    case ARMDataAbort: {
        word_t addr, fault;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        addr = getHDFAR();
        addr = (addressTranslateS1CPR(addr) & ~MASK(PAGE_BITS)) | (addr & MASK(PAGE_BITS));
        /* MSBs tell us that this was a DataAbort */
        fault = getHSR() & 0x3ffffff;
#else
        addr = getFAR();
        fault = getDFSR();
#endif
        current_fault = fault_vm_fault_new(addr, fault, false);
        return EXCEPTION_FAULT;
    }

    case ARMPrefetchAbort: {
        word_t pc, fault;

        pc = getRestartPC(thread);

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        pc = (addressTranslateS1CPR(pc) & ~MASK(PAGE_BITS)) | (pc & MASK(PAGE_BITS));
        /* MSBs tell us that this was a PrefetchAbort */
        fault = getHSR() & 0x3ffffff;
#else
        fault = getIFSR();
#endif
        current_fault = fault_vm_fault_new(pc, fault, true);
        return EXCEPTION_FAULT;
    }

    default:
        fail("Invalid VM fault type");
    }
}

void
deleteASIDPool(asid_t asid_base, asid_pool_t* pool)
{
    unsigned int offset;

    /* Haskell error: "ASID pool's base must be aligned" */
    assert((asid_base & MASK(asidLowBits)) == 0);

    if (armKSASIDTable[asid_base >> asidLowBits] == pool) {
        for (offset = 0; offset < BIT(asidLowBits); offset++) {
            if (pool->array[offset]) {
                flushSpace(asid_base + offset);
                invalidateASIDEntry(asid_base + offset);
            }
        }
        armKSASIDTable[asid_base >> asidLowBits] = NULL;
        setVMRoot(ksCurThread);
    }
}

void
deleteASID(asid_t asid, pde_t* pd)
{
    asid_pool_t *poolPtr;

    poolPtr = armKSASIDTable[asid >> asidLowBits];

    if (poolPtr != NULL && poolPtr->array[asid & MASK(asidLowBits)] == pd) {
        flushSpace(asid);
        invalidateASIDEntry(asid);
        poolPtr->array[asid & MASK(asidLowBits)] = NULL;
        setVMRoot(ksCurThread);
    }
}

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
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
#endif

void
unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr)
{
    findPDForASID_ret_t find_ret;
    paddr_t addr = addrFromPPtr(pptr);

    find_ret = findPDForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE)) {
        return;
    }

    switch (page_size) {
    case ARMSmallPage: {
        lookupPTSlot_ret_t lu_ret;

        lu_ret = lookupPTSlot(find_ret.pd, vptr);
        if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
            return;
        }

        if (unlikely(pte_ptr_get_pteType(lu_ret.ptSlot) != pte_pte_small)) {
            return;
        }
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (unlikely(pte_pte_small_ptr_get_contiguous_hint(lu_ret.ptSlot) != 0)) {
            return;
        }
#endif
        if (unlikely(pte_pte_small_ptr_get_address(lu_ret.ptSlot) != addr)) {
            return;
        }

        *(lu_ret.ptSlot) = pte_pte_invalid_new();
        cleanByVA_PoU((word_t)lu_ret.ptSlot, addrFromPPtr(lu_ret.ptSlot));

        break;
    }

    case ARMLargePage: {
        lookupPTSlot_ret_t lu_ret;
        word_t i;

        lu_ret = lookupPTSlot(find_ret.pd, vptr);
        if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
            return;
        }
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (unlikely(pte_ptr_get_pteType(lu_ret.ptSlot) != pte_pte_large)) {
            return;
        }
        if (unlikely(pte_pte_large_ptr_get_address(lu_ret.ptSlot) != addr)) {
            return;
        }
#else
        if (unlikely(pte_ptr_get_pteType(lu_ret.ptSlot) != pte_pte_small)) {
            return;
        }
        if (unlikely(pte_pte_small_ptr_get_contiguous_hint(lu_ret.ptSlot) != 1)) {
            return;
        }
        if (unlikely(pte_pte_small_ptr_get_address(lu_ret.ptSlot) != addr)) {
            return;
        }
#endif

        for (i = 0; i < PAGES_PER_LARGE_PAGE; i++) {
            lu_ret.ptSlot[i] = pte_pte_invalid_new();
        }
        cleanCacheRange_PoU((word_t)&lu_ret.ptSlot[0],
                            LAST_BYTE_PTE(lu_ret.ptSlot, PAGES_PER_LARGE_PAGE),
                            addrFromPPtr(&lu_ret.ptSlot[0]));

        break;
    }

    case ARMSection: {
        pde_t *pd;

        pd = lookupPDSlot(find_ret.pd, vptr);

        if (unlikely(pde_ptr_get_pdeType(pd) != pde_pde_section)) {
            return;
        }
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (unlikely(pde_pde_section_ptr_get_size(pd) != 0)) {
#else
        if (unlikely(pde_pde_section_ptr_get_contiguous_hint(pd) != 0)) {
#endif
            return;
        }
        if (unlikely(pde_pde_section_ptr_get_address(pd) != addr)) {
            return;
        }

        *pd = pde_pde_invalid_new(0, 0);
        cleanByVA_PoU((word_t)pd, addrFromPPtr(pd));

        break;
    }

    case ARMSuperSection: {
        pde_t *pd;
        word_t i;

        pd = lookupPDSlot(find_ret.pd, vptr);

        if (unlikely(pde_ptr_get_pdeType(pd) != pde_pde_section)) {
            return;
        }
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (unlikely(pde_pde_section_ptr_get_size(pd) != 1)) {
#else
        if (unlikely(pde_pde_section_ptr_get_contiguous_hint(pd) != 1)) {
#endif
            return;
        }
        if (unlikely(pde_pde_section_ptr_get_address(pd) != addr)) {
            return;
        }

        for (i = 0; i < SECTIONS_PER_SUPER_SECTION; i++) {
            pd[i] = pde_pde_invalid_new(0, 0);
        }
        cleanCacheRange_PoU((word_t)&pd[0], LAST_BYTE_PDE(pd, SECTIONS_PER_SUPER_SECTION),
                            addrFromPPtr(&pd[0]));

        break;
    }

    default:
        fail("Invalid ARM page type");
        break;
    }

    /* Flush the page now that the mapping has been updated */
    flushPage(page_size, find_ret.pd, asid, vptr);
}

void
flushPage(vm_page_size_t page_size, pde_t* pd, asid_t asid, word_t vptr)
{
    pde_t stored_hw_asid;
    word_t base_addr;
    bool_t root_switched;

    assert((vptr & MASK(pageBitsForSize(page_size))) == 0);

    /* Switch to the address space to allow a cache clean by VA */
    root_switched = setVMRootForFlush(pd, asid);
    stored_hw_asid = loadHWASID(asid);

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
flushTable(pde_t* pd, asid_t asid, word_t vptr, pte_t* pt)
{
    pde_t stored_hw_asid;
    bool_t root_switched;

    assert((vptr & MASK(PT_BITS + ARMSmallPageBits)) == 0);

    /* Switch to the address space to allow a cache clean by VA */
    root_switched = setVMRootForFlush(pd, asid);
    stored_hw_asid = loadHWASID(asid);

    if (pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        invalidateTLB_ASID(pde_pde_invalid_get_stored_hw_asid(stored_hw_asid));
        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }
}

void
flushSpace(asid_t asid)
{
    pde_t stored_hw_asid;

    stored_hw_asid = loadHWASID(asid);

    /* Clean the entire data cache, to guarantee that any VAs mapped
     * in the deleted space are clean (because we can't clean by VA after
     * deleting the space) */
    cleanCaches_PoU();

    /* If the given ASID doesn't have a hardware ASID
     * assigned, then it can't have any mappings in the TLB */
    if (!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        return;
    }

    /* Do the TLB flush */
    invalidateTLB_ASID(pde_pde_invalid_get_stored_hw_asid(stored_hw_asid));
}

void
invalidateTLBByASID(asid_t asid)
{
    pde_t stored_hw_asid;

    stored_hw_asid = loadHWASID(asid);

    /* If the given ASID doesn't have a hardware ASID
     * assigned, then it can't have any mappings in the TLB */
    if (!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid)) {
        return;
    }

    /* Do the TLB flush */
    invalidateTLB_ASID(pde_pde_invalid_get_stored_hw_asid(stored_hw_asid));
}

static inline bool_t CONST
checkVPAlignment(vm_page_size_t sz, word_t w)
{
    return (w & MASK(pageBitsForSize(sz))) == 0;
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
    word_t i;

    switch (frameSize) {

    case ARMSmallPage:

        ret.pte_entries.base = NULL; /* to avoid uninitialised warning */
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

        ret.pte_entries.base = lu_ret.ptSlot;
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (unlikely(pte_ptr_get_pteType(ret.pte_entries.base) ==
                     pte_pte_large)) {
#else
        if (unlikely(pte_ptr_get_pteType(ret.pte_entries.base) == pte_pte_small
                     && pte_pte_small_ptr_get_contiguous_hint(ret.pte_entries.base))) {
#endif
            current_syscall_error.type =
                seL4_DeleteFirst;

            ret.status = EXCEPTION_SYSCALL_ERROR;
            return ret;
        }

        ret.status = EXCEPTION_NONE;
        return ret;

    case ARMLargePage:

        ret.pte_entries.base = NULL; /* to avoid uninitialised warning */
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

        ret.pte_entries.base = lu_ret.ptSlot;

        for (i = 0; i < PAGES_PER_LARGE_PAGE; i++) {
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
            if (unlikely(pte_get_pteType(ret.pte_entries.base[i]) ==
                         pte_pte_small)) {
#else
            if (unlikely(pte_ptr_get_pteType(lu_ret.ptSlot) == pte_pte_small
                         && !pte_pte_small_get_contiguous_hint(ret.pte_entries.base[i]))) {
#endif
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
    word_t i;

    switch (frameSize) {

        /* PDE mappings */
    case ARMSection:
        ret.pde_entries.base = lookupPDSlot(pd, vaddr);
        ret.pde_entries.length = 1;

        ret.pde = makeUserPDE(ARMSection, base,
                              vm_attributes_get_armParityEnabled(attr),
                              vm_attributes_get_armPageCacheable(attr),
                              vm_attributes_get_armExecuteNever(attr),
                              0,
                              vmRights);

        currentPDEType =
            pde_ptr_get_pdeType(ret.pde_entries.base);
        if (unlikely(currentPDEType != pde_pde_invalid &&
                     (currentPDEType != pde_pde_section ||
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
                      pde_pde_section_ptr_get_size(ret.pde_entries.base) != 0))) {
#else
                      pde_pde_section_ptr_get_contiguous_hint(ret.pde_entries.base) != 0))) {
#endif
            current_syscall_error.type =
                seL4_DeleteFirst;
            ret.status = EXCEPTION_SYSCALL_ERROR;

            return ret;
        }

        ret.status = EXCEPTION_NONE;
        return ret;

    case ARMSuperSection:
        ret.pde_entries.base = lookupPDSlot(pd, vaddr);
        ret.pde_entries.length = SECTIONS_PER_SUPER_SECTION;

        ret.pde = makeUserPDE(ARMSuperSection, base,
                              vm_attributes_get_armParityEnabled(attr),
                              vm_attributes_get_armPageCacheable(attr),
                              vm_attributes_get_armExecuteNever(attr),
                              0,
                              vmRights);

        for (i = 0; i < SECTIONS_PER_SUPER_SECTION; i++) {
            currentPDEType =
                pde_get_pdeType(ret.pde_entries.base[i]);
            if (unlikely(currentPDEType != pde_pde_invalid &&
                         (currentPDEType != pde_pde_section ||
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
                          pde_pde_section_get_size(ret.pde_entries.base[i]) != 1))) {
#else
                          pde_pde_section_get_contiguous_hint(ret.pde_entries.base[i]) != 1))) {
#endif
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

static inline vptr_t
pageBase(vptr_t vaddr, vm_page_size_t size)
{
    return vaddr & ~MASK(pageBitsForSize(size));
}

static bool_t PURE
pteCheckIfMapped(pte_t *pte)
{
    return pte_ptr_get_pteType(pte) != pte_pte_invalid;
}

static bool_t PURE
pdeCheckIfMapped(pde_t *pde)
{
    return pde_ptr_get_pdeType(pde) != pde_pde_invalid;
}

static void
doFlush(int invLabel, vptr_t start, vptr_t end, paddr_t pstart)
{
    /** GHOSTUPD: "((gs_get_assn cap_get_capSizeBits_'proc \<acute>ghost'state = 0
            \<or> \<acute>end - \<acute>start <= gs_get_assn cap_get_capSizeBits_'proc \<acute>ghost'state)
        \<and> \<acute>start <= \<acute>end, id)" */
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* The hypervisor does not share an AS with userspace so we must flush
         * by kernel MVA instead. ARMv7 caches are PIPT so it makes no difference */
        end = (vptr_t)paddr_to_pptr(pstart) + (end - start);
        start = (vptr_t)paddr_to_pptr(pstart);
    }
    switch (invLabel) {
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

/* ================= INVOCATION HANDLING STARTS HERE ================== */

static exception_t
performPDFlush(int invLabel, pde_t *pd, asid_t asid, vptr_t start,
               vptr_t end, paddr_t pstart)
{
    bool_t root_switched;

    /* Flush if given a non zero range */
    if (start < end) {
        root_switched = setVMRootForFlush(pd, asid);

        doFlush(invLabel, start, end, pstart);

        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }

    return EXCEPTION_NONE;
}

static exception_t
performPageTableInvocationMap(cap_t cap, cte_t *ctSlot,
                              pde_t pde, pde_t *pdSlot)
{
    ctSlot->cap = cap;
    *pdSlot = pde;
    cleanByVA_PoU((word_t)pdSlot, addrFromPPtr(pdSlot));

    return EXCEPTION_NONE;
}

static exception_t
performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        pte_t *pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap));
        unmapPageTable(
            cap_page_table_cap_get_capPTMappedASID(cap),
            cap_page_table_cap_get_capPTMappedAddress(cap),
            pt);
        clearMemory((void *)pt, cap_get_capSizeBits(cap));
    }
    cap_page_table_cap_ptr_set_capPTIsMapped(&(ctSlot->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t
performPageInvocationMapPTE(asid_t asid, cap_t cap, cte_t *ctSlot, pte_t pte,
                            pte_range_t pte_entries)
{
    word_t i, j UNUSED;
    bool_t tlbflush_required;

    ctSlot->cap = cap;

    /* we only need to check the first entries because of how createSafeMappingEntries
     * works to preserve the consistency of tables */
    tlbflush_required = pteCheckIfMapped(pte_entries.base);

    j = pte_entries.length;
    /** GHOSTUPD: "(\<acute>j <= 16, id)" */

    for (i = 0; i < pte_entries.length; i++) {
        pte_entries.base[i] = pte;
    }
    cleanCacheRange_PoU((word_t)pte_entries.base,
                        LAST_BYTE_PTE(pte_entries.base, pte_entries.length),
                        addrFromPPtr(pte_entries.base));
    if (unlikely(tlbflush_required)) {
        invalidateTLBByASID(asid);
    }

    return EXCEPTION_NONE;
}

static exception_t
performPageInvocationMapPDE(asid_t asid, cap_t cap, cte_t *ctSlot, pde_t pde,
                            pde_range_t pde_entries)
{
    word_t i, j UNUSED;
    bool_t tlbflush_required;

    ctSlot->cap = cap;

    /* we only need to check the first entries because of how createSafeMappingEntries
     * works to preserve the consistency of tables */
    tlbflush_required = pdeCheckIfMapped(pde_entries.base);

    j = pde_entries.length;
    /** GHOSTUPD: "(\<acute>j <= 16, id)" */

    for (i = 0; i < pde_entries.length; i++) {
        pde_entries.base[i] = pde;
    }
    cleanCacheRange_PoU((word_t)pde_entries.base,
                        LAST_BYTE_PDE(pde_entries.base, pde_entries.length),
                        addrFromPPtr(pde_entries.base));
    if (unlikely(tlbflush_required)) {
        invalidateTLBByASID(asid);
    }

    return EXCEPTION_NONE;
}

static exception_t
performPageInvocationRemapPTE(asid_t asid, pte_t pte, pte_range_t pte_entries)
{
    word_t i, j UNUSED;
    bool_t tlbflush_required;

    /* we only need to check the first entries because of how createSafeMappingEntries
     * works to preserve the consistency of tables */
    tlbflush_required = pteCheckIfMapped(pte_entries.base);

    j = pte_entries.length;
    /** GHOSTUPD: "(\<acute>j <= 16, id)" */

    for (i = 0; i < pte_entries.length; i++) {
        pte_entries.base[i] = pte;
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            pte.words[0] += BIT(pageBitsForSize(ARMLargePage));
        }
    }
    cleanCacheRange_PoU((word_t)pte_entries.base,
                        LAST_BYTE_PTE(pte_entries.base, pte_entries.length),
                        addrFromPPtr(pte_entries.base));
    if (unlikely(tlbflush_required)) {
        invalidateTLBByASID(asid);
    }

    return EXCEPTION_NONE;
}

static exception_t
performPageInvocationRemapPDE(asid_t asid, pde_t pde, pde_range_t pde_entries)
{
    word_t i, j UNUSED;
    bool_t tlbflush_required;

    /* we only need to check the first entries because of how createSafeMappingEntries
     * works to preserve the consistency of tables */
    tlbflush_required = pdeCheckIfMapped(pde_entries.base);

    j = pde_entries.length;
    /** GHOSTUPD: "(\<acute>j <= 16, id)" */

    for (i = 0; i < pde_entries.length; i++) {
        pde_entries.base[i] = pde;
        if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
            pde.words[0] += BIT(pageBitsForSize(ARMSection));
        }
    }
    cleanCacheRange_PoU((word_t)pde_entries.base,
                        LAST_BYTE_PDE(pde_entries.base, pde_entries.length),
                        addrFromPPtr(pde_entries.base));
    if (unlikely(tlbflush_required)) {
        invalidateTLBByASID(asid);
    }

    return EXCEPTION_NONE;
}

static exception_t
performPageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (generic_frame_cap_get_capFIsMapped(cap)) {
        unmapPage(generic_frame_cap_get_capFSize(cap),
                  generic_frame_cap_get_capFMappedASID(cap),
                  generic_frame_cap_get_capFMappedAddress(cap),
                  (void *)generic_frame_cap_get_capFBasePtr(cap));
    }

    generic_frame_cap_ptr_set_capFMappedAddress(&ctSlot->cap, asidInvalid, 0);

    return EXCEPTION_NONE;
}

static exception_t
performPageFlush(int invLabel, pde_t *pd, asid_t asid, vptr_t start,
                 vptr_t end, paddr_t pstart)
{
    bool_t root_switched;

    /* now we can flush. But only if we were given a non zero range */
    if (start < end) {
        root_switched = setVMRootForFlush(pd, asid);

        doFlush(invLabel, start, end, pstart);

        if (root_switched) {
            setVMRoot(ksCurThread);
        }
    }

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
                wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, 1)));

    return EXCEPTION_NONE;
}

static exception_t
performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr,
                          cte_t *pdCapSlot)
{
    cap_page_directory_cap_ptr_set_capPDMappedASID(&pdCapSlot->cap, asid);
    cap_page_directory_cap_ptr_set_capPDIsMapped(&pdCapSlot->cap, 1);
    poolPtr->array[asid & MASK(asidLowBits)] =
        PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(pdCapSlot->cap));

    return EXCEPTION_NONE;
}

static exception_t
performASIDControlInvocation(void *frame, cte_t *slot,
                             cte_t *parent, asid_t asid_base)
{

    /** AUXUPD: "(True, typ_region_bytes (ptr_val \<acute>frame) 12)" */
    /** GHOSTUPD: "(True, gs_clear_region (ptr_val \<acute>frame) 12)" */
    cap_untyped_cap_ptr_set_capFreeIndex(&(parent->cap),
                                         MAX_FREE_INDEX(cap_untyped_cap_get_capBlockSize(parent->cap)));

    memzero(frame, 1 << ARMSmallPageBits);
    /** AUXUPD: "(True, ptr_retyps 1 (Ptr (ptr_val \<acute>frame) :: asid_pool_C ptr))" */

    cteInsert(cap_asid_pool_cap_new(asid_base, WORD_REF(frame)),
              parent, slot);;
    /* Haskell error: "ASID pool's base must be aligned" */
    assert((asid_base & MASK(asidLowBits)) == 0);
    armKSASIDTable[asid_base >> asidLowBits] = (asid_pool_t *)frame;

    return EXCEPTION_NONE;
}

static exception_t
decodeARMPageDirectoryInvocation(word_t invLabel, word_t length,
                                 cptr_t cptr, cte_t *cte, cap_t cap,
                                 extra_caps_t excaps, word_t *buffer)
{
    switch (invLabel) {
    case ARMPDClean_Data:
    case ARMPDInvalidate_Data:
    case ARMPDCleanInvalidate_Data:
    case ARMPDUnify_Instruction: {
        vptr_t start, end;
        paddr_t pstart;
        findPDForASID_ret_t find_ret;
        asid_t asid;
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

        if (unlikely(cap_get_capType(cap) != cap_page_directory_cap ||
                     !cap_page_directory_cap_get_capPDIsMapped(cap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }


        /* Make sure that the supplied pd is ok */
        pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));
        asid = cap_page_directory_cap_get_capPDMappedASID(cap);

        find_ret = findPDForASID(asid);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            userError("PD Flush: No PD for ASID");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(find_ret.pd != pd)) {
            userError("PD Flush: Invalid PD Cap");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

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
        return performPDFlush(invLabel, pd, asid, start, end - 1, pstart);
    }

    default:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

}

static exception_t
decodeARMPageTableInvocation(word_t invLabel, word_t length,
                             cte_t *cte, cap_t cap, extra_caps_t excaps,
                             word_t *buffer)
{
    word_t vaddr, pdIndex;

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    vm_attributes_t attr;
#endif
    cap_t pdCap;
    pde_t *pd, *pdSlot;
    pde_t pde;
    asid_t asid;
    paddr_t paddr;

    if (invLabel == ARMPageTableUnmap) {
        if (unlikely(! isFinalCapability(cte))) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageTableInvocationUnmap (cap, cte);
    }

    if (unlikely(invLabel != ARMPageTableMap)) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(length < 2 || excaps.excaprefs[0] == NULL)) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_page_table_cap_get_capPTIsMapped(cap))) {
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    attr = vmAttributesFromWord(getSyscallArg(1, buffer));
#endif
    pdCap = excaps.excaprefs[0]->cap;

    if (unlikely(cap_get_capType(pdCap) != cap_page_directory_cap ||
                 !cap_page_directory_cap_get_capPDIsMapped(pdCap))) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(pdCap));
    asid = cap_page_directory_cap_get_capPDMappedASID(pdCap);

    if (unlikely(vaddr >= kernelBase)) {
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    {
        findPDForASID_ret_t find_ret;

        find_ret = findPDForASID(asid);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(find_ret.pd != pd)) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    pdIndex = vaddr >> (PAGE_BITS + PT_BITS);
    pdSlot = &pd[pdIndex];
    if (unlikely(pde_ptr_get_pdeType(pdSlot) != pde_pde_invalid)) {
        current_syscall_error.type = seL4_DeleteFirst;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = addrFromPPtr(
                PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    pde = pde_pde_coarse_new(
              paddr,
              vm_attributes_get_armParityEnabled(attr),
              0 /* Domain */
          );
#else
    pde = pde_pde_coarse_new(paddr);
#endif

    cap = cap_page_table_cap_set_capPTIsMapped(cap, 1);
    cap = cap_page_table_cap_set_capPTMappedASID(cap, asid);
    cap = cap_page_table_cap_set_capPTMappedAddress(cap, vaddr);

    setThreadState(ksCurThread, ThreadState_Restart);
    return performPageTableInvocationMap(cap, cte, pde, pdSlot);
}

static exception_t
decodeARMFrameInvocation(word_t invLabel, word_t length,
                         cte_t *cte, cap_t cap, extra_caps_t excaps,
                         word_t *buffer)
{
    switch (invLabel) {
    case ARMPageMap: {
        word_t vaddr, vtop, w_rightsMask;
        paddr_t capFBasePtr;
        cap_t pdCap;
        pde_t *pd;
        asid_t asid;
        vm_rights_t capVMRights, vmRights;
        vm_page_size_t frameSize;
        vm_attributes_t attr;

        if (unlikely(length < 3 || excaps.excaprefs[0] == NULL)) {
            current_syscall_error.type =
                seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vaddr = getSyscallArg(0, buffer);
        w_rightsMask = getSyscallArg(1, buffer);
        attr = vmAttributesFromWord(getSyscallArg(2, buffer));
        pdCap = excaps.excaprefs[0]->cap;

        frameSize = generic_frame_cap_get_capFSize(cap);
        capVMRights = generic_frame_cap_get_capFVMRights(cap);

        if (unlikely(generic_frame_cap_get_capFIsMapped(cap))) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(cap_get_capType(pdCap) != cap_page_directory_cap ||
                     !cap_page_directory_cap_get_capPDIsMapped(pdCap))) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
        pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(
                         pdCap));
        asid = cap_page_directory_cap_get_capPDMappedASID(pdCap);

        {
            findPDForASID_ret_t find_ret;

            find_ret = findPDForASID(asid);
            if (unlikely(find_ret.status != EXCEPTION_NONE)) {
                userError("ARMPageMap: No PD for ASID");
                current_syscall_error.type =
                    seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource =
                    false;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (unlikely(find_ret.pd != pd)) {
                current_syscall_error.type =
                    seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;

                return EXCEPTION_SYSCALL_ERROR;
            }
        }

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
                                   generic_frame_cap_get_capFBasePtr(cap));

        cap = generic_frame_cap_set_capFMappedAddress(cap, asid,
                                                      vaddr);
        if (frameSize == ARMSmallPage || frameSize == ARMLargePage) {
            create_mappings_pte_return_t map_ret;
            map_ret = createSafeMappingEntries_PTE(capFBasePtr, vaddr,
                                                   frameSize, vmRights,
                                                   attr, pd);
            if (unlikely(map_ret.status != EXCEPTION_NONE)) {
                return map_ret.status;
            }

            setThreadState(ksCurThread, ThreadState_Restart);
            return performPageInvocationMapPTE(asid, cap, cte,
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
            return performPageInvocationMapPDE(asid, cap, cte,
                                               map_ret.pde,
                                               map_ret.pde_entries);
        }
    }

    case ARMPageRemap: {
        word_t vaddr, w_rightsMask;
        paddr_t capFBasePtr;
        cap_t pdCap;
        pde_t *pd;
        asid_t mappedASID;
        vm_rights_t capVMRights, vmRights;
        vm_page_size_t frameSize;
        vm_attributes_t attr;

#ifdef CONFIG_ARM_SMMU
        if (isIOSpaceFrame(cap)) {
            userError("ARMFrameRemap: Attempting to remap frame mapped into an IOSpace");
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }
#endif

        if (unlikely(length < 2 || excaps.excaprefs[0] == NULL)) {
            current_syscall_error.type =
                seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        w_rightsMask = getSyscallArg(0, buffer);
        attr = vmAttributesFromWord(getSyscallArg(1, buffer));
        pdCap = excaps.excaprefs[0]->cap;

        if (unlikely(cap_get_capType(pdCap) != cap_page_directory_cap ||
                     !cap_page_directory_cap_get_capPDIsMapped(pdCap))) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(!generic_frame_cap_get_capFIsMapped(cap))) {
            current_syscall_error.type =
                seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(pdCap));
        vaddr = generic_frame_cap_get_capFMappedAddress(cap);

        {
            findPDForASID_ret_t find_ret;

            mappedASID = generic_frame_cap_get_capFMappedASID(cap);

            find_ret = findPDForASID(mappedASID);
            if (unlikely(find_ret.status != EXCEPTION_NONE)) {
                userError("ARMPageRemap: No PD for ASID");
                current_syscall_error.type =
                    seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (unlikely(find_ret.pd != pd ||
                         cap_page_directory_cap_get_capPDMappedASID(pdCap) !=
                         mappedASID)) {
                current_syscall_error.type =
                    seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;

                return EXCEPTION_SYSCALL_ERROR;
            }
        }

        frameSize = generic_frame_cap_get_capFSize(cap);
        capVMRights = generic_frame_cap_get_capFVMRights(cap);
        vmRights =
            maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

        if (unlikely(!checkVPAlignment(frameSize, vaddr))) {
            current_syscall_error.type =
                seL4_AlignmentError;

            return EXCEPTION_SYSCALL_ERROR;
        }

        capFBasePtr = addrFromPPtr((void *)
                                   generic_frame_cap_get_capFBasePtr(cap));

        if (frameSize == ARMSmallPage || frameSize == ARMLargePage) {
            create_mappings_pte_return_t map_ret;
            map_ret = createSafeMappingEntries_PTE(capFBasePtr, vaddr,
                                                   frameSize, vmRights,
                                                   attr, pd);
            if (map_ret.status != EXCEPTION_NONE) {
                return map_ret.status;
            }

            setThreadState(ksCurThread, ThreadState_Restart);
            return performPageInvocationRemapPTE(mappedASID, map_ret.pte,
                                                 map_ret.pte_entries);
        } else {
            create_mappings_pde_return_t map_ret;
            map_ret = createSafeMappingEntries_PDE(capFBasePtr, vaddr,
                                                   frameSize, vmRights,
                                                   attr, pd);
            if (map_ret.status != EXCEPTION_NONE) {
                return map_ret.status;
            }

            setThreadState(ksCurThread, ThreadState_Restart);
            return performPageInvocationRemapPDE(mappedASID, map_ret.pde,
                                                 map_ret.pde_entries);
        }
    }

    case ARMPageUnmap: {
#ifdef CONFIG_ARM_SMMU
        if (isIOSpaceFrame(cap)) {
            return decodeARMIOUnMapInvocation(invLabel, length, cte, cap, excaps);
        } else
#endif
        {
            setThreadState(ksCurThread, ThreadState_Restart);
            return performPageInvocationUnmap(cap, cte);
        }
    }

    case ARMPageMapIO: {
        return decodeARMIOMapInvocation(invLabel, length, cte, cap, excaps, buffer);
    }

    case ARMPageClean_Data:
    case ARMPageInvalidate_Data:
    case ARMPageCleanInvalidate_Data:
    case ARMPageUnify_Instruction: {
        asid_t asid;
        vptr_t vaddr;
        findPDForASID_ret_t pd;
        vptr_t start, end;
        paddr_t pstart;
        word_t page_size;
        word_t page_base;

        if (length < 2) {
            userError("Page Flush: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        asid = generic_frame_cap_get_capFMappedASID(cap);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        /* Must use kernel vaddr in hyp mode. */
        vaddr = generic_frame_cap_get_capFBasePtr(cap);
#else
        vaddr = generic_frame_cap_get_capFMappedAddress(cap);
#endif

        if (unlikely(!generic_frame_cap_get_capFIsMapped(cap))) {
            userError("Page Flush: Frame is not mapped.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        pd = findPDForASID(asid);
        if (unlikely(pd.status != EXCEPTION_NONE)) {
            userError("Page Flush: No PD for ASID");
            current_syscall_error.type =
                seL4_FailedLookup;
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
        page_size = 1 << pageBitsForSize(generic_frame_cap_get_capFSize(cap));
        page_base = addrFromPPtr((void*)generic_frame_cap_get_capFBasePtr(cap));

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
        return performPageFlush(invLabel, pd.pd, asid, start, end - 1, pstart);
    }

    case ARMPageGetAddress: {


        /* Check that there are enough message registers */
        assert(n_msgRegisters >= 1);

        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageGetAddress((void*)generic_frame_cap_get_capFBasePtr(cap));
    }

    default:
        current_syscall_error.type = seL4_IllegalOperation;

        return EXCEPTION_SYSCALL_ERROR;
    }
}

exception_t
decodeARMMMUInvocation(word_t invLabel, word_t length, cptr_t cptr,
                       cte_t *cte, cap_t cap, extra_caps_t excaps,
                       word_t *buffer)
{
    switch (cap_get_capType(cap)) {
    case cap_page_directory_cap:
        return decodeARMPageDirectoryInvocation(invLabel, length, cptr, cte,
                                                cap, excaps, buffer);

    case cap_page_table_cap:
        return decodeARMPageTableInvocation (invLabel, length, cte,
                                             cap, excaps, buffer);

    case cap_small_frame_cap:
    case cap_frame_cap:
        return decodeARMFrameInvocation (invLabel, length, cte,
                                         cap, excaps, buffer);

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

        if (unlikely(length < 2 || excaps.excaprefs[0] == NULL
                     || excaps.excaprefs[1] == NULL)) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        index = getSyscallArg(0, buffer);
        depth = getSyscallArg(1, buffer);
        parentSlot = excaps.excaprefs[0];
        untyped = parentSlot->cap;
        root = excaps.excaprefs[1]->cap;

        /* Find first free pool */
        for (i = 0; i < nASIDPools && armKSASIDTable[i]; i++);

        if (unlikely(i == nASIDPools)) { /* If no unallocated pool is found */
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid_base = i << asidLowBits;

        if (unlikely(cap_get_capType(untyped) != cap_untyped_cap ||
                     cap_untyped_cap_get_capBlockSize(untyped) !=
                     seL4_ASIDPoolBits)) {
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

        setThreadState(ksCurThread, ThreadState_Restart);
        return performASIDControlInvocation(frame, destSlot,
                                            parentSlot, asid_base);
    }

    case cap_asid_pool_cap: {
        cap_t pdCap;
        cte_t *pdCapSlot;
        asid_pool_t *pool;
        word_t i;
        asid_t asid;

        if (unlikely(invLabel != ARMASIDPoolAssign)) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(excaps.excaprefs[0] == NULL)) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pdCapSlot = excaps.excaprefs[0];
        pdCap = pdCapSlot->cap;

        if (unlikely(
                    cap_get_capType(pdCap) != cap_page_directory_cap ||
                    cap_page_directory_cap_get_capPDIsMapped(pdCap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pool = armKSASIDTable[cap_asid_pool_cap_get_capASIDBase(cap) >>
                              asidLowBits];
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
        for (i = 0; i < (1 << asidLowBits) && (asid + i == 0 || pool->array[i]); i++);

        if (unlikely(i == 1 << asidLowBits)) {
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid += i;

        setThreadState(ksCurThread, ThreadState_Restart);
        return performASIDPoolInvocation(asid, pool, pdCapSlot);
    }

    default:
        fail("Invalid ARM arch cap type");
    }
}

#ifdef DEBUG
void kernelPrefetchAbort(word_t pc) VISIBLE;
void kernelDataAbort(word_t pc) VISIBLE;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

void kernelUndefinedInstruction(word_t pc) VISIBLE;

void
kernelPrefetchAbort(word_t pc)
{
    word_t sr = getHSR();

    printf("\n\nKERNEL PREFETCH ABORT!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("HSR: 0x%x\n", (unsigned int)sr);

    halt();
}

void
kernelDataAbort(word_t pc)
{
    word_t far = getHDFAR();
    word_t sr = getHSR();

    printf("\n\nKERNEL DATA ABORT!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("HDFAR: 0x%x HSR: 0x%x\n", (unsigned int)far, (unsigned int)sr);

    halt();
}

void
kernelUndefinedInstruction(word_t pc)
{
    word_t sr = getHSR();

    printf("\n\nKERNEL UNDEFINED INSTRUCTION!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("HSR: 0x%x\n", (unsigned int)sr);

    halt();
}

#else /* CONFIG_ARM_HYPERVISOR_SUPPORT */

void
kernelPrefetchAbort(word_t pc)
{
    word_t UNUSED ifsr = getIFSR();

    printf("\n\nKERNEL PREFETCH ABORT!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("IFSR: 0x%x\n", (unsigned int)ifsr);

    halt();
}

void
kernelDataAbort(word_t pc)
{
    word_t UNUSED dfsr = getDFSR();
    word_t UNUSED far = getFAR();

    printf("\n\nKERNEL DATA ABORT!\n");
    printf("Faulting instruction: 0x%x\n", (unsigned int)pc);
    printf("FAR: 0x%x DFSR: 0x%x\n", (unsigned int)far, (unsigned int)dfsr);

    halt();
}
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#endif

#ifdef CONFIG_PRINTING
typedef struct readWordFromVSpace_ret {
    exception_t status;
    word_t value;
} readWordFromVSpace_ret_t;

static readWordFromVSpace_ret_t
readWordFromVSpace(pde_t *pd, word_t vaddr)
{
    readWordFromVSpace_ret_t ret;
    lookupPTSlot_ret_t ptSlot;
    pde_t *pdSlot;
    paddr_t paddr;
    word_t offset;
    pptr_t kernel_vaddr;
    word_t *value;

    pdSlot = lookupPDSlot(pd, vaddr);
    if (pde_ptr_get_pdeType(pdSlot) == pde_pde_section) {
        paddr = pde_pde_section_ptr_get_address(pdSlot);
        offset = vaddr & MASK(ARMSectionBits);
    } else {
        ptSlot = lookupPTSlot(pd, vaddr);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        if (ptSlot.status == EXCEPTION_NONE && pte_ptr_get_pteType(ptSlot.ptSlot) == pte_pte_small) {
            paddr = pte_pte_small_ptr_get_address(ptSlot.ptSlot);
            if (pte_pte_small_ptr_get_contiguous_hint(ptSlot.ptSlot)) {
                offset = vaddr & MASK(ARMLargePageBits);
            } else {
                offset = vaddr & MASK(ARMSmallPageBits);
            }
#else
        if (ptSlot.status == EXCEPTION_NONE && pte_ptr_get_pteType(ptSlot.ptSlot) == pte_pte_small) {
            paddr = pte_pte_small_ptr_get_address(ptSlot.ptSlot);
            offset = vaddr & MASK(ARMSmallPageBits);
        } else if (ptSlot.status == EXCEPTION_NONE && pte_ptr_get_pteType(ptSlot.ptSlot) == pte_pte_large) {
            paddr = pte_pte_large_ptr_get_address(ptSlot.ptSlot);
            offset = vaddr & MASK(ARMLargePageBits);
#endif
        } else {
            ret.status = EXCEPTION_LOOKUP_FAULT;
            return ret;
        }
    }


    kernel_vaddr = (word_t)paddr_to_pptr(paddr);
    value = (word_t*)(kernel_vaddr + offset);
    ret.status = EXCEPTION_NONE;
    ret.value = *value;
    return ret;
}

void
Arch_userStackTrace(tcb_t *tptr)
{
    cap_t threadRoot;
    pde_t *pd;
    word_t sp;
    int i;

    threadRoot = TCB_PTR_CTE_PTR(tptr, tcbVTable)->cap;

    /* lookup the PD */
    if (cap_get_capType(threadRoot) != cap_page_directory_cap) {
        printf("Invalid vspace\n");
        return;
    }

    pd = (pde_t*)pptr_of_cap(threadRoot);

    sp = getRegister(tptr, SP);
    /* check for alignment so we don't have to worry about accessing
     * words that might be on two different pages */
    if (!IS_ALIGNED(sp, WORD_SIZE_BITS)) {
        printf("SP not aligned\n");
        return;
    }

    for (i = 0; i < CONFIG_USER_STACK_TRACE_LENGTH; i++) {
        word_t address = sp + (i * sizeof(word_t));
        readWordFromVSpace_ret_t result;
        result = readWordFromVSpace(pd, address);
        if (result.status == EXCEPTION_NONE) {
            printf("0x%lx: 0x%lx\n", (long)address, (long)result.value);
        } else {
            printf("0x%lx: INVALID\n", (long)address);
        }
    }
}
#endif

