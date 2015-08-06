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
#include <kernel/boot.h>
#include <machine.h>
#include <machine/io.h>
#include <model/statedata.h>
#include <object/interrupt.h>
#include <arch/machine.h>
#include <arch/kernel/apic.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/bootinfo.h>
#include <arch/kernel/boot_sys.h>
#include <arch/kernel/vspace.h>
#include <arch/machine/fpu.h>
#include <arch/object/ioport.h>
#include <arch/linker.h>
#include <util.h>

#ifdef CONFIG_IOMMU
#include <plat/machine/intel-vtd.h>
#endif

#ifdef CONFIG_VTX
#include <arch/object/vtx.h>
#endif

/* functions exactly corresponding to abstract specification */

BOOT_CODE static void
init_irqs(cap_t root_cnode_cap, bool_t mask_irqs)
{
    irq_t i;

    for (i = 0; i <= maxIRQ; i++) {
        if (i == irq_timer) {
            setIRQState(IRQTimer, i);
        } else if (i == irq_iommu) {
            setIRQState(IRQReserved, i);
#ifdef CONFIG_IRQ_PIC
        } else if (i == 2) {
            /* cascaded legacy PIC */
            setIRQState(IRQReserved, i);
#endif
        } else if (i >= irq_controller_min && i <= irq_controller_max)
            if (mask_irqs)
                /* Don't use setIRQState() here because it implicitly also enables */
                /* the IRQ on the interrupt controller which only node 0 is allowed to do. */
            {
                intStateIRQTable[i] = IRQReserved;
            } else {
                setIRQState(IRQInactive, i);
            }
        else if (i >= irq_msi_min && i <= irq_msi_max) {
            setIRQState(IRQInactive, i);
        } else if (i >= irq_ipi_min && i <= irq_ipi_max) {
            setIRQState(IRQInactive, i);
        }
    }

    /* provide the IRQ control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IRQ_CTRL), cap_irq_control_cap_new());
}

/* Create a frame cap for the initial thread. */

BOOT_CODE cap_t
create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large)
{
    vm_page_size_t frame_size;

    if (use_large) {
        frame_size = IA32_LargePage;
    } else {
        frame_size = IA32_SmallPage;
    }
    return
        cap_frame_cap_new(
            frame_size,                    /* capFSize           */
            0,                             /* capFMappedObject   */
            0,                             /* capFMappedIndex    */
            IA32_MAPPING_PD,               /* capFMappedType     */
            wordFromVMRights(VMReadWrite), /* capFVMRights       */
            pptr                           /* capFBasePtr        */
        );
}

BOOT_CODE cap_t
create_mapped_it_frame_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, bool_t use_large, bool_t executable)
{
    cap_t cap;
    int shift = PT_BITS + PD_BITS + PAGE_BITS;
    pde_t *pd;
    uint32_t pd_index;
    if (shift == 32) {
        pd = PD_PTR(cap_page_directory_cap_get_capPDBasePtr(vspace_cap));
    } else {
        uint32_t pdpt_index = vptr >> shift;
        pdpte_t *pdpt = PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(vspace_cap));
        pd = paddr_to_pptr(pdpte_get_pd_base_address(pdpt[pdpt_index]));
    }
    pd_index = vptr >> (PT_BITS + PAGE_BITS);

    if (use_large) {
        cap = cap_frame_cap_new(
                  IA32_LargePage,                /* capFSize           */
                  PD_REF(pd),                    /* capFMappedObject   */
                  pd_index,                      /* capFMappedIndex    */
                  IA32_MAPPING_PD,               /* capFMappedType     */
                  wordFromVMRights(VMReadWrite), /* capFVMRights       */
                  pptr                           /* capFBasePtr        */
              );
    } else {
        uint32_t pt_index = (vptr >> PAGE_BITS) & MASK(PT_BITS);
        pte_t *pt = paddr_to_pptr(pde_pde_small_get_pt_base_address(pd[pd_index]));
        cap = cap_frame_cap_new(
                  IA32_SmallPage,                /* capFSize           */
                  PT_REF(pt),                    /* capFMappedObject   */
                  pt_index,                      /* capFMappedIndex    */
                  IA32_MAPPING_PD,               /* capFMappedType     */
                  wordFromVMRights(VMReadWrite), /* capFVMRights       */
                  pptr                           /* capFBasePtr        */
              );
    }
    map_it_frame_cap(cap);
    return cap;
}

/* Create a page table for the initial thread */

static BOOT_CODE cap_t
create_it_page_table_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr)
{
    cap_t cap;
    int shift = PT_BITS + PD_BITS + PAGE_BITS;
    pde_t *pd;
    uint32_t pd_index;
    if (shift == 32) {
        pd = PD_PTR(cap_page_directory_cap_get_capPDBasePtr(vspace_cap));
    } else {
        uint32_t pdpt_index = vptr >> shift;
        pdpte_t *pdpt = PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(vspace_cap));
        pd = paddr_to_pptr(pdpte_get_pd_base_address(pdpt[pdpt_index]));
    }
    pd_index = vptr >> (PT_BITS + PAGE_BITS);
    cap = cap_page_table_cap_new(
              PD_REF(pd),   /* capPTMappedObject */
              pd_index,     /* capPTMappedIndex  */
              pptr          /* capPTBasePtr      */
          );
    map_it_pt_cap(cap);
    return cap;
}

static BOOT_CODE cap_t
create_it_page_directory_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr)
{
    cap_t cap;
    int shift = PT_BITS + PD_BITS + PAGE_BITS;
    uint32_t pdpt_index;
    pdpte_t *pdpt;
    if (shift == 32) {
        pdpt = NULL;
        pdpt_index = 0;
    } else {
        pdpt = PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(vspace_cap));
        pdpt_index = vptr >> shift;
    }
    cap = cap_page_directory_cap_new(
              PDPT_REF(pdpt),   /* capPDMappedObject */
              pdpt_index,       /* capPDMappedIndex  */
              pptr              /* capPDBasePtr      */
          );
    if (cap_get_capType(vspace_cap) != cap_null_cap) {
        map_it_pd_cap(cap);
    }
    return cap;
}

/* Create an address space for the initial thread.
 * This includes page directory and page tables */
BOOT_CODE static cap_t
create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg)
{
    cap_t      vspace_cap;
    vptr_t     vptr;
    pptr_t     pptr;
    slot_pos_t slot_pos_before;
    slot_pos_t slot_pos_after;

    slot_pos_before = ndks_boot.slot_pos_cur;
    if (PDPT_BITS == 0) {
        cap_t pd_cap;
        pptr_t pd_pptr;
        /* just create single PD obj and cap */
        pd_pptr = alloc_region(PD_SIZE_BITS);
        if (!pd_pptr) {
            return cap_null_cap_new();
        }
        memzero(PDE_PTR(pd_pptr), 1 << PD_SIZE_BITS);
        copyGlobalMappings(PDE_PTR(pd_pptr));
        pd_cap = create_it_page_directory_cap(cap_null_cap_new(), pd_pptr, 0);
        if (!provide_cap(root_cnode_cap, pd_cap)) {
            return cap_null_cap_new();
        }
        write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_VSPACE), pd_cap);
        vspace_cap = pd_cap;
    } else {
        cap_t pdpt_cap;
        pptr_t pdpt_pptr;
        unsigned int i;
        /* create a PDPT obj and cap */
        pdpt_pptr = alloc_region(PDPT_SIZE_BITS);
        if (!pdpt_pptr) {
            return cap_null_cap_new();
        }
        memzero(PDPTE_PTR(pdpt_pptr), 1 << PDPT_SIZE_BITS);
        pdpt_cap = cap_pdpt_cap_new(
                       pdpt_pptr        /* capPDPTBasePtr */
                   );
        /* create all PD objs and caps necessary to cover userland image. For simplicity
         * to ensure we also cover the kernel window we create all PDs */
        for (i = 0; i < BIT(PDPT_BITS); i++) {
            /* The compiler is under the mistaken belief here that this shift could be
             * undefined. However, in the case that it would be undefined this code path
             * is not reachable because PDPT_BITS == 0 (see if statement at the top of
             * this function), so to work around it we must both put in a redundant
             * if statement AND place the shift in a variable. While the variable
             * will get compiled away it prevents the compiler from evaluating
             * the 1 << 32 as a constant when it shouldn't
             * tl;dr gcc evaluates constants even if code is unreachable */
            int shift = (PD_BITS + PT_BITS + PAGE_BITS);
            if (shift != 32) {
                vptr = i << shift;
            } else {
                return cap_null_cap_new();
            }

            pptr = alloc_region(PD_SIZE_BITS);
            if (!pptr) {
                return cap_null_cap_new();
            }
            memzero(PDE_PTR(pptr), 1 << PD_SIZE_BITS);
            if (!provide_cap(root_cnode_cap,
                             create_it_page_directory_cap(pdpt_cap, pptr, vptr))
               ) {
                return cap_null_cap_new();
            }
        }
        /* now that PDs exist we can copy the global mappings */
        copyGlobalMappings(PDPTE_PTR(pdpt_pptr));
        write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_VSPACE), pdpt_cap);
        vspace_cap = pdpt_cap;
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->ui_pd_caps = (slot_region_t) {
        slot_pos_before, slot_pos_after
    };
    /* create all PT objs and caps necessary to cover userland image */
    slot_pos_before = ndks_boot.slot_pos_cur;

    for (vptr = ROUND_DOWN(it_v_reg.start, PT_BITS + PAGE_BITS);
            vptr < it_v_reg.end;
            vptr += BIT(PT_BITS + PAGE_BITS)) {
        pptr = alloc_region(PT_SIZE_BITS);
        if (!pptr) {
            return cap_null_cap_new();
        }
        memzero(PTE_PTR(pptr), 1 << PT_SIZE_BITS);
        if (!provide_cap(root_cnode_cap,
                         create_it_page_table_cap(vspace_cap, pptr, vptr))
           ) {
            return cap_null_cap_new();
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->ui_pt_caps = (slot_region_t) {
        slot_pos_before, slot_pos_after
    };

    return vspace_cap;
}

BOOT_CODE static bool_t
create_device_untypeds(
    cap_t root_cnode_cap,
    dev_p_regs_t *dev_p_regs)
{
    slot_pos_t     slot_pos_before;
    slot_pos_t     slot_pos_after;
    uint32_t       i;

    slot_pos_before = ndks_boot.slot_pos_cur;
    for (i = 0; i < dev_p_regs->count; i++) {
        if (!create_untypeds_for_region(root_cnode_cap, true, paddr_to_pptr_reg(dev_p_regs->list[i]), ndks_boot.bi_frame->ut_obj_caps.start)) {
            return false;
        }
    }
    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->ut_device_obj_caps = (slot_region_t) {
        slot_pos_before, slot_pos_after
    };
    return true;
}

BOOT_CODE static void
create_ia32_bootinfo(ia32_bootinfo_frame_t *bootinfo, vesa_info_t *vesa_info, ia32_mem_region_t* mem_regions)
{
    int i;
    bootinfo->vbe_control_info = vesa_info->vbe_control_info;
    bootinfo->vbe_mode_info = vesa_info->vbe_mode_info;
    bootinfo->vbe_mode = vesa_info->vbe_mode;
    bootinfo->vbe_interface_seg = vesa_info->vbe_interface_seg;
    bootinfo->vbe_interface_off = vesa_info->vbe_interface_off;
    bootinfo->vbe_interface_len = vesa_info->vbe_interface_len;
    for (i = 0; i < CONFIG_MAX_MEM_REGIONS; i++) {
        bootinfo->mem_regions[i] = mem_regions[i];
    }
}

BOOT_CODE static pptr_t
create_arch_bi_frame_cap(
    cap_t root_cnode_cap,
    cap_t pd_cap,
    vptr_t vptr
)
{
    pptr_t pptr;
    cap_t cap;

    pptr = alloc_region(PAGE_BITS);
    if (!pptr) {
        printf("Kernel init failed: could not allocate arch bootinfo frame\n");
        return 0;
    }
    clearMemory((void*)pptr, PAGE_BITS);

    /* create a cap and write it into the root cnode */
    cap = create_mapped_it_frame_cap(pd_cap, pptr, vptr, false, false);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_BI_ARCH_FRAME), cap);

    return pptr;
}

/* This function initialises a node's kernel state. It does NOT initialise the CPU. */

BOOT_CODE bool_t
init_node_state(
    p_region_t    avail_p_reg,
    p_region_t    sh_p_reg,
    dev_p_regs_t* dev_p_regs,
    ui_info_t     ui_info,
    p_region_t    boot_mem_reuse_p_reg,
    node_id_t     node_id,
    uint32_t      num_nodes,
    cpu_id_t*     cpu_list,
    /* parameters below not modeled in abstract specification */
    pdpte_t*      kernel_pdpt,
    pde_t*        kernel_pd,
    pte_t*        kernel_pt,
    vesa_info_t*  vesa_info,
    ia32_mem_region_t* mem_regions
#ifdef CONFIG_IOMMU
    , cpu_id_t      cpu_id,
    uint32_t      num_drhu,
    paddr_t*      drhu_list,
    acpi_rmrr_list_t *rmrr_list
#endif
)
{
    cap_t         root_cnode_cap;
    vptr_t        arch_bi_frame_vptr;
    vptr_t        bi_frame_vptr;
    vptr_t        ipcbuf_vptr;
    cap_t         it_vspace_cap;
    cap_t         ipcbuf_cap;
    pptr_t        bi_frame_pptr;
    pptr_t        arch_bi_frame_pptr;
    create_frames_of_region_ret_t create_frames_ret;
    int i;
#ifdef CONFIG_BENCHMARK
    vm_attributes_t buffer_attr = {{ 0 }};
    uint32_t paddr;
    pde_t pde;
#endif /* CONFIG_BENCHMARK */

    /* convert from physical addresses to kernel pptrs */
    region_t avail_reg          = paddr_to_pptr_reg(avail_p_reg);
    region_t ui_reg             = paddr_to_pptr_reg(ui_info.p_reg);
    region_t sh_reg             = paddr_to_pptr_reg(sh_p_reg);
    region_t boot_mem_reuse_reg = paddr_to_pptr_reg(boot_mem_reuse_p_reg);

    /* convert from physical addresses to userland vptrs */
    v_region_t ui_v_reg;
    v_region_t it_v_reg;
    ui_v_reg.start = ui_info.p_reg.start - ui_info.pv_offset;
    ui_v_reg.end   = ui_info.p_reg.end   - ui_info.pv_offset;

    ipcbuf_vptr = ui_v_reg.end;
    bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);
    arch_bi_frame_vptr = bi_frame_vptr + BIT(PAGE_BITS);

    /* The region of the initial thread is the user image + ipcbuf + boot info and arch boot info */
    it_v_reg.start = ui_v_reg.start;
    it_v_reg.end = arch_bi_frame_vptr + BIT(PAGE_BITS);

    /* make the free memory available to alloc_region() */
    ndks_boot.freemem[0] = avail_reg;
    for (i = 1; i < MAX_NUM_FREEMEM_REG; i++) {
        ndks_boot.freemem[i] = REG_EMPTY;
    }

    /* initialise virtual-memory-related data structures (not in abstract spec) */
    if (!init_vm_state(kernel_pdpt, kernel_pd, kernel_pt)) {
        return false;
    }

#ifdef CONFIG_BENCHMARK
    /* allocate and create the log buffer */
    buffer_attr.words[0] = IA32_PAT_MT_WRITE_THROUGH;

    paddr = pptr_to_paddr((void *) alloc_region(pageBitsForSize(IA32_LargePage)));

    /* allocate a large frame for logging */
    pde = pde_pde_large_new(
              paddr,                                   /* page_base_address    */
              vm_attributes_get_ia32PATBit(buffer_attr),      /* pat                  */
              0,                                       /* avl_cte_depth        */
              1,                                       /* global               */
              0,                                       /* dirty                */
              0,                                       /* accessed             */
              vm_attributes_get_ia32PCDBit(buffer_attr),      /* cache_disabled       */
              vm_attributes_get_ia32PWTBit(buffer_attr),      /* write_through        */
              0,                                       /* super_user           */
              1,                                       /* read_write           */
              1                                        /* present              */
          );

    /* TODO this shouldn't be hardcoded */
    ia32KSkernelPD[IA32_KSLOG_IDX] = pde;


    /* flush the tlb */
    invalidatePageStructureCache();

    /* if we crash here, the log isn't working */
#ifdef CONFIG_DEBUG_BUILD
    printf("Testing log\n");
    ksLog[0] = 0xdeadbeef;
    printf("Wrote to ksLog %x\n", ksLog[0]);
    assert(ksLog[0] == 0xdeadbeef);
#endif /* CONFIG_DEBUG_BUILD */
#endif /* CONFIG_BENCHMARK */

    /* create the root cnode */
    root_cnode_cap = create_root_cnode();

    /* create the IO port cap */
    write_slot(
        SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IO_PORT),
        cap_io_port_cap_new(
            0,                /* first port */
            NUM_IO_PORTS - 1 /* last port  */
        )
    );

    /* create the cap for managing thread domains */
    create_domain_cap(root_cnode_cap);

    /* create the IRQ CNode */
    if (!create_irq_cnode()) {
        return false;
    }

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap, node_id != 0);

    /* create the bootinfo frame */
    bi_frame_pptr = allocate_bi_frame(node_id, num_nodes, ipcbuf_vptr);
    if (!bi_frame_pptr) {
        return false;
    }

    /* Construct an initial address space with enough virtual addresses
     * to cover the user image + ipc buffer and bootinfo frames */
    it_vspace_cap = create_it_address_space(root_cnode_cap, it_v_reg);
    if (cap_get_capType(it_vspace_cap) == cap_null_cap) {
        return false;
    }

    /* Create and map bootinfo frame cap */
    create_bi_frame_cap(
        root_cnode_cap,
        it_vspace_cap,
        bi_frame_pptr,
        bi_frame_vptr
    );

    /* Create and map arch bootinfo frame cap */
    arch_bi_frame_pptr = create_arch_bi_frame_cap(
                             root_cnode_cap,
                             it_vspace_cap,
                             arch_bi_frame_vptr
                         );

    /* create the initial thread's IPC buffer */
    ipcbuf_cap = create_ipcbuf_frame(root_cnode_cap, it_vspace_cap, ipcbuf_vptr);
    if (cap_get_capType(ipcbuf_cap) == cap_null_cap) {
        return false;
    }

    /* create all userland image frames */
    create_frames_ret =
        create_frames_of_region(
            root_cnode_cap,
            it_vspace_cap,
            ui_reg,
            true,
            ui_info.pv_offset
        );
    if (!create_frames_ret.success) {
        return false;
    }
    ndks_boot.bi_frame->ui_frame_caps = create_frames_ret.region;

    /*
     * Initialise the NULL FPU state. This is different from merely zero'ing it
     * out (i.e., the NULL FPU state is non-zero), and must be performed before
     * the first thread is created.
     */
    resetFpu();
    saveFpuState(&ia32KSnullFpuState);
    ia32KSfpuOwner = NULL;

    /* create the idle thread */
    if (!create_idle_thread()) {
        return false;
    }

    /* create the initial thread */
    if (!create_initial_thread(
                root_cnode_cap,
                it_vspace_cap,
                ui_info.v_entry,
                bi_frame_vptr,
                ipcbuf_vptr,
                ipcbuf_cap
            )) {
        return false;
    }

#ifdef CONFIG_IOMMU
    /* initialise VTD-related data structures and the IOMMUs */
    if (!vtd_init(cpu_id, num_drhu, rmrr_list)) {
        return false;
    }

    /* write number of IOMMU PT levels into bootinfo */
    ndks_boot.bi_frame->num_iopt_levels = ia32KSnumIOPTLevels;

    /* write IOSpace master cap */
    if (ia32KSnumDrhu != 0) {
        write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IO_SPACE), master_iospace_cap());
    }
#endif

#ifdef CONFIG_VTX
    /* allow vtx to allocate any memory it may need before we give
       the rest away */
    if (!vtx_allocate()) {
        return false;
    }
#endif

    /* convert the remaining free memory into UT objects and provide the caps */
    if (!create_untypeds(root_cnode_cap, boot_mem_reuse_reg)) {
        return false;
    }
    /* WARNING: alloc_region() must not be called anymore after here! */

    /* create device frames */
    if (!create_device_untypeds(root_cnode_cap, dev_p_regs)) {
        return false;
    }

    /* create all shared frames */
    create_frames_ret =
        create_frames_of_region(
            root_cnode_cap,
            it_vspace_cap,
            sh_reg,
            false,
            0
        );
    if (!create_frames_ret.success) {
        return false;
    }
    ndks_boot.bi_frame->sh_frame_caps = create_frames_ret.region;;

    /* create ia32 specific bootinfo frame */
    create_ia32_bootinfo( (ia32_bootinfo_frame_t*)arch_bi_frame_pptr, vesa_info, mem_regions);

    /* finalise the bootinfo frame */
    bi_finalise();

#if defined DEBUG || defined RELEASE_PRINTF
    ia32KSconsolePort = console_port_of_node(node_id);
    ia32KSdebugPort = debug_port_of_node(node_id);
#endif

    ia32KSNodeID = node_id;
    ia32KSNumNodes = num_nodes;
    ia32KSCPUList = cpu_list;

    /* write IPI cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IPI), cap_ipi_cap_new());

    return true;
}

/* This function initialises the CPU. It does NOT initialise any kernel state. */

BOOT_CODE bool_t
init_node_cpu(
    uint32_t apic_khz,
    bool_t   mask_legacy_irqs
)
{
    /* initialise CPU's descriptor table registers (GDTR, IDTR, LDTR, TR) */
    init_dtrs();

    /* initialise MSRs (needs an initialised TSS) */
    init_sysenter_msrs();

    /* setup additional PAT MSR */
    if (!init_pat_msr()) {
        return false;
    }

    /* initialise floating-point unit */
    Arch_initFpu();

    /* initialise local APIC */
    if (!apic_init(apic_khz, mask_legacy_irqs)) {
        return false;
    }

#ifdef CONFIG_VTX
    /* initialise Intel VT-x extensions */
    vtx_enable();
#endif

#ifdef CONFIG_DEBUG_DISABLE_PREFETCHERS
    if (!disablePrefetchers()) {
        return false;
    }
#endif

    return true;
}
