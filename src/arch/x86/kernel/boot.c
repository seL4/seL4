/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <kernel/boot.h>
#include <machine/io.h>
#include <model/statedata.h>
#include <object/interrupt.h>
#include <arch/object/interrupt.h>
#include <arch/machine.h>
#include <arch/kernel/apic.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/boot_sys.h>
#include <arch/kernel/vspace.h>
#include <machine/fpu.h>
#include <arch/machine/timer.h>
#include <arch/object/ioport.h>
#include <linker.h>
#include <util.h>

#include <plat/machine/intel-vtd.h>

/* functions exactly corresponding to abstract specification */

BOOT_CODE static void init_irqs(cap_t root_cnode_cap)
{
    irq_t i;

    for (i = 0; i <= maxIRQ; i++) {
        if (i == irq_timer) {
            setIRQState(IRQTimer, i);
#ifdef ENABLE_SMP_SUPPORT
        } else if (i == irq_remote_call_ipi || i == irq_reschedule_ipi) {
            setIRQState(IRQIPI, i);
#endif /* ENABLE_SMP_SUPPORT */
#ifdef CONFIG_IOMMU
        } else if (i == irq_iommu) {
            setIRQState(IRQReserved, i);
#endif
        } else if (i == 2 && config_set(CONFIG_IRQ_PIC)) {
            /* cascaded legacy PIC */
            setIRQState(IRQReserved, i);
        } else if (i >= irq_isa_min && i <= irq_isa_max) {
            if (config_set(CONFIG_IRQ_PIC)) {
                setIRQState(IRQInactive, i);
            } else {
                setIRQState(IRQReserved, i);
            }
        } else if (i >= irq_user_min && i <= irq_user_max) {
            if (config_set(CONFIG_IRQ_IOAPIC)) {
                setIRQState(IRQInactive, i);
            } else {
                setIRQState(IRQReserved, i);
            }
        } else {
            setIRQState(IRQReserved, i);
        }
    }
    Arch_irqStateInit();
    /* provide the IRQ control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIRQControl), cap_irq_control_cap_new());
}

/* This function has two purposes:
 * - Just calculate the size of the extra boot. This will be done it the
 *   parameter extra_bi_size is set to 0, the function will return the required
 *   size then.
 * - Populate the extra boot info in rootserver.extra_bi. Pass the reserved size
 *   in the parameter extra_bi_size, this is likely the value that has been
 *   calculated before.
 */
BOOT_CODE static word_t extra_bi_helper(word_t extra_bi_size,
                                        seL4_X86_BootInfo_VBE *vbe,
                                        acpi_rsdp_t *acpi_rsdp,
                                        seL4_X86_BootInfo_fb_t *fb_info,
                                        seL4_X86_BootInfo_mmap_t *mb_mmap,
                                        uint32_t tsc_freq)
{
    /* if extra_bi_size is 0, then we just calculate the required length */
    word_t *bi = (0 == extra_bi_size) ? 0 : (void *)rootserver.extra_bi;
    pptr_t offset = 0;

    if (vbe->vbeMode != -1) {
        if (bi)
        {
            void *bi_ptr = (void *)(bi + offset);
            vbe->header.id = SEL4_BOOTINFO_HEADER_X86_VBE;
            vbe->header.len = sizeof(*vbe);
            memcpy(bi_ptr, vbe, sizeof(*vbe));
        }
        offset += sizeof(*vbe); /* struct includes bootinfo header */

    }
    if (acpi_rsdp) {
        offset += add_extra_bootinfo(bi ? (void *)(bi + offset), acpi_rsdp,
                                     SEL4_BOOTINFO_HEADER_X86_ACPI_RSDP,
                                     acpi_rsdp,
                                     sizeof(*acpi_rsdp));

    }

    if (fb_info && fb_info->addr) {
        offset += add_extra_bootinfo(bi ? (void *)(bi + offset), fb_info,
                                     SEL4_BOOTINFO_HEADER_X86_FRAMEBUFFER,
                                     fb_info,
                                     sizeof(*fb_info));
    }

    if (bi)
    {
        void *bi_ptr = (void *)(bi + offset);
        mb_mmap->header.id = SEL4_BOOTINFO_HEADER_X86_MBMMAP;
        mb_mmap->header.len = sizeof(*mb_mmap); /* struct includes header */
        memcpy(bi_ptr, mb_mmap, sizeof(*mb_mmap));
    }
    offset += sizeof(*mb_mmap); /* struct includes bootinfo header */


    offset += add_extra_bootinfo(bi ? (void *)(bi + offset),
                                 SEL4_BOOTINFO_HEADER_X86_TSC_FREQ
                                 &tsc_freq, 4);

    /* provide a chunk for any leftover padding */
    if (extra_bi_size > offset) {
        add_extra_bootinfo_padding(bi ? (void *)(bi + offset),
                                   extra_bi_size - offset);
    }

    return offset;
}

/* This function initialises a node's kernel state. It does NOT initialise the CPU. */

BOOT_CODE bool_t init_sys_state(
    cpu_id_t      cpu_id,
    mem_p_regs_t  *mem_p_regs,
    ui_info_t     ui_info,
    p_region_t    boot_mem_reuse_p_reg,
    /* parameters below not modeled in abstract specification */
    uint32_t      num_drhu,
    paddr_t      *drhu_list,
    acpi_rmrr_list_t *rmrr_list,
    acpi_rsdp_t      *acpi_rsdp,
    seL4_X86_BootInfo_VBE *vbe,
    seL4_X86_BootInfo_mmap_t *mb_mmap,
    seL4_X86_BootInfo_fb_t *fb_info
)
{
    vptr_t        extra_bi_frame_vptr;
    vptr_t        bi_frame_vptr;
    vptr_t        ipcbuf_vptr;
    cap_t         it_vspace_cap;
    cap_t         it_ap_cap;
    cap_t         ipcbuf_cap;
    pptr_t        extra_bi_offset = 0;
    uint32_t      tsc_freq = 0;
    create_frames_of_region_ret_t create_frames_ret;

    /* convert from physical addresses to kernel pptrs */
    region_t ui_reg             = paddr_to_pptr_reg(ui_info.p_reg);

    /* convert from physical addresses to userland vptrs */
    v_region_t ui_v_reg;
    v_region_t it_v_reg;
    ui_v_reg.start = ui_info.p_reg.start - ui_info.pv_offset;
    ui_v_reg.end   = ui_info.p_reg.end   - ui_info.pv_offset;

    ipcbuf_vptr = ui_v_reg.end;
    bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);
    extra_bi_frame_vptr = bi_frame_vptr + BIT(BI_FRAME_SIZE_BITS);

    /* calculate the extra boot info size */
    word_t extra_bi_size = extra_bi_helper(0, vbe, acpi_rsdp, fb_info, mb_mmap, tsc_freq);
    word_t extra_bi_size_bits = calculate_extra_bi_size_bits(extra_bi_size);

    /* The region of the initial thread is the user image + ipcbuf and boot info */
    it_v_reg.start = ui_v_reg.start;
    it_v_reg.end = ROUND_UP(extra_bi_frame_vptr + BIT(extra_bi_size_bits), PAGE_BITS);
#ifdef CONFIG_IOMMU
    /* calculate the number of io pts before initialising memory */
    if (!vtd_init_num_iopts(num_drhu)) {
        return false;
    }
#endif /* CONFIG_IOMMU */

    if (!reserve_region((p_region_t) {
        .start = 0,
        .end   = ui_p_end
    })) {
        printf("ERROR: can't add reserved region for user image\n");
        return false;
    }

    /* Make the free memory available to alloc_region(), create the rootserver
     * objects and the root c-node.
     */
     cap_t root_cnode_cap = init_freemem(mem_p_regs->count, mem_p_regs->list,
                                         it_v_reg, extra_bi_size_bits);
    if (cap_get_capType(root_cnode_cap) == cap_null_cap) {
        printf("ERROR: memory management initialization failed\n");
        return false;
    }

    /* create the IO port cap */
    write_slot(
        SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIOPortControl),
        cap_io_port_control_cap_new()
    );

    tsc_freq = tsc_init();

    /* populate the bootinfo frame */
    populate_bi_frame(0, ksNumCPUs, ipcbuf_vptr, extra_bi_size);
    (void)extra_bi_helper(extra_bi_size, vbe, acpi_rsdp, fb_info, mb_mmap, tsc_freq);

#ifdef CONFIG_KERNEL_MCS
    /* set up sched control for each core */
    init_sched_control(root_cnode_cap, CONFIG_MAX_NUM_NODES);
#endif

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
        bi_frame_vptr
    );

    /* create and map extra bootinfo region */
    if (extra_bi_size > 0) {
        region_t extra_bi_region = {
            .start = rootserver.extra_bi,
            .end = rootserver.extra_bi + BIT(extra_bi_size_bits)
        };
        create_frames_of_region_ret_t extra_bi_ret =
            create_frames_of_region(
                root_cnode_cap,
                it_vspace_cap,
                extra_bi_region
                true,
                pptr_to_paddr((void *)(rootserver.extra_bi - extra_bi_frame_vptr))
        );
        if (!extra_bi_ret.success) {
            return false;
        }
        ndks_boot.bi_frame->extraBIPages = extra_bi_ret.region;
    }

    /* create the initial thread's IPC buffer */
    ipcbuf_cap = create_ipcbuf_frame_cap(root_cnode_cap, it_vspace_cap, ipcbuf_vptr);
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
    ndks_boot.bi_frame->userImageFrames = create_frames_ret.region;

    /* create the initial thread's ASID pool */
    it_ap_cap = create_it_asid_pool(root_cnode_cap);
    if (cap_get_capType(it_ap_cap) == cap_null_cap) {
        return false;
    }
    write_it_asid_pool(it_ap_cap, it_vspace_cap);

#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
#endif

    /* create the idle thread */
    if (!create_idle_thread()) {
        return false;
    }

    /* create the initial thread */
    tcb_t *initial = create_initial_thread(root_cnode_cap,
                                           it_vspace_cap,
                                           ui_info.v_entry,
                                           bi_frame_vptr,
                                           ipcbuf_vptr,
                                           ipcbuf_cap);
    if (initial == NULL) {
        return false;
    }
    init_core_state(initial);

#ifdef CONFIG_IOMMU
    /* initialise VTD-related data structures and the IOMMUs */
    if (!vtd_init(cpu_id, rmrr_list)) {
        return false;
    }

    /* write number of IOMMU PT levels into bootinfo */
    ndks_boot.bi_frame->numIOPTLevels = x86KSnumIOPTLevels;

    /* write IOSpace master cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIOSpace), master_iospace_cap());
#else
    ndks_boot.bi_frame->numIOPTLevels = -1;
#endif

    /* create all of the untypeds, both devices and kernel window memory */
    assert(boot_mem_reuse_p_reg.end >= boot_mem_reuse_p_reg.start);
    if (!create_untypeds(root_cnode_cap,
                         boot_mem_reuse_p_reg.start,
                         boot_mem_reuse_p_reg.end - boot_mem_reuse_p_reg.start) {
        printf("ERROR: could not create untyped caps\n");
        return false;
    }

    /* finalise the bootinfo frame */
    bi_finalise();

    return true;
}

/* This function initialises the CPU. It does NOT initialise any kernel state. */

BOOT_CODE bool_t init_cpu(
    bool_t   mask_legacy_irqs
)
{
    /* initialise virtual-memory-related data structures */
    if (!init_vm_state()) {
        return false;
    }

    /* initialise CPU's descriptor table registers (GDTR, IDTR, LDTR, TR) */
    init_dtrs();

    if (config_set(CONFIG_SYSENTER)) {
        /* initialise MSRs (needs an initialised TSS) */
        init_sysenter_msrs();
    } else if (config_set(CONFIG_SYSCALL)) {
        init_syscall_msrs();
    } else {
        return false;
    }

    /* setup additional PAT MSR */
    if (!init_pat_msr()) {
        return false;
    }

    /* enable the Write Protect bit in cr0. This prevents the kernel from writing to
     * read only memory, which we shouldn't do under correct execution */
    write_cr0(read_cr0() | CR0_WRITE_PROTECT);

    /* check for SMAP and SMEP and enable */
    cpuid_007h_ebx_t ebx_007;
    ebx_007.words[0] = x86_cpuid_ebx(0x7, 0);
    if (cpuid_007h_ebx_get_smap(ebx_007)) {
        /* if we have user stack trace enabled or dangerous code injection then we cannot
         * enable this as SMAP will make them fault. */
        if (!config_set(CONFIG_PRINTING) && !config_set(CONFIG_DANGEROUS_CODE_INJECTION)) {
            write_cr4(read_cr4() | CR4_SMAP);
        }
    }
    if (cpuid_007h_ebx_get_smep(ebx_007)) {
        /* similar to smap we cannot enable smep if using dangerous code injection. it
         * does not affect stack trace printing though */
        if (!config_set(CONFIG_DANGEROUS_CODE_INJECTION)) {
            write_cr4(read_cr4() | CR4_SMEP);
        }
    }

    if (!init_ibrs()) {
        return false;
    }

#ifdef CONFIG_HARDWARE_DEBUG_API
    /* Initialize hardware breakpoints */
    Arch_initHardwareBreakpoints();
#endif

    /* initialise floating-point unit */
    if (!Arch_initFpu()) {
        return false;
    }

    /* initialise local APIC */
    if (!apic_init(mask_legacy_irqs)) {
        return false;
    }

#ifdef CONFIG_DEBUG_DISABLE_PREFETCHERS
    if (!disablePrefetchers()) {
        return false;
    }
#endif

    if (config_set(CONFIG_EXPORT_PMC_USER)) {
        enablePMCUser();
    }

#ifdef CONFIG_VTX
    /* initialise Intel VT-x extensions */
    if (!vtx_init()) {
        return false;
    }
#endif

    return true;
}
