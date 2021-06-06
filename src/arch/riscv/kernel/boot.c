/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <assert.h>
#include <kernel/boot.h>
#include <machine/io.h>
#include <model/statedata.h>
#include <object/interrupt.h>
#include <arch/machine.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/vspace.h>
#include <arch/benchmark.h>
#include <linker.h>
#include <plat/machine/hardware.h>
#include <machine.h>

/* linker symbol created at the end of boot code/data in kernel image */
extern char ki_boot_end[];

#ifdef ENABLE_SMP_SUPPORT
BOOT_BSS static volatile word_t node_boot_lock;
#endif

/* There are up to 3 reserved regions:
 *   - the kernel image
 *   - optional extra bootinfo, currently a DTB passed by a previous bootloader
 *   - the user image
 */
BOOT_BSS static region_t res_reg[3];

BOOT_CODE static bool_t create_untypeds(cap_t root_cnode_cap, region_t boot_mem_reuse_reg)
{
    seL4_SlotPos   slot_pos_before;
    seL4_SlotPos   slot_pos_after;

    slot_pos_before = ndks_boot.slot_pos_cur;
    create_device_untypeds(root_cnode_cap, slot_pos_before);
    bool_t res = create_kernel_untypeds(root_cnode_cap, boot_mem_reuse_reg, slot_pos_before);

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->untyped = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };
    return res;

}

BOOT_CODE cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t
                                           use_large, bool_t executable)
{
    cap_t cap;
    vm_page_size_t frame_size;

    if (use_large) {
        frame_size = RISCV_Mega_Page;
    } else {
        frame_size = RISCV_4K_Page;
    }

    cap = cap_frame_cap_new(
              asid,                            /* capFMappedASID       */
              pptr,                            /* capFBasePtr          */
              frame_size,                      /* capFSize             */
              wordFromVMRights(VMReadWrite),   /* capFVMRights         */
              0,                               /* capFIsDevice         */
              vptr                             /* capFMappedAddress    */
          );

    map_it_frame_cap(pd_cap, cap);
    return cap;
}

BOOT_CODE static void init_irqs(cap_t root_cnode_cap)
{
    irq_t i;

    for (i = 0; i <= maxIRQ; i++) {
        if (i != irqInvalid) {
            /* IRQ 0 is irqInvalid */
            setIRQState(IRQInactive, i);
        }
    }
    setIRQState(IRQTimer, KERNEL_TIMER_IRQ);
#ifdef ENABLE_SMP_SUPPORT
    setIRQState(IRQIPI, irq_remote_call_ipi);
    setIRQState(IRQIPI, irq_reschedule_ipi);
#endif
    /* provide the IRQ control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIRQControl), cap_irq_control_cap_new());
}

/* ASM symbol for the CPU initialisation trap. */
extern char trap_entry[1];

/* This and only this function initialises the CPU. It does NOT initialise any kernel state. */

#ifdef CONFIG_HAVE_FPU
BOOT_CODE static void init_fpu(void)
{
    set_fs_clean();
    write_fcsr(0);
    disableFpu();
}
#endif

BOOT_CODE static void init_cpu(void)
{

    activate_kernel_vspace();
    /* Write trap entry address to stvec */
    write_stvec((word_t)trap_entry);
    initLocalIRQController();
#ifndef CONFIG_KERNEL_MCS
    initTimer();
#endif

    /* disable FPU access */
    set_fs_off();

#ifdef CONFIG_HAVE_FPU
    init_fpu();
#endif
}

/* This and only this function initialises the platform. It does NOT initialise any kernel state. */

BOOT_CODE static void init_plat(void)
{
    initIRQController();
}


#ifdef ENABLE_SMP_SUPPORT
BOOT_CODE static bool_t try_init_kernel_secondary_core(word_t hart_id, word_t core_id)
{
    while (!node_boot_lock);

    fence_r_rw();

    init_cpu();
    NODE_LOCK_SYS;

    ksNumCPUs++;
    init_core_state(SchedulerAction_ResumeCurrentThread);
    ifence_local();
    return true;
}

BOOT_CODE static void release_secondary_cores(void)
{
    node_boot_lock = 1;
    fence_w_r();

    while (ksNumCPUs != CONFIG_MAX_NUM_NODES) {
        __atomic_signal_fence(__ATOMIC_ACQ_REL);
    }
}

#endif
/* Main kernel initialisation function. */

static BOOT_CODE bool_t try_init_kernel(
    paddr_t ui_p_reg_start,
    paddr_t ui_p_reg_end,
    uint32_t pv_offset,
    vptr_t  v_entry,
    paddr_t dtb_addr_start,
    paddr_t dtb_addr_end
)
{
    map_kernel_window();
    /* initialise the CPU */
    init_cpu();
    printf("Bootstrapping kernel\n");
    /* initialize the platform */
    init_plat();

    word_t extra_bi_size = 0;

    /* We will create up to 3 regions here, ensure there is space */
    SEL4_COMPILE_ASSERT(enough_reserved_regions, ARRAY_SIZE(res_reg) >= 3);
    unsigned int res_reg_cnt = 0;

    /* Add the kernel image to the reserved regions. This may look a bit awkward
     * as our symbols are a reference in the kernel image window, but we want to
     * do all allocations in terms of the main kernel window, so we do some
     * translation.
     */
    res_reg[res_reg_cnt++] = paddr_to_pptr_reg( (p_region_t) {
        .start = kpptr_to_paddr((void *)KERNEL_ELF_BASE),
        .end   = kpptr_to_paddr((void *)ki_end)
    });

    /* If no DTB was provided or the DTB size is zero, skip allocating extra
     * bootinfo. Otherwise mark the DTB region as reserved.
     */
    region_t *dtb_reg = NULL;
    if ((0 != dtb_addr_start) && (dtb_addr_start < dtb_addr_end)) {
        dtb_reg = &res_reg[res_reg_cnt++];
        /* convert physical address to addressable pointer */
        *dtb_reg = paddr_to_pptr_reg( (p_region_t) {
            .start = dtb_addr_start,
            .end = ROUND_UP(dtb_addr_end, PAGE_BITS)
        });
        /* calculate the size the DTB boot info block needs */
        extra_bi_size += write_bootinfo_dtb(NULL, dtb_reg);
    }

    /* Add the user image region to the reserved area. */
    region_t *ui_reg = &res_reg[res_reg_cnt++];

    *ui_reg = paddr_to_pptr_reg( (p_region_t) {
        .start = ui_p_reg_start,
        .end   = ui_p_reg_end
    });

    /* The region of the initial thread is:
     *   user image + ipcbuf + boot info + extra
     * convert from physical addresses to userland vptrs
     */
    vptr_t ipcbuf_vptr = (word_t)(ui_p_reg_end - pv_offset);
    vptr_t bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);
    vptr_t extra_bi_frame_vptr = bi_frame_vptr + BIT(PAGE_BITS);
    word_t extra_bi_size_bits = calculate_extra_bi_size_bits(extra_bi_size);
    v_region_t it_v_reg = {
        .start = (word_t)(ui_p_reg_start - pv_offset),
        .end   = extra_bi_frame_vptr + BIT(extra_bi_size_bits)
    };

    /* now that we have all regions, initialize the free memory */
    init_freemem(get_num_avail_p_regs(), get_avail_p_regs(),
                 res_reg_cnt, res_reg, it_v_reg, extra_bi_size_bits);

    /* create the root cnode */
    cap_t root_cnode_cap = create_root_cnode();
    if (cap_get_capType(root_cnode_cap) == cap_null_cap) {
        printf("ERROR: root c-node creation failed\n");
        return false;
    }

    /* create the cap for managing thread domains */
    create_domain_cap(root_cnode_cap);

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap);

    /* create the bootinfo frame */
    populate_bi_frame(0, CONFIG_MAX_NUM_NODES, ipcbuf_vptr, extra_bi_size);
    pptr_t extra_bi_offset = 0;
    /* put DTB in the bootinfo block, if present. */
    if (dtb_reg) {
        extra_bi_offset += write_bootinfo_dtb(
                            (void *)(rootserver.extra_bi + extra_bi_offset),
                            dtb_reg);
    }

    /* provide a chunk for any leftover padding in the extended boot info */
    if (extra_bi_size > extra_bi_offset) {
        extra_bi_offset += write_bootinfo_padding(
                            (void *)(rootserver.extra_bi + extra_bi_offset),
                            extra_bi_size - extra_bi_offset);
    }

    /* Construct an initial address space with enough virtual addresses
     * to cover the user image + ipc buffer and bootinfo frames */
    cap_t it_pd_cap = create_it_address_space(root_cnode_cap, it_v_reg);
    if (cap_get_capType(it_pd_cap) == cap_null_cap) {
        printf("ERROR: initial thread PD c-node creation failed\n");
        return false;
    }

    /* Create and map bootinfo frame cap */
    create_bi_frame_cap(
        root_cnode_cap,
        it_pd_cap,
        bi_frame_vptr
    );

    /* create and map extra bootinfo region */
    if (extra_bi_size > 0) {
        region_t extra_bi_region = {
            .start = rootserver.extra_bi,
            .end = rootserver.extra_bi + extra_bi_size
        };
        create_frames_of_region_ret_t extra_bi_ret =
            create_frames_of_region(
                root_cnode_cap,
                it_pd_cap,
                extra_bi_region,
                true,
                pptr_to_paddr((void *)extra_bi_region.start) - extra_bi_frame_vptr
            );
        if (!extra_bi_ret.success) {
            printf("ERROR: mapping extra boot info to initial thread failed\n");
            return false;
        }
        ndks_boot.bi_frame->extraBIPages = extra_bi_ret.region;
    }

#ifdef CONFIG_KERNEL_MCS
    init_sched_control(root_cnode_cap, CONFIG_MAX_NUM_NODES);
#endif

    /* create the initial thread's IPC buffer */
    cap_t ipcbuf_cap = create_ipcbuf_frame_cap(root_cnode_cap, it_pd_cap, ipcbuf_vptr);
    if (cap_get_capType(ipcbuf_cap) == cap_null_cap) {
        printf("ERROR: could not create IPC buffer for initial thread\n");
        return false;
    }

    /* create all userland image frames */
    create_frames_of_region_ret_t create_frames_ret =
        create_frames_of_region(
            root_cnode_cap,
            it_pd_cap,
            *ui_reg,
            true,
            pv_offset
        );
    if (!create_frames_ret.success) {
        printf("ERROR: could not create all userland image frames\n");
        return false;
    }
    ndks_boot.bi_frame->userImageFrames = create_frames_ret.region;

    /* create the initial thread's ASID pool */
    cap_t it_ap_cap = create_it_asid_pool(root_cnode_cap);
    if (cap_get_capType(it_ap_cap) == cap_null_cap) {
        printf("ERROR: could not create ASID pool for initial thread\n");
        return false;
    }
    write_it_asid_pool(it_ap_cap, it_pd_cap);

#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
#endif

    /* create the idle thread */
    if (!create_idle_thread()) {
        printf("ERROR: could not create idle thread\n");
        return false;
    }

    /* create the initial thread */
    tcb_t *initial = create_initial_thread(
                         root_cnode_cap,
                         it_pd_cap,
                         v_entry,
                         bi_frame_vptr,
                         ipcbuf_vptr,
                         ipcbuf_cap
                     );

    if (initial == NULL) {
        printf("ERROR: could not create initial thread\n");
        return false;
    }

    init_core_state(initial);

    /* convert the remaining free memory into UT objects and provide the caps */
    region_t boot_mem_reuse_reg = paddr_to_pptr_reg( (p_region_t) {
        .start = kpptr_to_paddr((void *)KERNEL_ELF_BASE),
        .end   = kpptr_to_paddr(ki_boot_end)
    });

    if (!create_untypeds(
            root_cnode_cap,
            boot_mem_reuse_reg)) {
        printf("ERROR: could not create untypteds for kernel image boot memory\n");
        return false;
    }

    /* no shared-frame caps (RISCV has no multikernel support) */
    ndks_boot.bi_frame->sharedFrames = S_REG_EMPTY;

    /* finalise the bootinfo frame */
    bi_finalise();

    ksNumCPUs = 1;

    SMP_COND_STATEMENT(clh_lock_init());
    SMP_COND_STATEMENT(release_secondary_cores());

    printf("Booting all finished, dropped to user space\n");
    return true;
}

BOOT_CODE VISIBLE void init_kernel(
    paddr_t ui_p_reg_start,
    paddr_t ui_p_reg_end,
    sword_t pv_offset,
    vptr_t  v_entry,
    paddr_t dtb_addr_p,
    uint32_t dtb_size
#ifdef ENABLE_SMP_SUPPORT
    ,
    word_t hart_id,
    word_t core_id
#endif
)
{
    bool_t result;
    paddr_t dtb_end_p = 0;

    if (dtb_addr_p) {
        dtb_end_p = dtb_addr_p + dtb_size;
    }

#ifdef ENABLE_SMP_SUPPORT
    add_hart_to_core_map(hart_id, core_id);
    if (core_id == 0) {
        result = try_init_kernel(ui_p_reg_start,
                                 ui_p_reg_end,
                                 pv_offset,
                                 v_entry,
                                 dtb_addr_p,
                                 dtb_end_p);
    } else {
        result = try_init_kernel_secondary_core(hart_id, core_id);
    }
#else
    result = try_init_kernel(ui_p_reg_start,
                             ui_p_reg_end,
                             pv_offset,
                             v_entry,
                             dtb_addr_p,
                             dtb_end_p);
#endif
    if (!result) {
        fail("Kernel init failed for some reason :(");
    }

#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
    NODE_STATE(ksConsumed) = 0;
#endif

    schedule();
    activateThread();
}
