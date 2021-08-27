/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <assert.h>
#include <kernel/boot.h>
#include <machine/io.h>
#include <model/statedata.h>
#include <object/interrupt.h>
#include <arch/machine.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/vspace.h>
#include <arch/benchmark.h>
#include <arch/user_access.h>
#include <arch/object/iospace.h>
#include <linker.h>
#include <plat/machine/hardware.h>
#include <machine.h>
#include <arch/machine/timer.h>
#include <arch/machine/fpu.h>
#include <arch/machine/tlb.h>

#ifdef CONFIG_ARM_SMMU
#include <drivers/smmu/smmuv2.h>
#endif

#ifdef ENABLE_SMP_SUPPORT
/* sync variable to prevent other nodes from booting
 * until kernel data structures initialized */
BOOT_BSS static volatile int node_boot_lock;
#endif /* ENABLE_SMP_SUPPORT */

BOOT_CODE static void init_irqs(cap_t root_cnode_cap)
{
    unsigned i;

    for (i = 0; i <= maxIRQ ; i++) {
        setIRQState(IRQInactive, CORE_IRQ_TO_IRQT(0, i));
    }
    setIRQState(IRQTimer, CORE_IRQ_TO_IRQT(0, KERNEL_TIMER_IRQ));
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    setIRQState(IRQReserved, CORE_IRQ_TO_IRQT(0, INTERRUPT_VGIC_MAINTENANCE));
    setIRQState(IRQReserved, CORE_IRQ_TO_IRQT(0, INTERRUPT_VTIMER_EVENT));
#endif
#ifdef CONFIG_TK1_SMMU
    setIRQState(IRQReserved, CORE_IRQ_TO_IRQT(0, INTERRUPT_SMMU));
#endif

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
#ifdef KERNEL_PMU_IRQ
    setIRQState(IRQReserved, CORE_IRQ_TO_IRQT(0, KERNEL_PMU_IRQ));
#if (defined CONFIG_PLAT_TX1 && defined ENABLE_SMP_SUPPORT)
//SELFOUR-1252
#error "This platform doesn't support tracking CPU utilisation on multicore"
#endif /* CONFIG_PLAT_TX1 && ENABLE_SMP_SUPPORT */
#else
#error "This platform doesn't support tracking CPU utilisation feature"
#endif /* KERNEL_TIMER_IRQ */
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

#ifdef ENABLE_SMP_SUPPORT
    setIRQState(IRQIPI, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), irq_remote_call_ipi));
    setIRQState(IRQIPI, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), irq_reschedule_ipi));
#endif

    /* provide the IRQ control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIRQControl), cap_irq_control_cap_new());
}

#ifdef CONFIG_ARM_SMMU
BOOT_CODE static void init_smmu(cap_t root_cnode_cap)
{
    plat_smmu_init();
    /*provide the SID and CB control cap*/
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapSMMUSIDControl), cap_sid_control_cap_new());
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapSMMUCBControl), cap_cb_control_cap_new());
}

#endif

/* This function has two purposes:
 * - Just calculate the size of the extra boot. This will be done it the
 *   parameter extra_bi_size is set to 0, the function will return the required
 *   size then.
 * - Populate the extra boot info in rootserver.extra_bi. Pass the reserved size
 *   in the parameter extra_bi_size, this is likely the value that has been
 *   calculated before.
 */
BOOT_CODE static word_t extra_bi_helper(word_t extra_bi_size,
                                        paddr_t dtb_phys_addr,
                                        word_t dtb_size)
{
    /* if extra_bi_size is 0, then we just calculate the required length */
    word_t bi = (0 == extra_bi_size) ? 0 : rootserver.extra_bi;
    pptr_t offset = 0;

    /* if there is a DTB, put it in the extra bootinfo block */
    if ((NULL != dtb_phys_addr) && (0 != dtb_size)) {

        offset += add_extra_bootinfo(bi ? (void *)(bi + offset) : NULL,
                                     SEL4_BOOTINFO_HEADER_FDT,
                                     paddr_to_pptr(dtb_phys_addr), dtb_size);
    }

    /* provide a chunk for any leftover padding */
    if (extra_bi_size > offset) {
        add_extra_bootinfo_padding(bi ? (void *)(bi + offset) : NULL,
                                   extra_bi_size - offset);
    }

    return offset;
}

/** This and only this function initialises the CPU.
 *
 * It does NOT initialise any kernel state.
 * @return For the verification build, this currently returns true always.
 */
BOOT_CODE static bool_t init_cpu(void)
{
    bool_t haveHWFPU;

#ifdef CONFIG_ARCH_AARCH64
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        if (!checkTCR_EL2()) {
            printf("ERROR: TCR_EL2 check failed\n");
            return false;
        }
    }
#endif

    activate_global_pd();
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_boot_init();
    }

#ifdef CONFIG_HARDWARE_DEBUG_API
    if (!Arch_initHardwareBreakpoints()) {
        printf("Kernel built with CONFIG_HARDWARE_DEBUG_API, but this board doesn't "
               "reliably support it.\n");
        return false;
    }
#endif

    /* Setup kernel stack pointer.
     * On ARM SMP, the array index here is the CPU ID
     */
    word_t stack_top = ((word_t) kernel_stack_alloc[CURRENT_CPU_INDEX()]) + BIT(CONFIG_KERNEL_STACK_BITS);
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_AARCH64)
    /* the least 12 bits are used to store logical core ID */
    stack_top |= getCurrentCPUIndex();
#endif
    setKernelStack(stack_top);

#ifdef CONFIG_ARCH_AARCH64
    /* initialise CPU's exception vector table */
    setVtable((pptr_t)arm_vector_table);
#endif /* CONFIG_ARCH_AARCH64 */

    haveHWFPU = fpsimd_HWCapTest();

    /* Disable FPU to avoid channels where a platform has an FPU but doesn't make use of it */
    if (haveHWFPU) {
        disableFpu();
    }

#ifdef CONFIG_HAVE_FPU
    if (haveHWFPU) {
        if (!fpsimd_init()) {
            return false;
        }
    } else {
        printf("Platform claims to have FP hardware, but does not!");
        return false;
    }
#endif /* CONFIG_HAVE_FPU */

    cpu_initLocalIRQController();

#ifdef CONFIG_ENABLE_BENCHMARKS
    arm_init_ccnt();
#endif /* CONFIG_ENABLE_BENCHMARKS */

    /* Export selected CPU features for access by PL0 */
    armv_init_user_access();

    initTimer();

    return true;
}

/* This and only this function initialises the platform. It does NOT initialise any kernel state. */

BOOT_CODE static void init_plat(void)
{
    initIRQController();
    initL2Cache();
#ifdef CONFIG_ARM_SMMU
    plat_smmu_init();
#endif
}

#ifdef ENABLE_SMP_SUPPORT
BOOT_CODE static bool_t try_init_kernel_secondary_core(void)
{
    /* need to first wait until some kernel init has been done */
    while (!node_boot_lock);

    /* Perform cpu init */
    if (!init_cpu()) {
        printf("ERROR: CPU init failed\n");
        return false;
    }

    for (unsigned int i = 0; i < NUM_PPI; i++) {
        maskInterrupt(true, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), i));
    }
    setIRQState(IRQIPI, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), irq_remote_call_ipi));
    setIRQState(IRQIPI, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), irq_reschedule_ipi));
    /* Enable per-CPU timer interrupts */
    setIRQState(IRQTimer, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), KERNEL_TIMER_IRQ));
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    setIRQState(IRQReserved, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), INTERRUPT_VGIC_MAINTENANCE));
    setIRQState(IRQReserved, CORE_IRQ_TO_IRQT(getCurrentCPUIndex(), INTERRUPT_VTIMER_EVENT));
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
    NODE_LOCK_SYS;

    ksNumCPUs++;

    init_core_state(SchedulerAction_ResumeCurrentThread);

    return true;
}

BOOT_CODE static void release_secondary_cpus(void)
{

    /* release the cpus at the same time */
    node_boot_lock = 1;

#ifndef CONFIG_ARCH_AARCH64
    /* At this point in time the other CPUs do *not* have the seL4 global pd set.
     * However, they still have a PD from the elfloader (which is mapping memory
     * as strongly ordered uncached, as a result we need to explicitly clean
     * the cache for it to see the update of node_boot_lock
     *
     * For ARMv8, the elfloader sets the page table entries as inner shareable
     * (so is the attribute of the seL4 global PD) when SMP is enabled, and
     * turns on the cache. Thus, we do not need to clean and invalidate the cache.
     */
    cleanInvalidateL1Caches();
    plat_cleanInvalidateL2Cache();
#endif

    /* Wait until all the secondary cores are done initialising */
    while (ksNumCPUs != CONFIG_MAX_NUM_NODES) {
        /* perform a memory release+acquire to get new values of ksNumCPUs */
        __atomic_signal_fence(__ATOMIC_ACQ_REL);
    }
}
#endif /* ENABLE_SMP_SUPPORT */

/* Main kernel initialisation function. */
static BOOT_CODE bool_t try_init_kernel(
    word_t ui_phys_start,
    word_t ui_phys_end,
    word_t ui_pv_offset,
    word_t ui_virt_entry,
    word_t dtb_phys_start,
    word_t dtb_size
)
{
    cap_t it_ap_cap;
    cap_t it_pd_cap;
    cap_t ipcbuf_cap;
    pptr_t extra_bi_offset = 0;
    create_frames_of_region_ret_t create_frames_ret;
    create_frames_of_region_ret_t extra_bi_ret;

    /* setup virtual memory for the kernel */
    map_kernel_window();

    /* initialise the CPU */
    if (!init_cpu()) {
        printf("ERROR: CPU init failed\n");
        return false;
    }

    /* debug output via serial port is only available from here */
    hack_enable_prints();
    printf("Bootstrapping kernel\n");

    /* initialise the platform */
    init_plat();

    /* Reserve the kernel image region */
    if (!reserve_region((p_region_t) {
        .start = kpptr_to_paddr((void *)KERNEL_ELF_BASE),
        .end   = kpptr_to_paddr((void *)ki_end)
    })) {
        printf("ERROR: can't add reserved region for kernel image\n");
        return false;
    }

    /* Reserve the user image region. */
    p_region_t ui_p_reg = (p_region_t) {
        .start = ui_p_reg_start,
        .end   = ui_p_reg_end
    };
    if (!reserve_region(ui_p_reg)) {
        printf("ERROR: can't add reserved region for user image\n");
        return false;
    }

#ifdef CONFIG_ARCH_AARCH32

    /* Reserve the HW ASID region. */
    p_region_t p_reg_hw_asid = pptr_to_paddr_reg(hw_asid_region);
    printf("HW ASID region: VA [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"], "
           "PA [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
           hw_asid_region.start, hw_asid_region.end,
           p_reg_hw_asid.start, p_reg_hw_asid.end);

    if (!reserve_region(p_reg_hw_asid)) {
        printf("ERROR: can't add reserved region for HW ASIDs\n");
        return false;
    }

#endif

    /* If a DTB was provided, pass the data on as extra bootinfo */
    p_region_t dtb_p_reg = P_REG_EMPTY;
    if (dtb_size > 0) {
        paddr_t dtb_p_end = ROUND_UP(dtb_phys_addr + dtb_size, PAGE_BITS);
        /* An integer overflow happened in DTB end address calculation, the
         * location or size passed seems invalid.
         */
        if (dtb_p_end < dtb_phys_addr) {
            printf("ERROR: DTB location at %"SEL4_PRIx_word
                   " len %"SEL4_PRIu_word" invalid\n",
                   dtb_phys_addr, dtb_size);
            return false;
        }
        /* If the DTB is located in physical memory that is not mapped in the
         * kernel window we cannot access it.
         */
        if (dtb_p_end >= PADDR_TOP) {
            printf("ERROR: DTB at [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] "
                   "exceeds PADDR_TOP (%"SEL4_PRIx_word")\n",
                   dtb_phys_addr, dtb_p_end, PADDR_TOP);
            return false;
        }
        /* Remember the page aligned memory region it uses. */
        dtb_p_reg = (p_region_t) {
            .start = ROUND_DOWN(dtb_phys_addr, PAGE_BITS),
            .end   = dtb_p_end
        };
        /* Reserve the DTB region */
        if (!reserve_region(dtb_p_reg)) {
            printf("ERROR: can't add reserved region for DTB\n");
            return false;
        }
    }

    word_t extra_bi_size_bits = calculate_extra_bi_size_bits(extra_bi_size);
    vptr_t ui_virt_end  = extra_bi_frame_vptr + BIT(extra_bi_size_bits);
    if (ui_virt_end <= ui_virt_start) {
        printf("ERROR: userland image virt [%p..%p] is invalid\n"
               (void *)ui_virt_start, (void *)ui_virt_end);
        return false;
    }
    if (ui_virt_end >= USER_TOP) {
        printf("ERROR: userland image virt [%p..%p] exceeds USER_TOP (%p)\n",
               (void *)ui_virt_start, (void *)ui_virt_end, (void *)USER_TOP);
        return false;
    }

    v_region_t it_v_reg = {
        .start = ui_virt_start;
        .end   = ui_virt_end;
    };

    /* Make the free memory available to alloc_region(), create the rootserver
     * objects and the root c-node.
     */
    cap_t root_cnode_cap = init_freemem(get_num_avail_p_regs(),
                                        get_avail_p_regs(),
                                        it_v_reg, extra_bi_size_bits);
    if (cap_get_capType(root_cnode_cap) == cap_null_cap) {
        printf("ERROR: memory management initialization failed\n");
        return false;
    }

#ifdef CONFIG_ARM_SMMU
    /* initialise the SMMU and provide the SMMU control caps*/
    init_smmu(root_cnode_cap);
#endif

    /* create the bootinfo frame */
    populate_bi_frame(0, CONFIG_MAX_NUM_NODES, ipcbuf_vptr, extra_bi_size);
    (void)extra_bi_helper(extra_bi_size, dtb_phys_addr, dtb_size);

    if (config_set(CONFIG_TK1_SMMU)) {
        ndks_boot.bi_frame->ioSpaceCaps = create_iospace_caps(root_cnode_cap);
        if (ndks_boot.bi_frame->ioSpaceCaps.start == 0 &&
            ndks_boot.bi_frame->ioSpaceCaps.end == 0) {
            printf("ERROR: SMMU I/O space creation failed\n");
            return false;
        }
    } else {
        ndks_boot.bi_frame->ioSpaceCaps = S_REG_EMPTY;
    }

    /* Construct an initial address space with enough virtual addresses
     * to cover the user image + ipc buffer and bootinfo frames */
    it_pd_cap = create_it_address_space(root_cnode_cap, it_v_reg);
    if (cap_get_capType(it_pd_cap) == cap_null_cap) {
        printf("ERROR: address space creation for initial thread failed\n");
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
        extra_bi_ret =
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
    ipcbuf_cap = create_ipcbuf_frame_cap(root_cnode_cap, it_pd_cap, ipcbuf_vptr);
    if (cap_get_capType(ipcbuf_cap) == cap_null_cap) {
        printf("ERROR: could not create IPC buffer for initial thread\n");
        return false;
    }

    /* create all userland image frames */
    create_frames_ret =
        create_frames_of_region(
            root_cnode_cap,
            it_pd_cap,
            paddr_to_pptr_reg(ui_p_reg),
            true,
            pv_offset
        );
    if (!create_frames_ret.success) {
        printf("ERROR: could not create all userland image frames\n");
        return false;
    }
    ndks_boot.bi_frame->userImageFrames = create_frames_ret.region;

    /* create/initialise the initial thread's ASID pool */
    it_ap_cap = create_it_asid_pool(root_cnode_cap);
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

    /* Before creating the initial thread (which also switches to it)
     * we clean the cache so that any page table information written
     * as a result of calling create_frames_of_region will be correctly
     * read by the hardware page table walker */
    cleanInvalidateL1Caches();

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

    /* create all of the untypeds, both devices and kernel window memory */
    assert(KERNEL_ELF_BASE <= (word_t)ki_boot_end);
    if (!create_untypeds(root_cnode_cap,
                         kpptr_to_paddr((void *)KERNEL_ELF_BASE),
                         (word_t)ki_boot_end - KERNEL_ELF_BASE)) {
        printf("ERROR: could not create untypteds for kernel image boot memory\n");
        return false;
    }

    /* no shared-frame caps (ARM has no multikernel support) */
    ndks_boot.bi_frame->sharedFrames = S_REG_EMPTY;

    /* finalise the bootinfo frame */
    bi_finalise();

    /* Flushing the L1 cache and invalidating the TLB is good enough here to
     * make sure everything written by the kernel is visible to userland. There
     * are no uncached userland frames at this stage that require enforcing
     * flushing to RAM. Any retyping operation will clean the memory down to RAM
     * anyway.
     */
    cleanInvalidateL1Caches();
    invalidateLocalTLB();
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        invalidateHypTLB();
    }

    ksNumCPUs = 1;

    /* initialize BKL before booting up other cores */
    SMP_COND_STATEMENT(clh_lock_init());
    SMP_COND_STATEMENT(release_secondary_cpus());

    /* All cores are up now, so there can be concurrency. The kernel booting is
     * supposed to be finished before the secondary cores are released, all the
     * primary has to do now is schedule the initial thread. Currently there is
     * nothing that touches any global data structures, nevertheless we grab the
     * BKL here to play safe. It is released when the kernel is left. */
    NODE_LOCK_SYS;

    printf("Booting all finished, dropped to user space\n");

    /* kernel successfully initialized */
    return true;
}

/* This is called from assembly code and thus there are no specific types in
 * the signature.
 */
BOOT_CODE VISIBLE void init_kernel(
    word_t ui_p_reg_start,
    word_t ui_p_reg_end,
    word_t pv_offset,
    word_t v_entry,
    word_t dtb_addr_p,
    word_t dtb_size
)
{
#ifdef ENABLE_SMP_SUPPORT
    /* The core with the ID 0 will do the bootstrapping, all other cores will
     * just do a lightweight initialization afterwards.
     */
    if (getCurrentCPUIndex() == 0) {
#endif /* ENABLE_SMP_SUPPORT */
        if (!try_init_kernel(ui_phys_start,
                             ui_phys_end,
                             ui_pv_offset,
                             ui_virt_entry,
                             dtb_phys_addr,
                             dtb_size)) {
            fail("ERROR: kernel init failed");
            UNREACHABLE();
        }
#ifdef ENABLE_SMP_SUPPORT
    } else {
        if (!try_init_kernel_secondary_core()) {
            fail("ERROR: kernel init on secondary core failed");
            UNREACHABLE();
        }
    }
#endif /* ENABLE_SMP_SUPPORT */

#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
    NODE_STATE(ksConsumed) = 0;
#endif
    schedule();
    activateThread();
}
