/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, HENSOLDT Cyber
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
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

#if defined(CONFIG_HAVE_CHERI)
#include <mode/cheri.h>
#endif

#ifdef ENABLE_SMP_SUPPORT
/* SMP boot synchronization works based on a global variable with the initial
 * value 0, as the loader must zero all BSS variables. Secondary cores keep
 * spinning until the primary core has initialized all kernel structures and
 * then set it to 1.
 */
BOOT_BSS static volatile int node_boot_lock;
#endif /* ENABLE_SMP_SUPPORT */

BOOT_BSS static region_t reserved[NUM_RESERVED_REGIONS];

BOOT_CODE static bool_t arch_init_freemem(p_region_t ui_p_reg,
                                          p_region_t dtb_p_reg,
                                          v_region_t it_v_reg,
                                          word_t extra_bi_size_bits)
{
    /* reserve the kernel image region */
    reserved[0] = paddr_to_pptr_reg(get_p_reg_kernel_img());

    int index = 1;

    /* add the dtb region, if it is not empty */
    if (dtb_p_reg.start) {
        if (index >= ARRAY_SIZE(reserved)) {
            printf("ERROR: no slot to add DTB to reserved regions\n");
            return false;
        }
        reserved[index].start = (pptr_t)paddr_to_pptr(dtb_p_reg.start);
        reserved[index].end = (pptr_t)paddr_to_pptr(dtb_p_reg.end);
        index++;
    }

    /* Reserve the user image region and the mode-reserved regions. For now,
     * only one mode-reserved region is supported, because this is all that is
     * needed.
     */
    if (MODE_RESERVED > 1) {
        printf("ERROR: MODE_RESERVED > 1 unsupported!\n");
        return false;
    }
    if (ui_p_reg.start < PADDR_TOP) {
        region_t ui_reg = paddr_to_pptr_reg(ui_p_reg);
        if (MODE_RESERVED == 1) {
            if (index + 1 >= ARRAY_SIZE(reserved)) {
                printf("ERROR: no slot to add the user image and the "
                       "mode-reserved region to the reserved regions\n");
                return false;
            }
            if (ui_reg.end > mode_reserved_region[0].start) {
                reserved[index] = mode_reserved_region[0];
                index++;
                reserved[index] = ui_reg;
            } else {
                reserved[index] = ui_reg;
                index++;
                reserved[index] = mode_reserved_region[0];
            }
            index++;
        } else {
            if (index >= ARRAY_SIZE(reserved)) {
                printf("ERROR: no slot to add the user image to the reserved"
                       "regions\n");
                return false;
            }
            reserved[index] = ui_reg;
            index++;
        }
    } else {
        if (MODE_RESERVED == 1) {
            if (index >= ARRAY_SIZE(reserved)) {
                printf("ERROR: no slot to add the mode-reserved region\n");
                return false;
            }
            reserved[index] = mode_reserved_region[0];
            index++;
        }

        /* Reserve the ui_p_reg region still so it doesn't get turned into device UT. */
        reserve_region(ui_p_reg);
    }

    /* avail_p_regs comes from the auto-generated code */
    return init_freemem(ARRAY_SIZE(avail_p_regs), avail_p_regs,
                        index, reserved,
                        it_v_reg, extra_bi_size_bits);
}


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

#ifdef CONFIG_ALLOW_SMC_CALLS
BOOT_CODE static void init_smc(cap_t root_cnode_cap)
{
    /* Provide the SMC cap*/
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapSMC), cap_smc_cap_new(0));
}
#endif

/** This and only this function initialises the CPU.
 *
 * It does NOT initialise any kernel state.
 * @return For the verification build, this currently returns true always.
 */
BOOT_CODE static bool_t init_cpu(void)
{
    bool_t haveHWFPU;

#if defined(CONFIG_PRINTING)
    init_console();
#endif /* CONFIG_PRINTING */

#ifdef CONFIG_ARCH_AARCH64
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        if (!checkTCR_EL2()) {
            return false;
        }
    }
#endif

    activate_kernel_vspace();
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
    pptr_t stack_top = ((pptr_t) kernel_stack_alloc[CURRENT_CPU_INDEX()]) + BIT(CONFIG_KERNEL_STACK_BITS);
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_AARCH64)
    /* the least 12 bits are used to store logical core ID */
    stack_top |= getCurrentCPUIndex();
#endif
    setKernelStack(stack_top);

#ifdef CONFIG_ARCH_AARCH64
    /* initialise CPU's exception vector table */
#if defined(__CHERI_PURE_CAPABILITY__)
    /* Need to re-build an unsealed exception entry capability */
    setVtable((pptr_t) cheri_build_code_cap_unbounded((ptraddr_t)arm_vector_table,
        __CHERI_CAP_PERMISSION_ACCESS_SYSTEM_REGISTERS__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
        __ARM_CAP_PERMISSION_MUTABLE_LOAD__ |
        __ARM_CAP_PERMISSION_EXECUTIVE__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__));
#else
    setVtable((pptr_t)arm_vector_table);
#endif
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
        printf("Platform claims to have FP hardware, but does not!\n");
        return false;
    }
#endif /* CONFIG_HAVE_FPU */

    cpu_initLocalIRQController();

#ifdef CONFIG_ENABLE_BENCHMARKS
    arm_init_ccnt();
#endif /* CONFIG_ENABLE_BENCHMARKS */

#if defined(CONFIG_PRINTING)
    init_console();
#endif /* CONFIG_PRINTING */

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
    init_cpu();

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

    clock_sync_test();
    ksNumCPUs++;

    init_core_state(SchedulerAction_ResumeCurrentThread);

    return true;
}

BOOT_CODE static void release_secondary_cpus(void)
{
    /* release the cpus at the same time */
    assert(0 == node_boot_lock); /* Sanity check for a proper lock state. */
    node_boot_lock = 1;

    /*
     * At this point in time the primary core (executing this code) already uses
     * the seL4 MMU/cache setup. However, the secondary cores are still using
     * the elfloader's MMU/cache setup, and thus any memory updates may not
     * be visible there.
     *
     * On AARCH64, both elfloader and seL4 map memory inner shareable and have
     * the caches enabled, so no explicit cache maintenance is necessary.
     *
     * On AARCH32 the elfloader uses strongly ordered uncached memory, but seL4
     * has caching enabled, thus explicit cache cleaning is required.
     */
#ifdef CONFIG_ARCH_AARCH32
    cleanInvalidateL1Caches();
    plat_cleanInvalidateL2Cache();
#endif

    /* Wait until all the secondary cores are done initialising */
    while (ksNumCPUs != CONFIG_MAX_NUM_NODES) {
#ifdef ENABLE_SMP_CLOCK_SYNC_TEST_ON_BOOT
        NODE_STATE(ksCurTime) = getCurrentTime();
#endif
        /* perform a memory acquire to get new values of ksNumCPUs, release for ksCurTime */
        __atomic_thread_fence(__ATOMIC_ACQ_REL);
    }
}
#endif /* ENABLE_SMP_SUPPORT */

/* Main kernel initialisation function. */

static BOOT_CODE bool_t try_init_kernel(
    paddr_t ui_p_reg_start,
    paddr_t ui_p_reg_end,
    sword_t pv_offset,
    vptr_t  v_entry,
    paddr_t dtb_phys_addr,
    word_t  dtb_size
)
{
    cap_t root_cnode_cap;
    cap_t it_ap_cap;
    cap_t it_pd_cap;
    cap_t ipcbuf_cap;
    p_region_t ui_p_reg = (p_region_t) {
        ui_p_reg_start, ui_p_reg_end
    };
    region_t ui_reg = paddr_to_pptr_reg(ui_p_reg);
    word_t extra_bi_size = 0;
    word_t extra_bi_offset = 0;
    vptr_t extra_bi_frame_vptr;
    vptr_t bi_frame_vptr;
    vptr_t ipcbuf_vptr;
    create_frames_of_region_ret_t create_frames_ret;
    create_frames_of_region_ret_t extra_bi_ret;

    /* convert from physical addresses to userland vptrs */
    v_region_t ui_v_reg = {
        .start = ui_p_reg_start - pv_offset,
        .end   = ui_p_reg_end   - pv_offset
    };

    ipcbuf_vptr = ui_v_reg.end;
    bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);
    extra_bi_frame_vptr = bi_frame_vptr + BIT(seL4_BootInfoFrameBits);

    /* setup virtual memory for the kernel */
    map_kernel_window();

    /* initialise the CPU */
    if (!init_cpu()) {
        printf("ERROR: CPU init failed\n");
        return false;
    }

    /* debug output via serial port is only available from here */
    printf("Bootstrapping kernel\n");

    /* initialise the platform */
    init_plat();

    /* If a DTB was provided, pass the data on as extra bootinfo */
    p_region_t dtb_p_reg = P_REG_EMPTY;
    if (dtb_size > 0) {
        paddr_t dtb_phys_end = dtb_phys_addr + dtb_size;
        if (dtb_phys_end < dtb_phys_addr) {
            /* An integer overflow happened in DTB end address calculation, the
             * location or size passed seems invalid.
             */
            printf("ERROR: DTB location at %"SEL4_PRIx_word
                   " len %"SEL4_PRIu_word" invalid\n",
                   dtb_phys_addr, dtb_size);
            return false;
        }
        /* If the DTB is located in physical memory that is not mapped in the
         * kernel window we cannot access it.
         */
        if (dtb_phys_end >= PADDR_TOP) {
            printf("ERROR: DTB at [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] "
                   "exceeds PADDR_TOP (%"SEL4_PRIx_word")\n",
                   dtb_phys_addr, dtb_phys_end, (word_t)PADDR_TOP);
            return false;
        }
        /* DTB seems valid and accessible, pass it on in bootinfo. */
        extra_bi_size += sizeof(seL4_BootInfoHeader) + dtb_size;
        /* Remember the memory region it uses. */
        dtb_p_reg = (p_region_t) {
            .start = dtb_phys_addr,
            .end   = dtb_phys_end
        };
    }

#if defined(CONFIG_HAVE_CHERI)
    /* Create CHERI capabilities for purecap user. This includes BootInfo pointer, IPC Buffer pointer,
     * and the entry function pointer to the root task.
     */
    bi_frame_vptr = (vptr_t) cheri_build_user_cap(bi_frame_vptr, BIT(seL4_BootInfoFrameBits)+extra_bi_size,
        __CHERI_CAP_PERMISSION_GLOBAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
        __ARM_CAP_PERMISSION_MUTABLE_LOAD__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE__);

   ipcbuf_vptr = (vptr_t) cheri_build_user_cap(ipcbuf_vptr, sizeof(seL4_IPCBuffer),
        __CHERI_CAP_PERMISSION_GLOBAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
        __ARM_CAP_PERMISSION_MUTABLE_LOAD__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE__);

   v_entry = (vptr_t) __builtin_cheri_address_set(cheri_build_user_cap(0, USER_TOP,
            __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
            __ARM_CAP_PERMISSION_MUTABLE_LOAD__ |
            __CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
            __ARM_CAP_PERMISSION_EXECUTIVE__ |
            __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |
            __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
            __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ |
            __CHERI_CAP_PERMISSION_PERMIT_STORE__ |
            __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__),
        v_entry);
#endif

    /* The region of the initial thread is the user image + ipcbuf and boot info */
    word_t extra_bi_size_bits = calculate_extra_bi_size_bits(extra_bi_size);
    v_region_t it_v_reg = {
        .start = ui_v_reg.start,
        .end   = extra_bi_frame_vptr + BIT(extra_bi_size_bits)
    };
    if (it_v_reg.end >= USER_TOP) {
        /* Variable arguments for printf() require well defined integer types to
         * work properly. Unfortunately, the definition of USER_TOP differs
         * between platforms (int, long), so we have to cast here to play safe.
         */
        printf("ERROR: userland image virt [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]"
               "exceeds USER_TOP (%"SEL4_PRIx_word")\n",
               (word_t)it_v_reg.start, (word_t)it_v_reg.end, (word_t)USER_TOP);
        return false;
    }

    if (!arch_init_freemem(ui_p_reg, dtb_p_reg, it_v_reg, extra_bi_size_bits)) {
        printf("ERROR: free memory management initialization failed\n");
        return false;
    }

    /* create the root cnode */
    root_cnode_cap = create_root_cnode();
    if (cap_get_capType(root_cnode_cap) == cap_null_cap) {
        printf("ERROR: root c-node creation failed\n");
        return false;
    }

    /* create the cap for managing thread domains */
    create_domain_cap(root_cnode_cap);

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap);

#ifdef CONFIG_ARM_SMMU
    /* initialise the SMMU and provide the SMMU control caps*/
    init_smmu(root_cnode_cap);
#endif
#ifdef CONFIG_ALLOW_SMC_CALLS
    init_smc(root_cnode_cap);
#endif

    populate_bi_frame(0, CONFIG_MAX_NUM_NODES, ipcbuf_vptr, extra_bi_size);

    /* put DTB in the bootinfo block, if present. */
    seL4_BootInfoHeader header;
    if (dtb_size > 0) {
        header.id = SEL4_BOOTINFO_HEADER_FDT;
        header.len = sizeof(header) + dtb_size;
        *(seL4_BootInfoHeader *)(rootserver.extra_bi + extra_bi_offset) = header;
        extra_bi_offset += sizeof(header);
        memcpy((void *)(rootserver.extra_bi + extra_bi_offset),
               (const void *) paddr_to_pptr(dtb_phys_addr),
               dtb_size);
        extra_bi_offset += dtb_size;
    }

    if (extra_bi_size > extra_bi_offset) {
        /* provide a chunk for any leftover padding in the extended boot info */
        header.id = SEL4_BOOTINFO_HEADER_PADDING;
        header.len = (extra_bi_size - extra_bi_offset);
        *(seL4_BootInfoHeader *)(rootserver.extra_bi + extra_bi_offset) = header;
    }

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
            ui_reg,
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
    create_idle_thread();

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

    /* create all of the untypeds. Both devices and kernel window memory */
    if (!create_untypeds(root_cnode_cap)) {
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

BOOT_CODE VISIBLE void init_kernel(
    paddr_t ui_p_reg_start,
    paddr_t ui_p_reg_end,
    sword_t pv_offset,
    vptr_t  v_entry,
    paddr_t dtb_addr_p,
    uint32_t dtb_size
)
{
    bool_t result;

#ifdef ENABLE_SMP_SUPPORT
    /* we assume there exists a cpu with id 0 and will use it for bootstrapping */
    if (getCurrentCPUIndex() == 0) {
        result = try_init_kernel(ui_p_reg_start,
                                 ui_p_reg_end,
                                 pv_offset,
                                 v_entry,
                                 dtb_addr_p, dtb_size);
    } else {
        result = try_init_kernel_secondary_core();
    }

#else
    result = try_init_kernel(ui_p_reg_start,
                             ui_p_reg_end,
                             pv_offset,
                             v_entry,
                             dtb_addr_p, dtb_size);

#endif /* ENABLE_SMP_SUPPORT */

    if (!result) {
        fail("ERROR: kernel init failed");
        UNREACHABLE();
    }

#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
    NODE_STATE(ksConsumed) = 0;
#endif
    schedule();
    activateThread();
}
