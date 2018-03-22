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
#include <plat/machine/timer.h>
#include <arch/machine/timer.h>
#include <arch/machine/fpu.h>
#include <arch/machine/tlb.h>

/* pointer to the end of boot code/data in kernel image */
/* need a fake array to get the pointer from the linker script */
extern char ki_boot_end[1];
/* pointer to end of kernel image */
extern char ki_end[1];

#ifdef ENABLE_SMP_SUPPORT
/* sync variable to prevent other nodes from booting
 * until kernel data structures initialized */
BOOT_DATA static volatile int node_boot_lock = 0;
#endif /* ENABLE_SMP_SUPPORT */

/**
 * Split mem_reg about reserved_reg. If memory exists in the lower
 * segment, insert it. If memory exists in the upper segment, return it.
 */
BOOT_CODE static region_t
insert_region_excluded(region_t mem_reg, region_t reserved_reg)
{
    region_t residual_reg = mem_reg;
    bool_t result UNUSED;

    if (reserved_reg.start < mem_reg.start) {
        /* Reserved region is below the provided mem_reg. */
        mem_reg.end = 0;
        mem_reg.start = 0;
        /* Fit the residual around the reserved region */
        if (reserved_reg.end > residual_reg.start) {
            residual_reg.start = reserved_reg.end;
        }
    } else if (mem_reg.end > reserved_reg.start) {
        /* Split mem_reg around reserved_reg */
        mem_reg.end = reserved_reg.start;
        residual_reg.start = reserved_reg.end;
    } else {
        /* reserved_reg is completely above mem_reg */
        residual_reg.start = 0;
        residual_reg.end = 0;
    }
    /* Add the lower region if it exists */
    if (mem_reg.start < mem_reg.end) {
        result = insert_region(mem_reg);
        assert(result);
    }
    /* Validate the upper region */
    if (residual_reg.start > residual_reg.end) {
        residual_reg.start = residual_reg.end;
    }

    return residual_reg;
}

BOOT_CODE static region_t
get_reserved_region(int i, pptr_t res_reg_end)
{
    region_t res_reg = mode_reserved_region[i];

    /* Force ordering and exclusivity of reserved regions. */
    assert(res_reg.start < res_reg.end);
    assert(res_reg_end <= res_reg.start);
    return res_reg;
}

BOOT_CODE static int
get_num_reserved_region(void)
{
    return sizeof(mode_reserved_region) / sizeof(region_t);
}

BOOT_CODE static void
init_freemem(region_t ui_reg)
{
    word_t i;
    bool_t result UNUSED;
    region_t cur_reg;
    region_t res_reg[] = {
        {
            .start = kernelBase,
            .end   = (pptr_t)ki_end
        },
        {
            .start = ui_reg.start,
            .end = ui_reg.end
        },
    };

    for (i = 0; i < MAX_NUM_FREEMEM_REG; i++) {
        ndks_boot.freemem[i] = REG_EMPTY;
    }

    /* Force ordering and exclusivity of reserved regions. */
    assert(res_reg[0].start < res_reg[0].end);
    assert(res_reg[1].start < res_reg[1].end);
    assert(res_reg[0].end  <= res_reg[1].start);
    for (i = 0; i < get_num_avail_p_regs(); i++) {
        cur_reg = paddr_to_pptr_reg(get_avail_p_reg(i));
        /* Adjust region if it exceeds the kernel window
         * Note that we compare physical address in case of overflow.
         */
        if (pptr_to_paddr((void*)cur_reg.end) > PADDR_TOP) {
            cur_reg.end = PPTR_TOP;
        }
        if (pptr_to_paddr((void*)cur_reg.start) > PADDR_TOP) {
            cur_reg.start = PPTR_TOP;
        }

        cur_reg = insert_region_excluded(cur_reg, res_reg[0]);
        cur_reg = insert_region_excluded(cur_reg, res_reg[1]);

        /* Check any reserved mode specific reagion */
        region_t mode_res_reg = res_reg[1];
        for (int m = 0; m < get_num_reserved_region(); m++) {
            mode_res_reg = get_reserved_region(i, mode_res_reg.end);
            cur_reg = insert_region_excluded(cur_reg, mode_res_reg);
        }

        if (cur_reg.start != cur_reg.end) {
            result = insert_region(cur_reg);
            assert(result);
        }
    }
}

BOOT_CODE static void
init_irqs(cap_t root_cnode_cap)
{
    irq_t i;

    for (i = 0; i <= maxIRQ; i++) {
        setIRQState(IRQInactive, i);
    }
    setIRQState(IRQTimer, KERNEL_TIMER_IRQ);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    setIRQState(IRQReserved, INTERRUPT_VGIC_MAINTENANCE);
#endif
#ifdef CONFIG_ARM_SMMU
    setIRQState(IRQReserved, INTERRUPT_SMMU);
#endif

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
#ifdef KERNEL_PMU_IRQ
    setIRQState(IRQReserved, KERNEL_PMU_IRQ);
#if (defined CONFIG_PLAT_TX1 && defined ENABLE_SMP_SUPPORT)
//SELFOUR-1252
#error "This platform doesn't support tracking CPU utilisation on multicore"
#endif /* CONFIG_PLAT_TX1 && ENABLE_SMP_SUPPORT */
#else
#error "This platform doesn't support tracking CPU utilisation feature"
#endif /* KERNEL_TIMER_IRQ */
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

#ifdef ENABLE_SMP_SUPPORT
    setIRQState(IRQIPI, irq_remote_call_ipi);
    setIRQState(IRQIPI, irq_reschedule_ipi);
#endif /* ENABLE_SMP_SUPPORT */

    /* provide the IRQ control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIRQControl), cap_irq_control_cap_new());
}

BOOT_CODE static bool_t
create_untypeds(cap_t root_cnode_cap, region_t boot_mem_reuse_reg)
{
    seL4_SlotPos   slot_pos_before;
    seL4_SlotPos   slot_pos_after;
    region_t       dev_reg;
    word_t         i;

    slot_pos_before = ndks_boot.slot_pos_cur;
    create_kernel_untypeds(root_cnode_cap, boot_mem_reuse_reg, slot_pos_before);
    for (i = 0; i < get_num_dev_p_regs(); i++) {
        dev_reg = paddr_to_pptr_reg(get_dev_p_reg(i));
        if (!create_untypeds_for_region(root_cnode_cap, true,
                                        dev_reg, slot_pos_before)) {
            return false;
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->untyped = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };
    return true;

}

/** This and only this function initialises the CPU.
 *
 * It does NOT initialise any kernel state.
 * @return For the verification build, this currently returns true always.
 */
BOOT_CODE static bool_t
init_cpu(void)
{
    bool_t haveHWFPU;

#ifdef CONFIG_ARCH_AARCH64
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        if (!checkTCR_EL2()) {
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
#ifndef CONFIG_ARCH_ARM_V6
    word_t stack_top = ((word_t) kernel_stack_alloc[SMP_TERNARY(getCurrentCPUIndex(), 0)]) + BIT(CONFIG_KERNEL_STACK_BITS);
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_AARCH64)
    /* the least 12 bits are used to store logical core ID */
    stack_top |= getCurrentCPUIndex();
#endif
    setKernelStack(stack_top);
#endif /* CONFIG_ARCH_ARM_V6 */

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
    armv_init_ccnt();
#endif /* CONFIG_ENABLE_BENCHMARKS */

    /* Export selected CPU features for access by PL0 */
    armv_init_user_access();

    initTimer();

    return true;
}

/* This and only this function initialises the platform. It does NOT initialise any kernel state. */

BOOT_CODE static void
init_plat(void)
{
    initIRQController();
    initL2Cache();
}

#ifdef ENABLE_SMP_SUPPORT
BOOT_CODE static bool_t
try_init_kernel_secondary_core(void)
{
    /* need to first wait until some kernel init has been done */
    while (!node_boot_lock);

    /* Perform cpu init */
    init_cpu();

    /* Enable per-CPU timer interrupts */
    maskInterrupt(false, KERNEL_TIMER_IRQ);

    NODE_LOCK_SYS;

    ksNumCPUs++;

    init_core_state(SchedulerAction_ResumeCurrentThread);

    return true;
}

BOOT_CODE static void
release_secondary_cpus(void)
{

    /* release the cpus at the same time */
    node_boot_lock = 1;

#ifndef CONFIG_ARCH_AARCH64
    /* At this point in time the other CPUs do *not* have the seL4 global pd set.
     * However, they still have a PD from the elfloader (which is mapping mmemory
     * as strongly ordered uncached, as a result we need to explicitly clean
     * the cache for it to see the update of node_boot_lock
     *
     * For ARMv8, the elfloader sets the page table entries as inner shareable
     * (so is the attribute of the seL4 global PD) when SMP is enabled, and
     * turns on the cache. Thus, we do not need to clean and invaliate the cache.
     */
    cleanInvalidateL1Caches();
    plat_cleanInvalidateCache();
#endif

    /* Wait until all the secondary cores are done initialising */
    while (ksNumCPUs != CONFIG_MAX_NUM_NODES) {
        /* perform a memory release+acquire to get new values of ksNumCPUs */
        __atomic_signal_fence(__ATOMIC_ACQ_REL);
    }
}
#endif /* ENABLE_SMP_SUPPORT */

/* Main kernel initialisation function. */

static BOOT_CODE bool_t
try_init_kernel(
    paddr_t ui_p_reg_start,
    paddr_t ui_p_reg_end,
    sword_t pv_offset,
    vptr_t  v_entry
)
{
    cap_t root_cnode_cap;
    cap_t it_ap_cap;
    cap_t it_pd_cap;
    cap_t ipcbuf_cap;
    region_t ui_reg = paddr_to_pptr_reg((p_region_t) {
        ui_p_reg_start, ui_p_reg_end
    });
    pptr_t bi_frame_pptr;
    vptr_t bi_frame_vptr;
    vptr_t ipcbuf_vptr;
    create_frames_of_region_ret_t create_frames_ret;

    /* convert from physical addresses to userland vptrs */
    v_region_t ui_v_reg;
    v_region_t it_v_reg;
    ui_v_reg.start = ui_p_reg_start - pv_offset;
    ui_v_reg.end   = ui_p_reg_end   - pv_offset;

    ipcbuf_vptr = ui_v_reg.end;
    bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);

    /* The region of the initial thread is the user image + ipcbuf and boot info */
    it_v_reg.start = ui_v_reg.start;
    it_v_reg.end = bi_frame_vptr + BIT(PAGE_BITS);

    if (it_v_reg.end > kernelBase) {
        printf("Userland image virtual end address too high\n");
        return false;
    }

    /* setup virtual memory for the kernel */
    map_kernel_window();

    /* initialise the CPU */
    if (!init_cpu()) {
        return false;
    }

    /* debug output via serial port is only available from here */
    printf("Bootstrapping kernel\n");

    /* initialise the platform */
    init_plat();

    /* make the free memory available to alloc_region() */
    init_freemem(ui_reg);

    /* create the root cnode */
    root_cnode_cap = create_root_cnode();
    if (cap_get_capType(root_cnode_cap) == cap_null_cap) {
        return false;
    }

    /* create the cap for managing thread domains */
    create_domain_cap(root_cnode_cap);

    /* create the IRQ CNode */
    if (!create_irq_cnode()) {
        return false;
    }

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap);

    /* create the bootinfo frame */
    bi_frame_pptr = allocate_bi_frame(0, CONFIG_MAX_NUM_NODES, ipcbuf_vptr);
    if (!bi_frame_pptr) {
        return false;
    }

    if (config_set(CONFIG_ARM_SMMU)) {
        ndks_boot.bi_frame->ioSpaceCaps = create_iospace_caps(root_cnode_cap);
        if (ndks_boot.bi_frame->ioSpaceCaps.start == 0 &&
                ndks_boot.bi_frame->ioSpaceCaps.end == 0) {
            return false;
        }
    } else {
        ndks_boot.bi_frame->ioSpaceCaps = S_REG_EMPTY;
    }

    /* Construct an initial address space with enough virtual addresses
     * to cover the user image + ipc buffer and bootinfo frames */
    it_pd_cap = create_it_address_space(root_cnode_cap, it_v_reg);
    if (cap_get_capType(it_pd_cap) == cap_null_cap) {
        return false;
    }

    /* Create and map bootinfo frame cap */
    create_bi_frame_cap(
        root_cnode_cap,
        it_pd_cap,
        bi_frame_pptr,
        bi_frame_vptr
    );

    /* create the initial thread's IPC buffer */
    ipcbuf_cap = create_ipcbuf_frame(root_cnode_cap, it_pd_cap, ipcbuf_vptr);
    if (cap_get_capType(ipcbuf_cap) == cap_null_cap) {
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
        return false;
    }
    ndks_boot.bi_frame->userImageFrames = create_frames_ret.region;

    /* create/initialise the initial thread's ASID pool */
    it_ap_cap = create_it_asid_pool(root_cnode_cap);
    if (cap_get_capType(it_ap_cap) == cap_null_cap) {
        return false;
    }
    write_it_asid_pool(it_ap_cap, it_pd_cap);

    /* create the idle thread */
    if (!create_idle_thread()) {
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
        return false;
    }

    init_core_state(initial);

    /* create all of the untypeds. Both devices and kernel window memory */
    if (!create_untypeds(
                root_cnode_cap,
    (region_t) {
    kernelBase, (pptr_t)ki_boot_end
    } /* reusable boot code/data */
            )) {
        return false;
    }

    /* no shared-frame caps (ARM has no multikernel support) */
    ndks_boot.bi_frame->sharedFrames = S_REG_EMPTY;

    /* finalise the bootinfo frame */
    bi_finalise();

    /* make everything written by the kernel visible to userland. Cleaning to PoC is not
     * strictly neccessary, but performance is not critical here so clean and invalidate
     * everything to PoC */
    cleanInvalidateL1Caches();
    invalidateLocalTLB();
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        invalidateHypTLB();
    }


    ksNumCPUs = 1;

    /* initialize BKL before booting up other cores */
    SMP_COND_STATEMENT(clh_lock_init());
    SMP_COND_STATEMENT(release_secondary_cpus());

    /* grab BKL before leaving the kernel */
    NODE_LOCK_SYS;

    printf("Booting all finished, dropped to user space\n");

    /* kernel successfully initialized */
    return true;
}

BOOT_CODE VISIBLE void
init_kernel(
    paddr_t ui_p_reg_start,
    paddr_t ui_p_reg_end,
    sword_t pv_offset,
    vptr_t  v_entry
)
{
    bool_t result;

#ifdef ENABLE_SMP_SUPPORT
    /* we assume there exists a cpu with id 0 and will use it for bootstrapping */
    if (getCurrentCPUIndex() == 0) {
        result = try_init_kernel(ui_p_reg_start,
                                 ui_p_reg_end,
                                 pv_offset,
                                 v_entry);
    } else {
        result = try_init_kernel_secondary_core();
    }

#else
    result = try_init_kernel(ui_p_reg_start,
                             ui_p_reg_end,
                             pv_offset,
                             v_entry);

#endif /* ENABLE_SMP_SUPPORT */

    if (!result) {
        fail ("Kernel init failed for some reason :(");
    }

    schedule();
    activateThread();
}

