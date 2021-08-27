/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
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
#include <linker.h>
#include <plat/machine/hardware.h>
#include <machine.h>

#ifdef ENABLE_SMP_SUPPORT
BOOT_BSS static volatile word_t node_boot_lock;
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
    uintptr_t bi = (0 == extra_bi_size) ? 0 : rootserver.extra_bi;
    pptr_t offset = 0;

    /* if there is a DTB, put it in the extra bootinfo block */
    if ((NULL != dtb) && (0 != dtb_size)) {
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

BOOT_CODE static bool_t arch_init_freemem(p_region_t ui_p_reg,
                                          p_region_t dtb_p_reg,
                                          v_region_t it_v_reg,
                                          word_t extra_bi_size_bits)
{
    /* Reserve the kernel image region. */
    if (!reserve_region((p_region_t) {
        .start = kpptr_to_paddr((void *)KERNEL_ELF_BASE),
        .end   = kpptr_to_paddr((void *)ki_end)
    })) {
        printf("ERROR: can't add reserved region for kernel image\n");
        return false;
    }

    /* Reserve the user image region. */
    if (!reserve_region(ui_p_reg)) {
        printf("ERROR: can't add reserved region for user image\n");
        return false;
    }

    /* Reserve the DTB region, it's ignored if the size is zero. */
    if (!reserve_region(dtb_p_reg)) {
        printf("ERROR: can't add reserved region for DTB\n");
        return false;
    }

    /* avail_p_regs comes from the auto-generated code */
    return init_freemem(ARRAY_SIZE(avail_p_regs), avail_p_regs,
                        it_v_reg, extra_bi_size_bits);
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
BOOT_CODE static bool_t try_init_kernel_secondary_core()
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
    word_t  pv_offset,
    vptr_t  v_entry,
    paddr_t dtb_phys_addr,
    word_t  dtb_size
)
{
    cap_t root_cnode_cap;
    cap_t it_pd_cap;
    cap_t it_ap_cap;
    cap_t ipcbuf_cap;
    pptr_t extra_bi_offset = 0;
    create_frames_of_region_ret_t create_frames_ret;
    create_frames_of_region_ret_t extra_bi_ret;

   map_kernel_window();

    /* initialise the CPU */
    init_cpu();

    printf("Bootstrapping kernel\n");

    /* initialize the platform */
    init_plat();

    /* Convert from physical addresses to userland vptrs with the parameter
     * pv_offset, which is defined as:
     *     virt_address + pv_offset = phys_address
     * The offset is usually a positive value, because the virtual address of
     * the user image is a low value and the actually physical address is much
     * greater. However, there is no restrictions on the physical and virtual
     * image location in general. Defining pv_offset as a signed value might
     * seem the intuitive choice how to handle this, but there is are two
     * catches here that break the C rules. We can't cover the full integer
     * range then, and overflows/underflows are well defined for unsigned values
     * only. They are undefined for signed values, even if such operations
     * practically work in many cases due to how compiler and machine implement
     * negative integers using the two's-complement.
     * Assume a 32-bit system with virt_address=0xc0000000 and phys_address=0,
     * then pv_offset would have to be -0xc0000000. This value is not in the
     * 32-bit signed integer range. Calculating '0 - 0xc0000000' using unsigned
     * integers, the result is 0x40000000 after an underflow, the reverse
     * calculation '0xc0000000 + 0x40000000' results in 0 again after overflow.
     * If 0x40000000 is a signed integer, the result is likely the same, but the
     * whole operation is completely undefined by C rules.
     */
    vptr_t ui_virt_start = ui_phys_start - pv_offset
    word_t ui_phys_size = ui_phys_end - ui_phys_start;
    vptr_t ipcbuf_vptr = ui_virt_start + ui_phys_size;
    vptr_t bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);
    vptr_t extra_bi_frame_vptr = bi_frame_vptr + BIT(PAGE_BITS);
    word_t extra_bi_size = 0;

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

    /* make the free memory available to alloc_region() */
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

    /* create the bootinfo frame */
    populate_bi_frame(0, CONFIG_MAX_NUM_NODES, ipcbuf_vptr, extra_bi_size);
    (void)extra_bi_helper(extra_bi_size, dtb_phys_addr, dtb_size);

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

    /* create the initial thread's ASID pool */
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
        .endf  = kpptr_to_paddr(ki_boot_end)
    });
    if (!create_untypeds(
            root_cnode_cap,
            boot_mem_reuse_reg)) {
        printf("ERROR: could not create untypteds for kernel image boot memory\n");
        return false;
    }

    /* no shared-frame caps (RISC-V has no multikernel support) */
    ndks_boot.bi_frame->sharedFrames = S_REG_EMPTY;

    /* finalise the bootinfo frame */
    bi_finalise();

    ksNumCPUs = 1;

    SMP_COND_STATEMENT(clh_lock_init());
    SMP_COND_STATEMENT(release_secondary_cores());

    /* All cores are up now, so there can be concurrency. The kernel booting is
     * supposed to be finished before the secondary cores are released, all the
     * primary has to do now is schedule the initial thread. Currently there is
     * nothing that touches any global data structures, nevertheless we grab the
     * BKL here to play safe. It is released when the kernel is left. */
    NODE_LOCK_SYS;

    printf("Booting all finished, dropped to user space\n");
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
#ifdef ENABLE_SMP_SUPPORT
    ,
    word_t hart_id,
    word_t core_id
#endif
)
{

#ifdef ENABLE_SMP_SUPPORT
    /* The core with the ID 0 will do the bootstrapping, all other cores will
     * just do a lightweight initialization afterwards.
     */
    add_hart_to_core_map(hart_id, core_id);
    if (core_id == 0) {
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
