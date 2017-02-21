/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#include <config.h>
#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/boot.h>
#include <model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/boot.h>
#include <arch/api/invocation.h>
#include <mode/kernel/tlb.h>
#include <arch/kernel/tlb_bitmap.h>

struct lookupPML4Slot_ret {
    exception_t status;
    pml4e_t     *pml4Slot;
};
typedef struct lookupPML4Slot_ret lookupPML4Slot_ret_t;

/* For the boot code we create two windows into the physical address space
 * One is at the same location as the kernel window, and is placed up high
 * The other is a 1-to-1 mapping of the first 512gb of memory. The purpose
 * of this is to have a 1-to-1 mapping for the low parts of memory, so that
 * when we switch paging on, and are still running at physical addresses,
 * we don't explode. Then we also want the high mappings so we can start
 * running at proper kernel virtual addresses */
pml4e_t boot_pml4[BIT(PML4_INDEX_BITS)] ALIGN(BIT(seL4_PageBits)) VISIBLE PHYS_BSS;
pdpte_t boot_pdpt[BIT(PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PageBits)) VISIBLE PHYS_BSS;

/* 'gdt_idt_ptr' is declared globally because of a C-subset restriction.
 * It is only used in init_drts(), which therefore is non-reentrant.
 */
gdt_idt_ptr_t gdt_idt_ptr;

BOOT_CODE bool_t
map_kernel_window(
    uint32_t num_ioapic,
    paddr_t*   ioapic_paddrs,
    uint32_t   num_drhu,
    paddr_t*   drhu_list
)
{

    uint64_t paddr;
    uint64_t vaddr;

#ifdef CONFIG_HUGE_PAGE
    /* using 1 GiB page size */

    /* verify that the kernel window as at the last entry of the PML4 */
    assert(GET_PML4_INDEX(PPTR_BASE) == BIT(PML4_INDEX_BITS) - 1);
    /* verify that the kernel_base is located in the last entry of the PML4,
     * the second last entry of the PDPT, is 1gb aligned and 1gb in size */
    assert(GET_PML4_INDEX(KERNEL_BASE) == BIT(PML4_INDEX_BITS) - 1);
    assert(GET_PDPT_INDEX(KERNEL_BASE) == BIT(PML4_INDEX_BITS) - 2);
    assert(GET_PDPT_INDEX(PPTR_KDEV) == BIT(PML4_INDEX_BITS) - 1);
    assert(IS_ALIGNED(KERNEL_BASE, seL4_HugePageBits));
    assert(IS_ALIGNED(PPTR_KDEV, seL4_HugePageBits));
    /* place the PDPT into the PML4 */
    pml4e_ptr_new(&x64KSGlobalPML4[GET_PML4_INDEX(PPTR_BASE)],
                  0, /* xd */
                  kpptr_to_paddr(x64KSGlobalPDPT),
                  0, /* accessed */
                  0, /* cache_disabled */
                  0, /* write_through */
                  0, /* super_user */
                  1, /* read_write */
                  1  /* present */
                 );
    /* put the 1GB kernel_base mapping into the PDPT */
    pdpte_pdpte_1g_ptr_new(&x64KSGlobalPDPT[GET_PDPT_INDEX(KERNEL_BASE)],
                           0, /* xd */
                           PADDR_BASE,
                           0, /* PAT */
                           1, /* global */
                           0, /* dirty */
                           0, /* accessed */
                           0, /* cache_disabled */
                           0, /* write_through */
                           0, /* super_user */
                           1, /* read_write */
                           1  /* present */
                          );
    /* also map the physical memory into the big kernel window */
    paddr = 0;
    vaddr = PPTR_BASE;
    for (paddr = 0; paddr < PADDR_TOP;
            paddr += BIT(seL4_HugePageBits)) {

        int pdpte_index = GET_PDPT_INDEX(vaddr);
        pdpte_pdpte_1g_ptr_new(&x64KSGlobalPDPT[pdpte_index],
                               0,          /* xd               */
                               paddr,      /* physical address */
                               0,          /* PAT              */
                               1,          /* global           */
                               0,          /* dirty            */
                               0,          /* accessed         */
                               0,          /* cache_disabled   */
                               0,          /* write_through    */
                               0,          /* super_user       */
                               1,          /* read_write       */
                               1           /* present          */
                              );

        vaddr += BIT(seL4_HugePageBits);
    }

    /* put the PD into the PDPT */
    pdpte_pdpte_pd_ptr_new(&x64KSGlobalPDPT[GET_PDPT_INDEX(PPTR_KDEV)],
                           0, /* xd */
                           kpptr_to_paddr(x64KSGlobalPD),
                           0, /* accessed */
                           0, /* cache_disabled */
                           0, /* write_through */
                           0, /* super_user */
                           1, /* read_write */
                           1  /* present */
                          );
    /* put the PT into the PD */
    pde_pde_small_ptr_new(&x64KSGlobalPD[0],
                          0, /* xd */
                          kpptr_to_paddr(x64KSGlobalPT),
                          0, /* accessed */
                          0, /* cache_disabled */
                          0, /* write_through */
                          0, /* super_user */
                          1, /* read_write */
                          1  /* present */
                         );
#else

    int pd_index = 0;
    /* use 2 MiB page size */
    /* verify that the kernel window as at the last entry of the PML4 */
    assert(GET_PML4_INDEX(PPTR_BASE) == BIT(PML4_INDEX_BITS) - 1);
    /* verify that the kernel_base is located in the last entry of the PML4,
     * the second last entry of the PDPT, is 1gb aligned and 1gb in size */
    assert(GET_PML4_INDEX(KERNEL_BASE) == BIT(PML4_INDEX_BITS) - 1);
    assert(GET_PDPT_INDEX(KERNEL_BASE) == BIT(PML4_INDEX_BITS) - 2);
    assert(GET_PDPT_INDEX(PPTR_KDEV) == BIT(PML4_INDEX_BITS) - 1);
    assert(IS_ALIGNED(KERNEL_BASE, seL4_HugePageBits));
    assert(IS_ALIGNED(PPTR_KDEV, seL4_HugePageBits));

    /* place the PDPT into the PML4 */
    pml4e_ptr_new(&x64KSGlobalPML4[GET_PML4_INDEX(PPTR_BASE)],
                  0, /* xd */
                  kpptr_to_paddr(x64KSGlobalPDPT),
                  0, /* accessed */
                  0, /* cache_disabled */
                  0, /* write_through */
                  0, /* super_user */
                  1, /* read_write */
                  1  /* present */
                 );

    for (pd_index = 0; pd_index < PADDR_TOP >> seL4_HugePageBits; pd_index++) {
        /* put the 1GB kernel_base mapping into the PDPT */
        pdpte_pdpte_pd_ptr_new(&x64KSGlobalPDPT[GET_PDPT_INDEX(PPTR_BASE) + pd_index],
                               0, /* xd */
                               kpptr_to_paddr(&x64KSGlobalPDs[pd_index][0]),
                               0, /* accessed */
                               0, /* cache disabled */
                               0, /* write through */
                               0, /* super user */
                               1, /* read write */
                               1 /* present */
                              );
    }

    pdpte_pdpte_pd_ptr_new(&x64KSGlobalPDPT[GET_PDPT_INDEX(KERNEL_BASE)],
                           0, /* xd */
                           kpptr_to_paddr(&x64KSGlobalPDs[0][0]),
                           0, /* accessed */
                           0, /* cache disable */
                           1, /* write through */
                           0, /* super user */
                           1, /* read write */
                           1  /* present */
                          );

    paddr = 0;
    vaddr = PPTR_BASE;

    for (paddr = 0; paddr < PADDR_TOP;
            paddr += 0x200000) {

        int pd_index = GET_PDPT_INDEX(vaddr) - GET_PDPT_INDEX(PPTR_BASE);
        int pde_index = GET_PD_INDEX(vaddr);

        pde_pde_large_ptr_new(&x64KSGlobalPDs[pd_index][pde_index],
                              0, /* xd */
                              paddr,
                              0, /* pat */
                              1, /* global */
                              0, /* dirty */
                              0, /* accessed */
                              0, /* cache disabled */
                              0, /* write through */
                              0, /* super user */
                              1, /* read write */
                              1  /* present */
                             );
        vaddr += 0x200000;
    }

    /* put the PD into the PDPT */
    pdpte_pdpte_pd_ptr_new(&x64KSGlobalPDPT[GET_PDPT_INDEX(PPTR_KDEV)],
                           0, /* xd */
                           kpptr_to_paddr(&x64KSGlobalPDs[BIT(PDPT_INDEX_BITS) - 1][0]),
                           0, /* accessed */
                           0, /* cache_disabled */
                           0, /* write_through */
                           0, /* super_user */
                           1, /* read_write */
                           1  /* present */
                          );

    /* put the PT into the PD */
    pde_pde_small_ptr_new(&x64KSGlobalPDs[BIT(PDPT_INDEX_BITS) - 1][0],
                          0, /* xd */
                          kpptr_to_paddr(x64KSGlobalPT),
                          0, /* accessed */
                          0, /* cache_disabled */
                          0, /* write_through */
                          0, /* super_user */
                          1, /* read_write */
                          1  /* present */
                         );
#endif

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
    /* use the last PD entry as the benchmark log storage.
     * the actual backing physical memory will be filled
     * later by using alloc_region */
    ksLog = (ks_log_entry_t *)(PPTR_KDEV + 0x200000 * (BIT(PD_INDEX_BITS) - 1));
#endif

    /* now map in the kernel devices */
    if (!map_kernel_window_devices(x64KSGlobalPT, num_ioapic, ioapic_paddrs, num_drhu, drhu_list)) {
        return false;
    }

#if CONFIG_MAX_NUM_NODES > 1
    /* initialize the TLB bitmap */
    tlb_bitmap_init(x64KSGlobalPML4);
#endif /* CONFIG_MAX_NUM_NODES */

    /* In boot code, so fine to just trash everything here */
    invalidateLocalTranslationAll();
    printf("Mapping kernel window is done\n");
    return true;
}

BOOT_CODE void
init_tss(tss_t *tss)
{
    word_t base = (word_t)&MODE_NODE_STATE(x64KSIRQStack)[IRQ_STACK_SIZE];
    tss_ptr_new(
        tss,
        sizeof(*tss),   /* io map base */
        0, 0,       /* ist 7 */
        0, 0,
        0, 0,
        0, 0,
        0, 0,
        0, 0,
        /* ist 1 is the stack frame we use for interrupts */
        base >> 32, base & 0xffffffff,  /* ist 1 */
        0, 0,       /* rsp 2 */
        0, 0,       /* rsp 1 */
        0, 0        /* rsp 0 */
    );
    /* set the IO map to all 1 to block user IN/OUT instructions */
    memset(&ARCH_NODE_STATE(x86KStss).io_map[0], 0xff, sizeof(ARCH_NODE_STATE(x86KStss).io_map));
}

BOOT_CODE void
init_syscall_msrs(void)
{
    x86_wrmsr(IA32_LSTAR_MSR, (uint64_t)&handle_fastsyscall);
    // mask bit 9 in the kernel (which is the interrupt enable bit)
    // also mask bit 8, which is the Trap Flag, to prevent the kernel
    // from single stepping
    x86_wrmsr(IA32_FMASK_MSR, FLAGS_TF | FLAGS_IF);
    x86_wrmsr(IA32_STAR_MSR, ((uint64_t)SEL_CS_0 << 32) | ((uint64_t)SEL_CS_3 << 48));
}

BOOT_CODE void
init_gdt(gdt_entry_t *gdt, tss_t *tss)
{

    uint64_t tss_base = (uint64_t)tss;
    gdt_tss_t gdt_tss;

    gdt[GDT_NULL] = gdt_entry_gdt_null_new();

    gdt[GDT_CS_0] = gdt_entry_gdt_code_new(
                        0,                  /* base high */
                        1,                  /* granularity */
                        0,                  /* operation size, must be 0 when 64-bit is set */
                        1,                  /* long mode */
                        0,                  /* avl */
                        0xf,                /* limit high */
                        1,                  /* present */
                        0,                  /* dpl */
                        1,                  /* always 1 for segment */
                        0,                  /* base middle */
                        0,                  /* base low */
                        0xffff              /* limit low */
                    );

    gdt[GDT_DS_0] = gdt_entry_gdt_data_new(
                        0,                  /* base high */
                        1,                  /* granularity */
                        1,                  /* operation size */
                        0,                  /* avl */
                        0xf,                /* seg limit high */
                        1,                  /* present */
                        0,                  /* dpl */
                        1,                  /* always 1 */
                        0,                  /* base mid */
                        0,                  /* base low */
                        0xffff              /* seg limit low */
                    );

    gdt[GDT_CS_3] = gdt_entry_gdt_code_new(
                        0,                  /* base high */
                        1,                  /* granularity */
                        0,                  /* operation size, must be 0 when 64-bit is set */
                        1,                  /* long mode */
                        0,                  /* avl */
                        0xf,                /* limit high */
                        1,                  /* present */
                        3,                  /* dpl */
                        1,                  /* always 1 */
                        0,                  /* base middle */
                        0,                  /* base low */
                        0xffff              /* limit low */
                    );

    gdt[GDT_DS_3] = gdt_entry_gdt_data_new(
                        0,
                        1,
                        1,
                        0,
                        0xf,
                        1,
                        3,
                        1,
                        0,
                        0,
                        0xffff
                    );

    gdt[GDT_TLS] = gdt_entry_gdt_data_new(
                       0,
                       1,
                       1,
                       0,
                       0xf,
                       1,
                       3,
                       1,
                       0,
                       0,
                       0xffff
                   );

    gdt[GDT_IPCBUF] = gdt_entry_gdt_data_new(
                          0,
                          1,
                          1,
                          0,
                          0xf,
                          1,
                          3,
                          1,
                          0,
                          0,
                          0xffff
                      );

    gdt_tss = gdt_tss_new(
                  tss_base >> 32,                     /* base 63 - 32 */
                  (tss_base & 0xff000000UL) >> 24,    /* base 31 - 24 */
                  1,                                  /* granularity */
                  0,                                  /* avl */
                  0,                                  /* limit high */
                  1,                                  /* present */
                  0,                                  /* dpl */
                  9,                                  /* desc type */
                  (tss_base & 0xff0000UL) >> 16,      /* base 23-16 */
                  (tss_base & 0xffffUL),              /* base 15 - 0 */
                  sizeof(tss_io_t) - 1
              );

    gdt[GDT_TSS].words[0] = gdt_tss.words[0];
    gdt[GDT_TSS + 1].words[0] = gdt_tss.words[1];
}

BOOT_CODE void
init_idt_entry(idt_entry_t *idt, interrupt_t interrupt, void(*handler)(void))
{
    uint64_t handler_addr = (uint64_t)handler;
    uint64_t dpl = 3;

    if (interrupt < int_trap_min && interrupt != int_software_break_request) {
        dpl = 0;
    }

    idt[interrupt] = idt_entry_interrupt_gate_new(
                         handler_addr >> 32,                 /* offset 63 - 32 */
                         ((handler_addr >> 16) & 0xffff),
                         1,                                  /* present */
                         dpl,                                /* dpl */
                         1,                                  /* ist */
                         SEL_CS_0,                           /* segment selector */
                         (handler_addr & 0xffff)               /* offset 15 - 0 */
                     );
}

void setVMRoot(tcb_t *tcb)
{
    cap_t threadRoot;
    asid_t asid;
    pml4e_t *pml4;
    findVSpaceForASID_ret_t find_ret;
    cr3_t cr3;

    threadRoot = TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap;

    if (cap_get_capType(threadRoot) != cap_pml4_cap ||
            !cap_pml4_cap_get_capPML4IsMapped(threadRoot)) {
        setCurrentVSpaceRoot(kpptr_to_paddr(x64KSGlobalPML4), 0);
        return;
    }

    pml4 = PML4E_PTR(cap_pml4_cap_get_capPML4BasePtr(threadRoot));
    asid = cap_pml4_cap_get_capPML4MappedASID(threadRoot);
    find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE || find_ret.vspace_root != pml4)) {
        setCurrentVSpaceRoot(kpptr_to_paddr(x64KSGlobalPML4), 0);
        return;
    }
    cr3 = cr3_new(pptr_to_paddr(pml4), asid);
    if (getCurrentCR3().words[0] != cr3.words[0]) {
        SMP_COND_STATEMENT(tlb_bitmap_set(pml4, getCurrentCPUIndex());)
        setCurrentCR3(cr3, 1);
    }
}


BOOT_CODE void
init_dtrs(void)
{
    gdt_idt_ptr.limit = (sizeof(gdt_entry_t) * GDT_ENTRIES) - 1;
    gdt_idt_ptr.base = (uint64_t)ARCH_NODE_STATE(x86KSgdt);

    /* When we install the gdt it will clobber any value of gs that
     * we have. Since we might be using it for TLS we can stash
     * and unstash any gs value using swapgs
     */
    swapgs();
    x64_install_gdt(&gdt_idt_ptr);
    swapgs();

    gdt_idt_ptr.limit = (sizeof(idt_entry_t) * (int_max + 1 )) - 1;
    gdt_idt_ptr.base = (uint64_t)ARCH_NODE_STATE(x86KSidt);
    x64_install_idt(&gdt_idt_ptr);

    x64_install_ldt(SEL_NULL);

    x64_install_tss(SEL_TSS);
}

BOOT_CODE void
map_it_frame_cap(cap_t pd_cap, cap_t frame_cap)
{
    pml4e_t *pml4 = PML4_PTR(pptr_of_cap(pd_cap));
    pdpte_t *pdpt;
    pde_t *pd;
    pte_t *pt;
    vptr_t vptr = cap_frame_cap_get_capFMappedAddress(frame_cap);
    void *pptr = (void*)cap_frame_cap_get_capFBasePtr(frame_cap);

    assert(cap_frame_cap_get_capFMapType(frame_cap) == X86_MappingVSpace);
    assert(cap_frame_cap_get_capFMappedASID(frame_cap) != asidInvalid);
    pml4 += GET_PML4_INDEX(vptr);
    assert(pml4e_ptr_get_present(pml4));
    pdpt = paddr_to_pptr(pml4e_ptr_get_pdpt_base_address(pml4));
    pdpt += GET_PDPT_INDEX(vptr);
    assert(pdpte_pdpte_pd_ptr_get_present(pdpt));
    pd = paddr_to_pptr(pdpte_pdpte_pd_ptr_get_pd_base_address(pdpt));
    pd += GET_PD_INDEX(vptr);
    assert(pde_pde_small_ptr_get_present(pd));
    pt = paddr_to_pptr(pde_pde_small_ptr_get_pt_base_address(pd));
    pte_ptr_new(
        pt + GET_PT_INDEX(vptr),
        0,                      /* xd                   */
        pptr_to_paddr(pptr),    /* page_base_address    */
        0,                      /* global               */
        0,                      /* pat                  */
        0,                      /* dirty                */
        0,                      /* accessed             */
        0,                      /* cache_disabled       */
        0,                      /* write_through        */
        1,                      /* super_user           */
        1,                      /* read_write           */
        1                       /* present              */
    );
}

static BOOT_CODE void
map_it_pdpt_cap(cap_t vspace_cap, cap_t pdpt_cap)
{
    pml4e_t *pml4 = PML4_PTR(pptr_of_cap(vspace_cap));
    pdpte_t *pdpt = PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(pdpt_cap));
    vptr_t vptr = cap_pdpt_cap_get_capPDPTMappedAddress(pdpt_cap);

    assert(cap_pdpt_cap_get_capPDPTIsMapped(pdpt_cap));
    pml4e_ptr_new(
        pml4 + GET_PML4_INDEX(vptr),
        0,                      /* xd                   */
        pptr_to_paddr(pdpt),    /* pdpt_base_address    */
        0,                      /* accessed             */
        0,                      /* cache_disabled       */
        0,                      /* write_through        */
        1,                      /* super_user           */
        1,                      /* read_write           */
        1                       /* present              */
    );
}

BOOT_CODE void
map_it_pd_cap(cap_t vspace_cap, cap_t pd_cap)
{
    pml4e_t *pml4 = PML4_PTR(pptr_of_cap(vspace_cap));
    pdpte_t *pdpt;
    pde_t *pd = PD_PTR(cap_page_directory_cap_get_capPDBasePtr(pd_cap));
    vptr_t vptr = cap_page_directory_cap_get_capPDMappedAddress(pd_cap);

    assert(cap_page_directory_cap_get_capPDIsMapped(pd_cap));
    pml4 += GET_PML4_INDEX(vptr);
    assert(pml4e_ptr_get_present(pml4));
    pdpt = paddr_to_pptr(pml4e_ptr_get_pdpt_base_address(pml4));
    pdpte_pdpte_pd_ptr_new(
        pdpt + GET_PDPT_INDEX(vptr),
        0,                      /* xd                   */
        pptr_to_paddr(pd),      /* pd_base_address      */
        0,                      /* accessed             */
        0,                      /* cache_disabled       */
        0,                      /* write_through        */
        1,                      /* super_user           */
        1,                      /* read_write           */
        1                       /* present              */
    );
}

BOOT_CODE void
map_it_pt_cap(cap_t vspace_cap, cap_t pt_cap)
{
    pml4e_t *pml4 = PML4_PTR(pptr_of_cap(vspace_cap));
    pdpte_t *pdpt;
    pde_t *pd;
    pte_t *pt = PT_PTR(cap_page_table_cap_get_capPTBasePtr(pt_cap));
    vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(pt_cap);

    assert(cap_page_table_cap_get_capPTIsMapped(pt_cap));
    pml4 += GET_PML4_INDEX(vptr);
    assert(pml4e_ptr_get_present(pml4));
    pdpt = paddr_to_pptr(pml4e_ptr_get_pdpt_base_address(pml4));
    pdpt += GET_PDPT_INDEX(vptr);
    assert(pdpte_pdpte_pd_ptr_get_present(pdpt));
    pd = paddr_to_pptr(pdpte_pdpte_pd_ptr_get_pd_base_address(pdpt));
    pde_pde_small_ptr_new(
        pd + GET_PD_INDEX(vptr),
        0,                      /* xd                   */
        pptr_to_paddr(pt),      /* pt_base_address      */
        0,                      /* accessed             */
        0,                      /* cache_disabled       */
        0,                      /* write_through        */
        1,                      /* super_user           */
        1,                      /* read_write           */
        1                       /* present              */
    );
}

BOOT_CODE void*
map_temp_boot_page(void* entry, uint32_t large_pages)
{
    /* this function is for legacy 32-bit systems where the ACPI tables might
     * collide with the kernel window. Here we just assert that the table is
     * in fact in the lower 4GiB region (which is already 1-to-1 mapped) and
     * continue */
    assert((word_t)entry < BIT(32));
    return entry;
}

static BOOT_CODE cap_t
create_it_pdpt_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_pdpt_cap_new(
              asid,   /* capPDPTMappedASID    */
              pptr,   /* capPDPTBasePtr       */
              1,      /* capPDPTIsMapped      */
              vptr    /* capPDPTMappedAddress */
          );
    map_it_pdpt_cap(vspace_cap, cap);
    return cap;
}

static BOOT_CODE cap_t
create_it_pd_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_directory_cap_new(
              asid,   /* capPDMappedASID      */
              pptr,   /* capPDBasePtr         */
              1,      /* capPDIsMapped        */
              vptr    /* capPDMappedAddress   */
          );
    map_it_pd_cap(vspace_cap, cap);
    return cap;
}

static BOOT_CODE cap_t
create_it_pt_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              asid,   /* capPTMappedASID      */
              pptr,   /* capPTBasePtr         */
              1,      /* capPTIsMapped        */
              vptr    /* capPTMappedAddress   */
          );
    map_it_pt_cap(vspace_cap, cap);
    return cap;
}

BOOT_CODE cap_t
create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg)
{
    cap_t      vspace_cap;
    vptr_t     vptr;
    pptr_t     pptr;
    seL4_SlotPos slot_pos_before;
    seL4_SlotPos slot_pos_after;

    slot_pos_before = ndks_boot.slot_pos_cur;
    /* create the PML4 */
    pptr = alloc_region(seL4_PML4Bits);
    if (!pptr) {
        return cap_null_cap_new();
    }
    memzero(PML4_PTR(pptr), BIT(seL4_PML4Bits));
    copyGlobalMappings(PML4_PTR(pptr));
    vspace_cap = cap_pml4_cap_new(
                     IT_ASID,        /* capPML4MappedASID */
                     pptr,           /* capPML4BasePtr   */
                     1               /* capPML4IsMapped   */
                 );


    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace), vspace_cap);

    /* Create any PDPTs needed for the user land image */
    for (vptr = ROUND_DOWN(it_v_reg.start, PML4_INDEX_OFFSET);
            vptr < it_v_reg.end;
            vptr += BIT(PML4_INDEX_OFFSET)) {
        pptr = alloc_region(seL4_PDPTBits);
        if (!pptr) {
            return cap_null_cap_new();
        }
        memzero(PDPT_PTR(pptr), BIT(seL4_PDPTBits));
        if (!provide_cap(root_cnode_cap,
                         create_it_pdpt_cap(vspace_cap, pptr, vptr, IT_ASID))
           ) {
            return cap_null_cap_new();
        }
    }

    /* Create any PDs needed for the user land image */
    for (vptr = ROUND_DOWN(it_v_reg.start, PDPT_INDEX_OFFSET);
            vptr < it_v_reg.end;
            vptr += BIT(PDPT_INDEX_OFFSET)) {
        pptr = alloc_region(seL4_PageDirBits);
        if (!pptr) {
            return cap_null_cap_new();
        }
        memzero(PD_PTR(pptr), BIT(seL4_PageDirBits));
        if (!provide_cap(root_cnode_cap,
                         create_it_pd_cap(vspace_cap, pptr, vptr, IT_ASID))
           ) {
            return cap_null_cap_new();
        }
    }

    /* Create any PTs needed for the user land image */
    for (vptr = ROUND_DOWN(it_v_reg.start, PD_INDEX_OFFSET);
            vptr < it_v_reg.end;
            vptr += BIT(PD_INDEX_OFFSET)) {
        pptr = alloc_region(seL4_PageTableBits);
        if (!pptr) {
            return cap_null_cap_new();
        }
        memzero(PT_PTR(pptr), BIT(seL4_PageTableBits));
        if (!provide_cap(root_cnode_cap,
                         create_it_pt_cap(vspace_cap, pptr, vptr, IT_ASID))
           ) {
            return cap_null_cap_new();
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->userImagePaging = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };
    return vspace_cap;
}

void copyGlobalMappings(vspace_root_t *new_vspace)
{
    unsigned long i;
    pml4e_t *vspace = (pml4e_t *)new_vspace;

    /* Copy from the user top so that we copy the default entries of the
     * tlb bitmap (if it exists). If it doesn't exist then this loop
     * will be equivalent to copying from PPTR_BASE
     */
    for (i = GET_PML4_INDEX(PPTR_USER_TOP); i < BIT(PML4_INDEX_BITS); i++) {
        vspace[i] = x64KSGlobalPML4[i];
    }
}

static BOOT_CODE cap_t
create_it_frame_cap(pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large, seL4_Word map_type)
{
    vm_page_size_t frame_size;

    if (use_large) {
        frame_size = X86_LargePage;
    } else {
        frame_size = X86_SmallPage;
    }

    return
        cap_frame_cap_new(
            asid,                          /* capFMappedASID     */
            pptr,                          /* capFBasePtr        */
            frame_size,                    /* capFSize           */
            map_type,                      /* capFMapType        */
            vptr,                          /* capFMappedAddress  */
            wordFromVMRights(VMReadWrite), /* capFVMRights       */
            0                              /* capFIsDevice       */
        );
}

BOOT_CODE cap_t
create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large)
{
    return create_it_frame_cap(pptr, 0, asidInvalid, use_large, X86_MappingNone);
}

BOOT_CODE cap_t
create_mapped_it_frame_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large, bool_t executable UNUSED)
{
    cap_t cap = create_it_frame_cap(pptr, vptr, asid, use_large, X86_MappingVSpace);
    map_it_frame_cap(vspace_cap, cap);
    return cap;
}

/* ====================== BOOT CODE FINISHES HERE ======================== */



exception_t
performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr, cte_t *vspaceCapSlot)
{
    asid_map_t asid_map;
    if (cap_get_capType(vspaceCapSlot->cap) == cap_pml4_cap) {
        cap_pml4_cap_ptr_set_capPML4MappedASID(&vspaceCapSlot->cap, asid);
        cap_pml4_cap_ptr_set_capPML4IsMapped(&vspaceCapSlot->cap, 1);
        asid_map = asid_map_asid_map_vspace_new(cap_pml4_cap_get_capPML4BasePtr(vspaceCapSlot->cap));
    } else {
#ifdef CONFIG_VTX
        assert(cap_get_capType(vspaceCapSlot->cap) == cap_ept_pml4_cap);
        cap_ept_pml4_cap_ptr_set_capPML4MappedASID(&vspaceCapSlot->cap, asid);
        cap_ept_pml4_cap_ptr_set_capPML4IsMapped(&vspaceCapSlot->cap, 1);
        asid_map = asid_map_asid_map_ept_new(cap_ept_pml4_cap_get_capPML4BasePtr(vspaceCapSlot->cap));
#endif
    }
    poolPtr->array[asid & MASK(asidLowBits)] = asid_map;
    return EXCEPTION_NONE;
}

bool_t CONST
isVTableRoot(cap_t cap)
{
    return cap_get_capType(cap) == cap_pml4_cap;
}

bool_t CONST
isValidNativeRoot(cap_t cap)
{
    return isVTableRoot(cap) &&
           cap_pml4_cap_get_capPML4IsMapped(cap);
}

static pdpte_t CONST
makeUserPDPTEHugePage(paddr_t paddr, vm_attributes_t vm_attr, vm_rights_t vm_rights)
{
    return pdpte_pdpte_1g_new(
               0,          /* xd               */
               paddr,      /* physical address */
               0,          /* PAT              */
               0,          /* global           */
               0,          /* dirty            */
               0,          /* accessed         */
               vm_attributes_get_x86PCDBit(vm_attr),  /* cache disabled */
               vm_attributes_get_x86PWTBit(vm_attr),  /* write through  */
               SuperUserFromVMRights(vm_rights),       /* super user     */
               WritableFromVMRights(vm_rights),        /* read write     */
               1                                       /* present        */
           );
}

static pdpte_t CONST
makeUserPDPTEHugePageInvalid(void)
{
    return pdpte_pdpte_1g_new(
               0,          /* xd               */
               0,          /* physical address */
               0,          /* PAT              */
               0,          /* global           */
               0,          /* dirty            */
               0,          /* accessed         */
               0,          /* cache disabled */
               0,          /* write through  */
               0,          /* super user     */
               0,          /* read write     */
               0           /* present        */
           );
}

pde_t CONST
makeUserPDELargePage(paddr_t paddr, vm_attributes_t vm_attr, vm_rights_t vm_rights)
{
    return pde_pde_large_new(
               0,                                              /* xd                   */
               paddr,                                          /* page_base_address    */
               vm_attributes_get_x86PATBit(vm_attr),          /* pat                  */
               0,                                              /* global               */
               0,                                              /* dirty                */
               0,                                              /* accessed             */
               vm_attributes_get_x86PCDBit(vm_attr),          /* cache_disabled       */
               vm_attributes_get_x86PWTBit(vm_attr),          /* write_through        */
               SuperUserFromVMRights(vm_rights),               /* super_user           */
               WritableFromVMRights(vm_rights),                /* read_write           */
               1                                               /* present              */
           );
}

pde_t CONST
makeUserPDEPageTable(paddr_t paddr, vm_attributes_t vm_attr)
{

    return  pde_pde_small_new(
                0,                                  /* xd               */
                paddr,                              /* pt_base_address  */
                0,                                  /* accessed         */
                vm_attributes_get_x86PCDBit(vm_attr), /* cache_disabled   */
                vm_attributes_get_x86PWTBit(vm_attr), /* write_through    */
                1,                                  /* super_user       */
                1,                                  /* read_write       */
                1                                   /* present          */
            );
}

pde_t CONST
makeUserPDELargePageInvalid(void)
{
    return pde_pde_large_new(
               0,           /* xd                   */
               0,           /* page_base_address    */
               0,           /* pat                  */
               0,           /* global               */
               0,           /* dirty                */
               0,           /* accessed             */
               0,           /* cache_disabled       */
               0,           /* write_through        */
               0,           /* super_user           */
               0,           /* read_write           */
               0            /* present              */
           );
}

pde_t CONST
makeUserPDEPageTableInvalid(void)
{
    return pde_pde_small_new(
               0,      /* xd               */
               0,      /* pt_base_addr     */
               0,      /* accessed         */
               0,      /* cache_disabled   */
               0,      /* write_through    */
               0,      /* super_user       */
               0,      /* read_write       */
               0       /* present          */
           );
}

pte_t CONST
makeUserPTE(paddr_t paddr, vm_attributes_t vm_attr, vm_rights_t vm_rights)
{
    return pte_new(
               0,                                              /* xd                   */
               paddr,                                          /* page_base_address    */
               0,                                              /* global               */
               vm_attributes_get_x86PATBit(vm_attr),          /* pat                  */
               0,                                              /* dirty                */
               0,                                              /* accessed             */
               vm_attributes_get_x86PCDBit(vm_attr),          /* cache_disabled       */
               vm_attributes_get_x86PWTBit(vm_attr),          /* write_through        */
               SuperUserFromVMRights(vm_rights),               /* super_user           */
               WritableFromVMRights(vm_rights),                /* read_write           */
               1                                               /* present              */
           );
}

pte_t CONST
makeUserPTEInvalid(void)
{
    return pte_new(
               0,                   /* xd                   */
               0,                   /* page_base_address    */
               0,                   /* global               */
               0,                   /* pat                  */
               0,                   /* dirty                */
               0,                   /* accessed             */
               0,                   /* cache_disabled       */
               0,                   /* write_through        */
               0,                   /* super_user           */
               0,                   /* read_write           */
               0                    /* present              */
           );
}


static lookupPML4Slot_ret_t
lookupPML4Slot(vspace_root_t*pml4, vptr_t vptr)
{
    lookupPML4Slot_ret_t ret;
    pml4e_t *pml4e = PML4E_PTR(pml4);
    word_t pml4Index = GET_PML4_INDEX(vptr);
    ret.status = EXCEPTION_NONE;
    ret.pml4Slot = pml4e + pml4Index;
    return ret;
}

static lookupPDPTSlot_ret_t
lookupPDPTSlot(vspace_root_t *pml4, vptr_t vptr)
{
    lookupPML4Slot_ret_t pml4Slot = lookupPML4Slot(pml4, vptr);
    lookupPDPTSlot_ret_t ret;

    if (pml4Slot.status != EXCEPTION_NONE) {
        ret.pdptSlot = NULL;
        ret.status = pml4Slot.status;
        return ret;
    }
    if (!pml4e_ptr_get_present(pml4Slot.pml4Slot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(PML4_INDEX_OFFSET);

        ret.pdptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        pdpte_t *pdpt;
        pdpte_t *pdptSlot;
        word_t pdptIndex = GET_PDPT_INDEX(vptr);
        pdpt = paddr_to_pptr(pml4e_ptr_get_pdpt_base_address(pml4Slot.pml4Slot));
        pdptSlot = pdpt + pdptIndex;

        ret.status = EXCEPTION_NONE;
        ret.pdptSlot = pdptSlot;
        return ret;
    }
}

lookupPDSlot_ret_t
lookupPDSlot(vspace_root_t *pml4, vptr_t vptr)
{
    lookupPDPTSlot_ret_t pdptSlot;
    lookupPDSlot_ret_t ret;

    pdptSlot = lookupPDPTSlot(pml4, vptr);

    if (pdptSlot.status != EXCEPTION_NONE) {
        ret.pdSlot = NULL;
        ret.status = pdptSlot.status;
        return ret;
    }
    if ((pdpte_ptr_get_page_size(pdptSlot.pdptSlot) != pdpte_pdpte_pd) ||
            !pdpte_pdpte_pd_ptr_get_present(pdptSlot.pdptSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(PDPT_INDEX_OFFSET);

        ret.pdSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        pde_t *pd;
        pde_t *pdSlot;
        word_t pdIndex = GET_PD_INDEX(vptr);
        pd = paddr_to_pptr(pdpte_pdpte_pd_ptr_get_pd_base_address(pdptSlot.pdptSlot));
        pdSlot = pd + pdIndex;

        ret.status = EXCEPTION_NONE;
        ret.pdSlot = pdSlot;
        return ret;
    }
}

static void
flushPD(vspace_root_t *vspace, word_t vptr, pde_t *pd, asid_t asid)
{
    /* clearing the entire PCID vs flushing the virtual addresses
     * one by one using invplg.
     * choose the easy way, invalidate the PCID
     */
    invalidateASID(vspace, asid, SMP_TERNARY(tlb_bitmap_get(vspace), 0));

}

static void
flushPDPT(vspace_root_t *vspace, word_t vptr, pdpte_t *pdpt, asid_t asid)
{
    /* similar here */
    invalidateASID(vspace, asid, SMP_TERNARY(tlb_bitmap_get(vspace), 0));
    return;
}

void
hwASIDInvalidate(asid_t asid, vspace_root_t *vspace)
{
    invalidateASID(vspace, asid, SMP_TERNARY(tlb_bitmap_get(vspace), 0));
}

void
unmapPageDirectory(asid_t asid, vptr_t vaddr, pde_t *pd)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPDPTSlot_ret_t    lu_ret;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    lu_ret = lookupPDPTSlot(find_ret.vspace_root, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }

    /* check if the PDPT has the PD */
    if (! (pdpte_ptr_get_page_size(lu_ret.pdptSlot) == pdpte_pdpte_pd &&
            pdpte_pdpte_pd_ptr_get_present(lu_ret.pdptSlot) &&
            (pdpte_pdpte_pd_ptr_get_pd_base_address(lu_ret.pdptSlot) == pptr_to_paddr(pd)))) {
        return;
    }

    flushPD(find_ret.vspace_root, vaddr, pd, asid);

    *lu_ret.pdptSlot = pdpte_pdpte_pd_new(
                           0,                      /* xd               */
                           0,                      /* paddr            */
                           0,                      /* accessed         */
                           0,                      /* cache disabled   */
                           0,                      /* write through    */
                           0,                      /* super user       */
                           0,                      /* read write       */
                           0                       /* present          */
                       );

    invalidatePageStructureCacheASID(pptr_to_paddr(find_ret.vspace_root), asid,
                                     SMP_TERNARY(tlb_bitmap_get(find_ret.vspace_root), 0));
}


static exception_t
performX64PageDirectoryInvocationUnmap(cap_t cap, cte_t *ctSlot)
{

    if (cap_page_directory_cap_get_capPDIsMapped(cap)) {
        pde_t *pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));
        unmapPageDirectory(
            cap_page_directory_cap_get_capPDMappedASID(cap),
            cap_page_directory_cap_get_capPDMappedAddress(cap),
            pd
        );
        clearMemory((void *)pd, cap_get_capSizeBits(cap));
    }

    cap_page_directory_cap_ptr_set_capPDIsMapped(&(ctSlot->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t
performX64PageDirectoryInvocationMap(cap_t cap, cte_t *ctSlot, pdpte_t pdpte, pdpte_t *pdptSlot, vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *pdptSlot = pdpte;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_page_directory_cap_get_capPDMappedASID(cap),
                                     SMP_TERNARY(tlb_bitmap_get(vspace), 0));
    return EXCEPTION_NONE;
}


static exception_t
decodeX64PageDirectoryInvocation(
    word_t label,
    word_t length,
    cte_t* cte,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    word_t              vaddr;
    vm_attributes_t     vm_attr;
    cap_t               vspaceCap;
    vspace_root_t      *vspace;
    pdpte_t             pdpte;
    paddr_t             paddr;
    asid_t              asid;
    lookupPDPTSlot_ret_t pdptSlot;

    if (label == X86PageDirectoryUnmap) {
        if (!isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            userError("X86PageDirectory: Cannot unmap if more than one cap exist.");
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);

        return performX64PageDirectoryInvocationUnmap(cap, cte);
    }

    if (label != X86PageDirectoryMap) {
        userError("X64Directory: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || extraCaps.excaprefs[0] == NULL) {
        userError("X64PageDirectory: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_page_directory_cap_get_capPDIsMapped(cap)) {
        userError("X64PageDirectory: PD is already mapped to a PML4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer) & (~MASK(PDPT_INDEX_OFFSET));
    vm_attr = vmAttributesFromWord(getSyscallArg(1, buffer));
    vspaceCap = extraCaps.excaprefs[0]->cap;

    if (!isValidNativeRoot(vspaceCap)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vspace = (vspace_root_t *)pptr_of_cap(vspaceCap);
    asid = cap_get_capMappedASID(vspaceCap);

    if (vaddr >= PPTR_USER_TOP) {
        userError("X64PageDirectory: Mapping address too high.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pdptSlot = lookupPDPTSlot(vspace, vaddr);
    if (pdptSlot.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if ((pdpte_ptr_get_page_size(pdptSlot.pdptSlot) == pdpte_pdpte_pd &&
            pdpte_pdpte_pd_ptr_get_present(pdptSlot.pdptSlot)) ||
            (pdpte_ptr_get_page_size(pdptSlot.pdptSlot) == pdpte_pdpte_1g
             && pdpte_pdpte_1g_ptr_get_present(pdptSlot.pdptSlot))) {
        current_syscall_error.type = seL4_DeleteFirst;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap)));
    pdpte = pdpte_pdpte_pd_new(
                0,                      /* xd       */
                paddr,                  /* paddr    */
                0,                      /* accessed */
                vm_attributes_get_x86PCDBit(vm_attr),  /* cache disabled */
                vm_attributes_get_x86PWTBit(vm_attr),  /* write through  */
                1,                      /* super user */
                1,                      /* read write */
                1                       /* present    */
            );

    cap = cap_page_directory_cap_set_capPDIsMapped(cap, 1);
    cap = cap_page_directory_cap_set_capPDMappedASID(cap, asid);
    cap = cap_page_directory_cap_set_capPDMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performX64PageDirectoryInvocationMap(cap, cte, pdpte, pdptSlot.pdptSlot, vspace);
}

static void unmapPDPT(asid_t asid, vptr_t vaddr, pdpte_t *pdpt)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPML4Slot_ret_t    lu_ret;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    lu_ret = lookupPML4Slot(find_ret.vspace_root, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }

    /* check if the PML4 has the PDPT */
    if (! (pml4e_ptr_get_present(lu_ret.pml4Slot) &&
            pml4e_ptr_get_pdpt_base_address(lu_ret.pml4Slot) == pptr_to_paddr(pdpt))) {
        return;
    }

    flushPDPT(find_ret.vspace_root, vaddr, pdpt, asid);

    *lu_ret.pml4Slot = pml4e_new(
                           0,                  /* xd               */
                           0,                  /* pdpt_base_addr   */
                           0,                  /* accessed         */
                           0,                  /* cache_disabled   */
                           0,                  /* write through    */
                           0,                  /* super user       */
                           0,                  /* read_write       */
                           0                   /* present          */
                       );
}

static exception_t
performX64PDPTInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_pdpt_cap_get_capPDPTIsMapped(cap)) {
        pdpte_t *pdpt = PDPTE_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap));
        unmapPDPT(cap_pdpt_cap_get_capPDPTMappedASID(cap),
                  cap_pdpt_cap_get_capPDPTMappedAddress(cap),
                  pdpt);
        clearMemory((void *)pdpt, cap_get_capSizeBits(cap));
    }

    cap_pdpt_cap_ptr_set_capPDPTIsMapped(&(ctSlot->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t
performX64PDPTInvocationMap(cap_t cap, cte_t *ctSlot, pml4e_t pml4e, pml4e_t *pml4Slot, vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *pml4Slot = pml4e;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_pdpt_cap_get_capPDPTMappedASID(cap),
                                     SMP_TERNARY(tlb_bitmap_get(vspace), 0));

    return EXCEPTION_NONE;
}

static exception_t
decodeX64PDPTInvocation(
    word_t  label,
    word_t length,
    cte_t   *cte,
    cap_t   cap,
    extra_caps_t extraCaps,
    word_t  *buffer)
{
    word_t                  vaddr;
    vm_attributes_t         attr;
    lookupPML4Slot_ret_t    pml4Slot;
    cap_t                   vspaceCap;
    vspace_root_t          *vspace;
    pml4e_t                 pml4e;
    paddr_t                 paddr;
    asid_t                  asid;

    if (label == X86PDPTUnmap) {
        if (!isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            userError("X86PDPT: Cannot unmap if more than one cap exist.");
            return EXCEPTION_SYSCALL_ERROR;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);

        return performX64PDPTInvocationUnmap(cap, cte);
    }

    if (label != X86PDPTMap) {
        userError("X86PDPT: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || extraCaps.excaprefs[0] == NULL) {
        userError("X64PDPT: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_pdpt_cap_get_capPDPTIsMapped(cap)) {
        userError("X64PDPT: PDPT is already mapped to a PML4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer) & (~MASK(PML4_INDEX_OFFSET));
    attr = vmAttributesFromWord(getSyscallArg(1, buffer));
    vspaceCap = extraCaps.excaprefs[0]->cap;

    if (!isValidNativeRoot(vspaceCap)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vspace = (vspace_root_t *)pptr_of_cap(vspaceCap);
    asid = cap_get_capMappedASID(vspaceCap);

    if (vaddr >= PPTR_USER_TOP) {
        userError("X64PDPT: Mapping address too high.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pml4Slot = lookupPML4Slot(vspace, vaddr);
    if (pml4Slot.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (pml4e_ptr_get_present(pml4Slot.pml4Slot)) {
        current_syscall_error.type = seL4_DeleteFirst;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(PDPTE_PTR((cap_pdpt_cap_get_capPDPTBasePtr(cap))));
    pml4e = pml4e_new(
                0,
                paddr,
                0,
                vm_attributes_get_x86PCDBit(attr),
                vm_attributes_get_x86PWTBit(attr),
                1,
                1,
                1
            );

    cap = cap_pdpt_cap_set_capPDPTIsMapped(cap, 1);
    cap = cap_pdpt_cap_set_capPDPTMappedASID(cap, asid);
    cap = cap_pdpt_cap_set_capPDPTMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performX64PDPTInvocationMap(cap, cte, pml4e, pml4Slot.pml4Slot, vspace);
}

exception_t
decodeX86ModeMMUInvocation(
    word_t label,
    word_t length,
    cptr_t cptr,
    cte_t* cte,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {

    case cap_pml4_cap:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;

    case cap_pdpt_cap:
        return decodeX64PDPTInvocation(label, length, cte, cap, extraCaps, buffer);

    case cap_page_directory_cap:
        return decodeX64PageDirectoryInvocation(label, length, cte, cap, extraCaps, buffer);

    default:
        fail("Invalid arch cap type");
    }
}

void modeUnmapPage(vm_page_size_t page_size, vspace_root_t *vroot, vptr_t vaddr, void *pptr)
{
    if (config_set(CONFIG_HUGE_PAGE) && page_size == X64_HugePage) {
        pdpte_t     *pdpte;
        lookupPDPTSlot_ret_t pdpt_ret = lookupPDPTSlot(vroot, vaddr);
        if (pdpt_ret.status != EXCEPTION_NONE) {
            return;
        }
        pdpte = pdpt_ret.pdptSlot;


        if (! (pdpte_ptr_get_page_size(pdpte) == pdpte_pdpte_1g
                && pdpte_pdpte_1g_ptr_get_present(pdpte)
                &&  (pdpte_pdpte_1g_ptr_get_page_base_address(pdpte)
                     == pptr_to_paddr(pptr)))) {
            return;
        }

        *pdpte = makeUserPDPTEHugePageInvalid();
        return;
    }
    fail("Invalid page type");

}

static exception_t
performX64ModeMapRemapPage(cap_t cap, cte_t *ctSlot, pdpte_t pdpte, pdpte_t *pdptSlot, vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *pdptSlot = pdpte;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_frame_cap_get_capFMappedASID(cap),
                                     SMP_TERNARY(tlb_bitmap_get(vspace), 0));
    return EXCEPTION_NONE;
}


exception_t
decodeX86ModeMapRemapPage(word_t label, vm_page_size_t page_size, cte_t *cte, cap_t cap, vspace_root_t *vroot, vptr_t vaddr, paddr_t paddr, vm_rights_t vm_rights, vm_attributes_t vm_attr)
{
    if (config_set(CONFIG_HUGE_PAGE) && page_size == X64_HugePage) {
        pdpte_t pdpte;
        pdpte_t *pdptSlot;
        lookupPDPTSlot_ret_t lu_ret;
        lu_ret = lookupPDPTSlot(vroot, vaddr);

        if (lu_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        pdptSlot = lu_ret.pdptSlot;
        switch (label) {

        case X86PageMap: {
            /* check for existing page directory */
            if ((pdpte_ptr_get_page_size(pdptSlot) == pdpte_pdpte_pd)  &&
                    (pdpte_pdpte_pd_ptr_get_present(pdptSlot))) {
                current_syscall_error.type = seL4_DeleteFirst;
                return EXCEPTION_SYSCALL_ERROR;
            }
            /* check for existing huge page */
            if ((pdpte_ptr_get_page_size(pdptSlot) == pdpte_pdpte_1g) &&
                    (pdpte_pdpte_1g_ptr_get_present(pdptSlot))) {
                current_syscall_error.type = seL4_DeleteFirst;
                return EXCEPTION_SYSCALL_ERROR;
            }

            pdpte = makeUserPDPTEHugePage(paddr, vm_attr, vm_rights);
            setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
            return performX64ModeMapRemapPage(cap, cte, pdpte, pdptSlot, vroot);
        }

        case X86PageRemap: {
            /* check for existing page directory */
            if ((pdpte_ptr_get_page_size(pdptSlot) == pdpte_pdpte_pd)  &&
                    (pdpte_pdpte_pd_ptr_get_present(pdptSlot))) {
                current_syscall_error.type = seL4_DeleteFirst;
                return EXCEPTION_SYSCALL_ERROR;
            }

            pdpte = makeUserPDPTEHugePage(paddr, vm_attr, vm_rights);
            setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
            return performX64ModeMapRemapPage(cap, cte, pdpte, pdptSlot, vroot);
        }

        default: {
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        }
    }
    fail("Invalid Page type");
}

#ifdef CONFIG_PRINTING
typedef struct readWordFromVSpace_ret {
    exception_t status;
    word_t value;
} readWordFromVSpace_ret_t;

static readWordFromVSpace_ret_t
readWordFromVSpace(vspace_root_t *vspace, word_t vaddr)
{
    readWordFromVSpace_ret_t ret;
    lookupPTSlot_ret_t ptSlot;
    lookupPDSlot_ret_t pdSlot;
    lookupPDPTSlot_ret_t pdptSlot;
    paddr_t paddr;
    word_t offset;
    pptr_t kernel_vaddr;
    word_t *value;

    pdptSlot = lookupPDPTSlot(vspace, vaddr);
    if (pdptSlot.status == EXCEPTION_NONE &&
            pdpte_ptr_get_page_size(pdptSlot.pdptSlot) == pdpte_pdpte_1g &&
            pdpte_pdpte_1g_ptr_get_present(pdptSlot.pdptSlot)) {

        paddr = pdpte_pdpte_1g_ptr_get_page_base_address(pdptSlot.pdptSlot);
        offset = vaddr & MASK(seL4_HugePageBits);
    } else {
        pdSlot = lookupPDSlot(vspace, vaddr);
        if (pdSlot.status == EXCEPTION_NONE &&
                ((pde_ptr_get_page_size(pdSlot.pdSlot) == pde_pde_large) &&
                 pde_pde_large_ptr_get_present(pdSlot.pdSlot))) {

            paddr = pde_pde_large_ptr_get_page_base_address(pdSlot.pdSlot);
            offset = vaddr & MASK(seL4_LargePageBits);
        } else {
            ptSlot = lookupPTSlot(vspace, vaddr);
            if (ptSlot.status == EXCEPTION_NONE && pte_ptr_get_present(ptSlot.ptSlot)) {
                paddr = pte_ptr_get_page_base_address(ptSlot.ptSlot);
                offset = vaddr & MASK(seL4_PageBits);
            } else {
                ret.status = EXCEPTION_LOOKUP_FAULT;
                return ret;
            }
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
    vspace_root_t *vspace_root;
    word_t sp;
    int i;

    threadRoot = TCB_PTR_CTE_PTR(tptr, tcbVTable)->cap;

    /* lookup the PD */
    if (cap_get_capType(threadRoot) != cap_pml4_cap) {
        printf("Invalid vspace\n");
        return;
    }

    vspace_root = (vspace_root_t*)pptr_of_cap(threadRoot);

    sp = getRegister(tptr, RSP);
    /* check for alignment so we don't have to worry about accessing
     * words that might be on two different pages */
    if (!IS_ALIGNED(sp, WORD_SIZE_BITS)) {
        printf("RSP not aligned\n");
        return;
    }

    for (i = 0; i < CONFIG_USER_STACK_TRACE_LENGTH; i++) {
        word_t address = sp + (i * sizeof(word_t));
        readWordFromVSpace_ret_t result;
        result = readWordFromVSpace(vspace_root, address);
        if (result.status == EXCEPTION_NONE) {
            printf("0x%lx: 0x%lx\n", (long)address, (long)result.value);
        } else {
            printf("0x%lx: INVALID\n", (long)address);
        }
    }
}
#endif /* CONFIG_PRINTING */
