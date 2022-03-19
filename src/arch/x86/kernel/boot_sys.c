/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <util.h>
#include <hardware.h>
#include <machine/io.h>
#include <arch/machine.h>
#include <arch/kernel/apic.h>
#include <arch/kernel/cmdline.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/boot_sys.h>
#include <arch/kernel/smp_sys.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/elf.h>
#include <smp/lock.h>
#include <linker.h>
#include <plat/machine/acpi.h>
#include <plat/machine/devices.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <sel4/arch/bootinfo_types.h>

/* addresses defined in linker script */
/* need a fake array to get the pointer from the linker script */

/* start/end of CPU boot code */
extern char boot_cpu_start[1];
extern char boot_cpu_end[1];

/* start/end of boot stack */
extern char boot_stack_bottom[1];
extern char boot_stack_top[1];

/* locations in kernel image */
extern char ki_skim_start[1];
extern char ki_skim_end[1];

#ifdef CONFIG_PRINTING
/* kernel entry point */
extern char _start[1];
#endif

/* constants */

#define HIGHMEM_PADDR 0x100000

BOOT_BSS
boot_state_t boot_state;

/* global variables (not covered by abstract specification) */

BOOT_BSS
cmdline_opt_t cmdline_opt;

/* functions not modeled in abstract specification */

BOOT_CODE static paddr_t find_load_paddr(paddr_t min_paddr, word_t image_size)
{
    int i;

    for (i = 0; i < boot_state.mem_p_regs.count; i++) {
        paddr_t start = MAX(min_paddr, boot_state.mem_p_regs.list[i].start);
        paddr_t end = boot_state.mem_p_regs.list[i].end;
        word_t region_size = end - start;

        if (region_size >= image_size) {
            return start;
        }
    }

    return 0;
}

BOOT_CODE static paddr_t load_boot_module(word_t boot_module_start, paddr_t load_paddr)
{
    v_region_t v_reg;
    word_t entry;
    Elf_Header_t *elf_file = (Elf_Header_t *)boot_module_start;

    if (!elf_checkFile(elf_file)) {
        printf("Boot module does not contain a valid ELF image\n");
        return 0;
    }

    v_reg = elf_getMemoryBounds(elf_file);
    entry = elf_file->e_entry;

    if (v_reg.end == 0) {
        printf("ELF image in boot module does not contain any segments\n");
        return 0;
    }
    v_reg.end = ROUND_UP(v_reg.end, PAGE_BITS);

    printf("size=0x%lx v_entry=%p v_start=%p v_end=%p ",
           v_reg.end - v_reg.start,
           (void *)entry,
           (void *)v_reg.start,
           (void *)v_reg.end
          );

    if (!IS_ALIGNED(v_reg.start, PAGE_BITS)) {
        printf("Userland image virtual start address must be 4KB-aligned\n");
        return 0;
    }
    if (v_reg.end + 2 * BIT(PAGE_BITS) > USER_TOP) {
        /* for IPC buffer frame and bootinfo frame, need 2*4K of additional userland virtual memory */
        printf("Userland image virtual end address too high\n");
        return 0;
    }
    if ((entry < v_reg.start) || (entry >= v_reg.end)) {
        printf("Userland image entry point does not lie within userland image\n");
        return 0;
    }

    load_paddr = find_load_paddr(load_paddr, v_reg.end - v_reg.start);
    assert(load_paddr);

    /* fill ui_info struct */
    boot_state.ui_info.pv_offset = load_paddr - v_reg.start;
    boot_state.ui_info.p_reg.start = load_paddr;
    load_paddr += v_reg.end - v_reg.start;
    boot_state.ui_info.p_reg.end = load_paddr;
    boot_state.ui_info.v_entry = entry;

    printf("p_start=0x%lx p_end=0x%lx\n",
           boot_state.ui_info.p_reg.start,
           boot_state.ui_info.p_reg.end
          );

    /* initialise all initial userland memory and load potentially sparse ELF image */
    memzero(
        (void *)boot_state.ui_info.p_reg.start,
        boot_state.ui_info.p_reg.end - boot_state.ui_info.p_reg.start
    );
    elf_load(elf_file, boot_state.ui_info.pv_offset);

    return load_paddr;
}

static BOOT_CODE bool_t try_boot_sys_node(cpu_id_t cpu_id)
{
    p_region_t boot_mem_reuse_p_reg;

    if (!map_kernel_window(
            boot_state.num_ioapic,
            boot_state.ioapic_paddr,
            boot_state.num_drhu,
            boot_state.drhu_list
        )) {
        return false;
    }
    setCurrentVSpaceRoot(kpptr_to_paddr(X86_KERNEL_VSPACE_ROOT), 0);
    /* Sync up the compilers view of the world here to force the PD to actually
     * be set *right now* instead of delayed */
    asm volatile("" ::: "memory");

#ifdef CONFIG_KERNEL_SKIM_WINDOW
    if (!map_skim_window((vptr_t)ki_skim_start, (vptr_t)ki_skim_end)) {
        return false;
    }
#endif

    /* reuse boot code/data memory */
    boot_mem_reuse_p_reg.start = KERNEL_ELF_PADDR_BASE;
    boot_mem_reuse_p_reg.end = kpptr_to_paddr(ki_boot_end);

    /* initialise the CPU */
    if (!init_cpu(config_set(CONFIG_IRQ_IOAPIC) ? 1 : 0)) {
        return false;
    }

    /* initialise NDKS and kernel heap */
    if (!init_sys_state(
            cpu_id,
            &boot_state.mem_p_regs,
            boot_state.ui_info,
            boot_mem_reuse_p_reg,
            /* parameters below not modeled in abstract specification */
            boot_state.num_drhu,
            boot_state.drhu_list,
            &boot_state.rmrr_list,
            &boot_state.acpi_rsdp,
            &boot_state.vbe_info,
            &boot_state.mb_mmap_info,
            &boot_state.fb_info
        )) {
        return false;
    }

    return true;
}

static BOOT_CODE bool_t add_mem_p_regs(p_region_t reg)
{
    if (reg.end > PADDR_TOP && reg.start > PADDR_TOP) {
        /* Return true here as it's not an error for there to exist memory outside the kernel window,
         * we're just going to ignore it and leave it to be given out as device memory */
        return true;
    }
    if (boot_state.mem_p_regs.count == MAX_NUM_FREEMEM_REG) {
        printf("Dropping memory region 0x%lx-0x%lx, try increasing MAX_NUM_FREEMEM_REG\n", reg.start, reg.end);
        return false;
    }
    if (reg.end > PADDR_TOP) {
        assert(reg.start <= PADDR_TOP);
        /* Clamp a region to the top of the kernel window if it extends beyond */
        reg.end = PADDR_TOP;
    }
    printf("Adding physical memory region 0x%lx-0x%lx\n", reg.start, reg.end);
    boot_state.mem_p_regs.list[boot_state.mem_p_regs.count] = reg;
    boot_state.mem_p_regs.count++;
    return true;
}

/*
 * the code relies that the GRUB provides correct information
 * about the actual physical memory regions.
 */
static BOOT_CODE bool_t parse_mem_map(uint32_t mmap_length, uint32_t mmap_addr)
{
    multiboot_mmap_t *mmap = (multiboot_mmap_t *)((word_t)mmap_addr);
    printf("Parsing GRUB physical memory map\n");

    while ((word_t)mmap < (word_t)(mmap_addr + mmap_length)) {
        uint64_t mem_start = mmap->base_addr;
        uint64_t mem_length = mmap->length;
        uint32_t type = mmap->type;
        if (mem_start != (uint64_t)(word_t)mem_start) {
            printf("\tPhysical memory region not addressable\n");
        } else {
            printf("\tPhysical Memory Region from %lx size %lx type %d\n", (long)mem_start, (long)mem_length, type);
            if (type == MULTIBOOT_MMAP_USEABLE_TYPE && mem_start >= HIGHMEM_PADDR) {
                if (!add_mem_p_regs((p_region_t) {
                mem_start, mem_start + mem_length
            })) {
                    return false;
                }
            }
        }
        mmap++;
    }
    return true;
}

static BOOT_CODE bool_t is_compiled_for_microarchitecture(void)
{
    word_t microarch_generation = 0;
    x86_cpu_identity_t *model_info = x86_cpuid_get_model_info();

    if (config_set(CONFIG_ARCH_X86_SKYLAKE)) {
        microarch_generation = 7;
    } else if (config_set(CONFIG_ARCH_X86_BROADWELL)) {
        microarch_generation = 6;
    } else if (config_set(CONFIG_ARCH_X86_HASWELL)) {
        microarch_generation = 5;
    } else if (config_set(CONFIG_ARCH_X86_IVY)) {
        microarch_generation = 4;
    } else if (config_set(CONFIG_ARCH_X86_SANDY)) {
        microarch_generation = 3;
    } else if (config_set(CONFIG_ARCH_X86_WESTMERE)) {
        microarch_generation = 2;
    } else if (config_set(CONFIG_ARCH_X86_NEHALEM)) {
        microarch_generation = 1;
    }

    switch (model_info->model) {
    case SKYLAKE_1_MODEL_ID:
    case SKYLAKE_2_MODEL_ID:
        if (microarch_generation > 7) {
            return false;
        }
        break;

    case BROADWELL_1_MODEL_ID:
    case BROADWELL_2_MODEL_ID:
    case BROADWELL_3_MODEL_ID:
    case BROADWELL_4_MODEL_ID:
    case BROADWELL_5_MODEL_ID:
        if (microarch_generation > 6) {
            return false;
        }
        break;

    case HASWELL_1_MODEL_ID:
    case HASWELL_2_MODEL_ID:
    case HASWELL_3_MODEL_ID:
    case HASWELL_4_MODEL_ID:
        if (microarch_generation > 5) {
            return false;
        }
        break;

    case IVY_BRIDGE_1_MODEL_ID:
    case IVY_BRIDGE_2_MODEL_ID:
    case IVY_BRIDGE_3_MODEL_ID:
        if (microarch_generation > 4) {
            return false;
        }
        break;

    case SANDY_BRIDGE_1_MODEL_ID:
    case SANDY_BRIDGE_2_MODEL_ID:
        if (microarch_generation > 3) {
            return false;
        }
        break;

    case WESTMERE_1_MODEL_ID:
    case WESTMERE_2_MODEL_ID:
    case WESTMERE_3_MODEL_ID:
        if (microarch_generation > 2) {
            return false;
        }
        break;

    case NEHALEM_1_MODEL_ID:
    case NEHALEM_2_MODEL_ID:
    case NEHALEM_3_MODEL_ID:
        if (microarch_generation > 1) {
            return false;
        }
        break;

    default:
        if (!config_set(CONFIG_ARCH_X86_GENERIC)) {
            return false;
        }
    }

    return true;
}

static BOOT_CODE bool_t try_boot_sys(void)
{
    paddr_t mods_end_paddr = boot_state.mods_end_paddr;
    p_region_t ui_p_regs;
    paddr_t load_paddr;

    boot_state.ki_p_reg.start = KERNEL_ELF_PADDR_BASE;
    boot_state.ki_p_reg.end = kpptr_to_paddr(ki_end);

    if (!x86_cpuid_initialize()) {
        printf("Warning: Your x86 CPU has an unsupported vendor, '%s'.\n"
               "\tYour setup may not be able to competently run seL4 as "
               "intended.\n"
               "\tCurrently supported x86 vendors are AMD and Intel.\n",
               x86_cpuid_get_identity()->vendor_string);
    }

    if (!is_compiled_for_microarchitecture()) {
        printf("Warning: Your kernel was not compiled for the current microarchitecture.\n");
    }

    cpuid_007h_edx_t edx;
    edx.words[0] = x86_cpuid_edx(0x7, 0);
    /* see if we can definitively say whether or not we need the skim window by
     * checking whether the CPU is vulnerable to rogue data cache loads (rdcl) */
    if (cpuid_007h_edx_get_ia32_arch_cap_msr(edx)) {
        ia32_arch_capabilities_msr_t cap_msr;
        cap_msr.words[0] = x86_rdmsr(IA32_ARCH_CAPABILITIES_MSR);
        if (ia32_arch_capabilities_msr_get_rdcl_no(cap_msr) && config_set(CONFIG_KERNEL_SKIM_WINDOW)) {
            printf("CPU reports not vulnerable to Rogue Data Cache Load (aka Meltdown https://meltdownattack.com) "
                   "yet SKIM window is enabled. Performance is needlessly being impacted, consider disabling.\n");
        } else if (!ia32_arch_capabilities_msr_get_rdcl_no(cap_msr) && !config_set(CONFIG_KERNEL_SKIM_WINDOW)) {
            printf("CPU reports vulnerable to Rogue Data Cache Load (aka Meltdown https://meltdownattack.com) "
                   "yet SKIM window is *not* enabled. Please re-build with SKIM window enabled.");
            return false;
        }
    } else {
        /* hardware doesn't tell us directly so guess based on CPU vendor */
        if (config_set(CONFIG_KERNEL_SKIM_WINDOW) && x86_cpuid_get_identity()->vendor == X86_VENDOR_AMD) {
            printf("SKIM window for mitigating Meltdown (https://www.meltdownattack.com) "
                   "not necessary for AMD and performance is being needlessly affected, "
                   "consider disabling\n");
        }
        if (!config_set(CONFIG_KERNEL_SKIM_WINDOW) && x86_cpuid_get_identity()->vendor == X86_VENDOR_INTEL) {
            printf("***WARNING*** SKIM window not enabled, this machine is probably vulnerable "
                   "to Meltdown (https://www.meltdownattack.com), consider enabling\n");
        }
    }

#ifdef ENABLE_SMP_SUPPORT
    /* copy boot code for APs to lower memory to run in real mode */
    if (!copy_boot_code_aps(boot_state.mem_lower)) {
        return false;
    }
    /* Initialize any kernel TLS */
    mode_init_tls(0);
#endif /* ENABLE_SMP_SUPPORT */

    printf("Kernel loaded to: start=0x%lx end=0x%lx size=0x%lx entry=0x%lx\n",
           boot_state.ki_p_reg.start,
           boot_state.ki_p_reg.end,
           boot_state.ki_p_reg.end - boot_state.ki_p_reg.start,
           (paddr_t)_start
          );

    /* remapping legacy IRQs to their correct vectors */
    pic_remap_irqs(IRQ_INT_OFFSET);
    if (config_set(CONFIG_IRQ_IOAPIC)) {
        /* Disable the PIC so that it does not generate any interrupts. We need to
         * do this *before* we initialize the apic */
        pic_disable();
    }

    /* validate the ACPI table */
    if (!acpi_validate_rsdp(&boot_state.acpi_rsdp)) {
        return false;
    }

    /* check if kernel configuration matches platform requirments */
    if (!acpi_fadt_scan(&boot_state.acpi_rsdp)) {
        return false;
    }

    if (!config_set(CONFIG_IOMMU) || cmdline_opt.disable_iommu) {
        boot_state.num_drhu = 0;
    } else {
        /* query available IOMMUs from ACPI */
        acpi_dmar_scan(
            &boot_state.acpi_rsdp,
            boot_state.drhu_list,
            &boot_state.num_drhu,
            MAX_NUM_DRHU,
            &boot_state.rmrr_list
        );
    }

    /* query available CPUs from ACPI */
    boot_state.num_cpus = acpi_madt_scan(&boot_state.acpi_rsdp, boot_state.cpus, &boot_state.num_ioapic,
                                         boot_state.ioapic_paddr);
    if (boot_state.num_cpus == 0) {
        printf("No CPUs detected\n");
        return false;
    }

    if (config_set(CONFIG_IRQ_IOAPIC)) {
        if (boot_state.num_ioapic == 0) {
            printf("No IOAPICs detected\n");
            return false;
        }
    } else {
        if (boot_state.num_ioapic > 0) {
            printf("Detected %d IOAPICs, but configured to use PIC instead\n", boot_state.num_ioapic);
        }
    }

    mods_end_paddr = ROUND_UP(mods_end_paddr, PAGE_BITS);
    assert(mods_end_paddr > boot_state.ki_p_reg.end);

    printf("ELF-loading userland images from boot modules:\n");
    load_paddr = mods_end_paddr;

    load_paddr = load_boot_module(boot_state.boot_module_start, load_paddr);
    if (!load_paddr) {
        return false;
    }

    /* calculate final location of userland images */
    ui_p_regs.start = boot_state.ki_p_reg.end;
    ui_p_regs.end = ui_p_regs.start + load_paddr - mods_end_paddr;

    printf(
        "Moving loaded userland images to final location: from=0x%lx to=0x%lx size=0x%lx\n",
        mods_end_paddr,
        ui_p_regs.start,
        ui_p_regs.end - ui_p_regs.start
    );
    memcpy((void *)ui_p_regs.start, (void *)mods_end_paddr, ui_p_regs.end - ui_p_regs.start);

    /* adjust p_reg and pv_offset to final load address */
    boot_state.ui_info.p_reg.start -= mods_end_paddr - ui_p_regs.start;
    boot_state.ui_info.p_reg.end   -= mods_end_paddr - ui_p_regs.start;
    boot_state.ui_info.pv_offset   -= mods_end_paddr - ui_p_regs.start;

    /* ==== following code corresponds to abstract specification after "select" ==== */

    if (!platAddDevices()) {
        return false;
    }

    /* Total number of cores we intend to boot */
    ksNumCPUs = boot_state.num_cpus;

    printf("Starting node #0 with APIC ID %lu\n", boot_state.cpus[0]);
    if (!try_boot_sys_node(boot_state.cpus[0])) {
        return false;
    }

    if (config_set(CONFIG_IRQ_IOAPIC)) {
        ioapic_init(1, boot_state.cpus, boot_state.num_ioapic);
    }

    /* initialize BKL before booting up APs */
    SMP_COND_STATEMENT(clh_lock_init());
    SMP_COND_STATEMENT(start_boot_aps());

    /* grab BKL before leaving the kernel */
    NODE_LOCK_SYS;

    printf("Booting all finished, dropped to user space\n");

    return true;
}

static BOOT_CODE bool_t try_boot_sys_mbi1(
    multiboot_info_t *mbi
)
{
    word_t i;
    multiboot_module_t *modules = (multiboot_module_t *)(word_t)mbi->part1.mod_list;

    cmdline_parse((const char *)(word_t)mbi->part1.cmdline, &cmdline_opt);

    if ((mbi->part1.flags & MULTIBOOT_INFO_MEM_FLAG) == 0) {
        printf("Boot loader did not provide information about physical memory size\n");
        return false;
    }

    if (!(mbi->part1.flags & MULTIBOOT_INFO_MODS_FLAG)) {
        printf("Boot loader did not provide information about boot modules\n");
        return false;
    }

    printf("Detected %d boot module(s):\n", mbi->part1.mod_count);

    if (mbi->part1.mod_count < 1) {
        printf("Expect at least one boot module (containing a userland image)\n");
        return false;
    }

    for (i = 0; i < mbi->part1.mod_count; i++) {
        printf(
            "  module #%ld: start=0x%x end=0x%x size=0x%x name='%s'\n",
            i,
            modules[i].start,
            modules[i].end,
            modules[i].end - modules[i].start,
            (char *)(long)modules[i].name
        );
        if ((sword_t)(modules[i].end - modules[i].start) <= 0) {
            printf("Invalid boot module size! Possible cause: boot module file not found by QEMU\n");
            return false;
        }
        if (boot_state.mods_end_paddr < modules[i].end) {
            boot_state.mods_end_paddr = modules[i].end;
        }
    }

    /* initialize the memory. We track two kinds of memory regions. Physical memory
     * that we will use for the kernel, and physical memory regions that we must
     * not give to the user. Memory regions that must not be given to the user
     * include all the physical memory in the kernel window, but also includes any
     * important or kernel devices. */
    boot_state.mem_p_regs.count = 0;
    if (mbi->part1.flags & MULTIBOOT_INFO_MMAP_FLAG) {
        if (!parse_mem_map(mbi->part2.mmap_length, mbi->part2.mmap_addr)) {
            return false;
        }
        uint32_t multiboot_mmap_length = mbi->part2.mmap_length;
        if (multiboot_mmap_length > (SEL4_MULTIBOOT_MAX_MMAP_ENTRIES * sizeof(seL4_X86_mb_mmap_t))) {
            printf("Warning: Multiboot has reported more memory map entries, %zd, "
                   "than the max amount that will be passed in the bootinfo, %d. "
                   "These extra regions will still be turned into untyped caps.",
                   multiboot_mmap_length / sizeof(seL4_X86_mb_mmap_t), SEL4_MULTIBOOT_MAX_MMAP_ENTRIES);
            multiboot_mmap_length = SEL4_MULTIBOOT_MAX_MMAP_ENTRIES * sizeof(seL4_X86_mb_mmap_t);
        }
        memcpy(&boot_state.mb_mmap_info.mmap, (void *)(word_t)mbi->part2.mmap_addr, multiboot_mmap_length);
        boot_state.mb_mmap_info.mmap_length = multiboot_mmap_length;
    } else {
        /* calculate memory the old way */
        p_region_t avail;
        avail.start = HIGHMEM_PADDR;
        avail.end = ROUND_DOWN(avail.start + (mbi->part1.mem_upper << 10), PAGE_BITS);
        if (!add_mem_p_regs(avail)) {
            return false;
        }
    }

    /* copy VESA information from multiboot header */
    if ((mbi->part1.flags & MULTIBOOT_INFO_GRAPHICS_FLAG) == 0) {
        boot_state.vbe_info.vbeMode = -1;
        printf("Multiboot gave us no video information\n");
    } else {
        boot_state.vbe_info.vbeInfoBlock = *(seL4_VBEInfoBlock_t *)(seL4_Word)mbi->part2.vbe_control_info;
        boot_state.vbe_info.vbeModeInfoBlock = *(seL4_VBEModeInfoBlock_t *)(seL4_Word)mbi->part2.vbe_mode_info;
        boot_state.vbe_info.vbeMode = mbi->part2.vbe_mode;
        printf("Got VBE info in multiboot. Current video mode is %d\n", mbi->part2.vbe_mode);
        boot_state.vbe_info.vbeInterfaceSeg = mbi->part2.vbe_interface_seg;
        boot_state.vbe_info.vbeInterfaceOff = mbi->part2.vbe_interface_off;
        boot_state.vbe_info.vbeInterfaceLen = mbi->part2.vbe_interface_len;
    }

    boot_state.mem_lower = mbi->part1.mem_lower;
    boot_state.boot_module_start = modules->start;

    /* Initialize ACPI */
    if (!acpi_init(&boot_state.acpi_rsdp)) {
        return false;
    }

    return true;
}

static BOOT_CODE bool_t try_boot_sys_mbi2(
    multiboot2_header_t *mbi2
)
{
    int mod_count                  = 0;
    multiboot2_tag_t const *tag   = (multiboot2_tag_t *)(mbi2 + 1);
    multiboot2_tag_t const *tag_e = (multiboot2_tag_t *)((word_t)mbi2 + mbi2->total_size);

    /* initialize the memory. We track two kinds of memory regions. Physical memory
     * that we will use for the kernel, and physical memory regions that we must
     * not give to the user. Memory regions that must not be given to the user
     * include all the physical memory in the kernel window, but also includes any
     * important or kernel devices. */
    boot_state.mem_p_regs.count = 0;
    boot_state.mb_mmap_info.mmap_length = 0;
    boot_state.vbe_info.vbeMode = -1;

    while (tag < tag_e && tag->type != MULTIBOOT2_TAG_END) {
        word_t const behind_tag = (word_t)tag + sizeof(*tag);

        if (tag->type == MULTIBOOT2_TAG_CMDLINE) {
            char const *const cmdline = (char const * const)(behind_tag);
            cmdline_parse(cmdline, &cmdline_opt);
        } else if (tag->type == MULTIBOOT2_TAG_ACPI_1) {
            if (ACPI_V1_SIZE == tag->size - sizeof(*tag)) {
                memcpy(&boot_state.acpi_rsdp, (void *)behind_tag, tag->size - sizeof(*tag));
            }
        } else if (tag->type == MULTIBOOT2_TAG_ACPI_2) {
            if (sizeof(boot_state.acpi_rsdp) == tag->size - sizeof(*tag)) {
                memcpy(&boot_state.acpi_rsdp, (void *)behind_tag, sizeof(boot_state.acpi_rsdp));
            }
        } else if (tag->type == MULTIBOOT2_TAG_MODULE) {
            multiboot2_module_t const *module = (multiboot2_module_t const *)behind_tag;
            printf(
                "  module #%d: start=0x%x end=0x%x size=0x%x name='%s'\n",
                mod_count,
                module->start,
                module->end,
                module->end - module->start,
                module->string
            );

            if (mod_count == 0) {
                boot_state.boot_module_start = module->start;
            }

            mod_count ++;
            if ((sword_t)(module->end - module->start) <= 0) {
                printf("Invalid boot module size! Possible cause: boot module file not found\n");
                return false;
            }
            if (boot_state.mods_end_paddr < module->end) {
                boot_state.mods_end_paddr = module->end;
            }
        } else if (tag->type == MULTIBOOT2_TAG_MEMORY) {
            multiboot2_memory_t const *s = (multiboot2_memory_t *)(behind_tag + 8);
            multiboot2_memory_t const *e = (multiboot2_memory_t *)((word_t)tag + tag->size);

            for (multiboot2_memory_t const *m = s; m < e; m++) {
                if (!m->addr) {
                    boot_state.mem_lower = m->size;
                }

                printf("\tPhysical Memory Region from %llx size %llx type %u\n", m->addr, m->size, m->type);
                if (m->addr != (uint64_t)(word_t)m->addr) {
                    printf("\t\tPhysical memory region not addressable\n");
                }

                if (m->type == MULTIBOOT_MMAP_USEABLE_TYPE && m->addr >= HIGHMEM_PADDR) {
                    if (!add_mem_p_regs((p_region_t) {
                    m->addr, m->addr + m->size
                }))
                    return false;
                }
            }
        } else if (tag->type == MULTIBOOT2_TAG_FB) {
            multiboot2_fb_t const *fb = (multiboot2_fb_t const *)behind_tag;
            printf("Got framebuffer info in multiboot2. Current video mode is at physical address=%llx pitch=%u resolution=%ux%u@%u type=%u\n",
                   fb->addr, fb->pitch, fb->width, fb->height, fb->bpp, fb->type);
            boot_state.fb_info = *fb;
        }

        tag = (multiboot2_tag_t const *)((word_t)tag + ROUND_UP(tag->size, 3));
    }

    printf("Detected %d boot module(s):\n", mod_count);

    if (mod_count < 1) {
        printf("Expect at least one boot module (containing a userland image)\n");
        return false;
    }

    return true;
}

BOOT_CODE VISIBLE void boot_sys(
    unsigned long multiboot_magic,
    void *mbi)
{
    bool_t result = false;

    if (multiboot_magic == MULTIBOOT_MAGIC) {
        result = try_boot_sys_mbi1(mbi);
    } else if (multiboot_magic == MULTIBOOT2_MAGIC) {
        result = try_boot_sys_mbi2(mbi);
    } else {
        printf("Boot loader is not multiboot 1 or 2 compliant %lx\n", multiboot_magic);
    }

    if (result) {
        result = try_boot_sys();
    }

    if (!result) {
        fail("boot_sys failed for some reason :(\n");
    }

    ARCH_NODE_STATE(x86KScurInterrupt) = int_invalid;
    ARCH_NODE_STATE(x86KSPendingInterrupt) = int_invalid;

#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
    NODE_STATE(ksConsumed) = 0;
#endif

    schedule();
    activateThread();
}

