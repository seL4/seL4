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
#include <util.h>
#include <machine/io.h>
#include <arch/machine.h>
#include <arch/kernel/apic.h>
#include <arch/kernel/cmdline.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/boot_sys.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/elf.h>
#include <arch/linker.h>
#include <plat/machine/acpi.h>
#include <plat/machine/devices.h>
#include <plat/machine/pci.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>

/* addresses defined in linker script */
/* need a fake array to get the pointer from the linker script */

/* start/end of CPU boot code */
extern char _boot_cpu_start[1];
extern char _boot_cpu_end[1];

/* start/end of boot stack */
extern char _boot_stack_bottom[1];
extern char _boot_stack_top[1];

/* locations in kernel image */
extern char ki_boot_end[1];
extern char ki_end[1];

#ifdef DEBUG
/* kernel entry point */
extern char _start[1];
#endif

/* constants */

#define BOOT_NODE_PADDR 0x80000

/* type definitions (directly corresponding to abstract specification) */

typedef struct boot_state {
    p_region_t   avail_p_reg; /* region of available physical memory on platform */
    p_region_t   ki_p_reg;    /* region where the kernel image is in */
    ui_info_t    ui_info;     /* info about userland images */
    dev_p_regs_t dev_p_regs;  /* device memory regions */
    uint32_t     num_ioapic;  /* number of IOAPICs detected */
    paddr_t      ioapic_paddr[CONFIG_MAX_NUM_IOAPIC];
    uint32_t     num_drhu; /* number of IOMMUs */
    paddr_t      drhu_list[MAX_NUM_DRHU]; /* list of physical addresses of the IOMMUs */
    acpi_rmrr_list_t rmrr_list;
} boot_state_t;

BOOT_DATA
boot_state_t boot_state;

/* There are a lot of assumptions on this being page aligned and
 * precisely 4K in size. DO NOT MODIFY */
ALIGN(BIT(PAGE_BITS))
char kernel_stack_alloc[4096];

/* global variables (not covered by abstract specification) */

BOOT_DATA
cmdline_opt_t cmdline_opt;

#if defined DEBUG || defined RELEASE_PRINTF

/* Determine whether we are in bootstrapping phase or runtime phase.
 * Is currently only needed to determine console port in debug mode.
 */
bool_t
in_boot_phase()
{
    paddr_t esp = pptr_to_paddr(get_current_esp());

    return (esp <= BOOT_NODE_PADDR ||
            (esp <= (paddr_t)_boot_stack_top && esp > (paddr_t)_boot_stack_bottom));
}

#endif

/* functions not modeled in abstract specification */

BOOT_CODE static paddr_t
load_boot_module(multiboot_module_t* boot_module, paddr_t load_paddr)
{
    Elf32_Header_t* elf_file = (Elf32_Header_t*)(word_t)boot_module->start;
    v_region_t v_reg;

    if (!elf32_checkFile(elf_file)) {
        printf("Boot module does not contain a valid ELF32 image\n");
        return 0;
    }

    v_reg = elf32_getMemoryBounds(elf_file);

    if (v_reg.end == 0) {
        printf("ELF32 image in boot module does not contain any segments\n");
        return 0;
    }
    v_reg.end = ROUND_UP(v_reg.end, PAGE_BITS);

    printf("size=0x%lx v_entry=%x v_start=0x%lx v_end=0x%lx ",
           v_reg.end - v_reg.start,
           elf_file->e_entry,
           v_reg.start,
           v_reg.end
          );

    if (!IS_ALIGNED(v_reg.start, PAGE_BITS)) {
        printf("Userland image virtual start address must be 4KB-aligned\n");
        return 0;
    }
    if (v_reg.end + 2 * BIT(PAGE_BITS) > PPTR_USER_TOP) {
        /* for IPC buffer frame and bootinfo frame, need 2*4K of additional userland virtual memory */
        printf("Userland image virtual end address too high\n");
        return 0;
    }
    if ((elf_file->e_entry < v_reg.start) || (elf_file->e_entry >= v_reg.end)) {
        printf("Userland image entry point does not lie within userland image\n");
        return 0;
    }

    /* fill ui_info struct */
    boot_state.ui_info.pv_offset = load_paddr - v_reg.start;
    boot_state.ui_info.p_reg.start = load_paddr;
    load_paddr += v_reg.end - v_reg.start;
    boot_state.ui_info.p_reg.end = load_paddr;
    boot_state.ui_info.v_entry = elf_file->e_entry;

    printf("p_start=0x%lx p_end=0x%lx\n",
           boot_state.ui_info.p_reg.start,
           boot_state.ui_info.p_reg.end
          );

    if (load_paddr > boot_state.avail_p_reg.end) {
        printf("End of loaded userland image lies outside of usable physical memory\n");
        return 0;
    }

    /* initialise all initial userland memory and load potentially sparse ELF image */
    memzero(
        (void*)boot_state.ui_info.p_reg.start,
        boot_state.ui_info.p_reg.end - boot_state.ui_info.p_reg.start
    );
    elf32_load(elf_file, boot_state.ui_info.pv_offset);

    return load_paddr;
}

BOOT_CODE void
insert_dev_p_reg(p_region_t reg)
{
    if (boot_state.dev_p_regs.count < CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS) {
        boot_state.dev_p_regs.list[boot_state.dev_p_regs.count] = reg;
        boot_state.dev_p_regs.count++;
        printf("\n");
    } else {
        printf(" -> IGNORED! (too many)\n");
    }
}

BOOT_CODE static void
discover_devices(void)
{
    /* We do not add any ia32 specific devices. Just add any platform ones */
    platAddDevices();
}

static BOOT_CODE bool_t
try_boot_sys_node(cpu_id_t cpu_id)
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
    write_cr3(pptr_to_paddr(X86_GLOBAL_VSPACE_ROOT));
    /* Sync up the compilers view of the world here to force the PD to actually
     * be set *right now* instead of delayed */
    asm volatile("" ::: "memory");

    /* reuse boot code/data memory */
    boot_mem_reuse_p_reg.start = PADDR_LOAD;
    boot_mem_reuse_p_reg.end = (paddr_t)ki_boot_end - KERNEL_BASE_OFFSET;

    /* initialise NDKS and kernel heap */
    if (!init_sys_state(
                cpu_id,
                boot_state.avail_p_reg,
                &boot_state.dev_p_regs,
                boot_state.ui_info,
                boot_mem_reuse_p_reg,
                /* parameters below not modeled in abstract specification */
                boot_state.num_drhu,
                boot_state.drhu_list,
                &boot_state.rmrr_list
            )) {
        return false;
    }

    /* initialise the CPU */
    if (!init_cpu(config_set(CONFIG_IRQ_IOAPIC) ? 1 : 0)) {
        return false;
    }
    return true;
}

/* This is the entry function for SMP nodes. Currently unused
 * as we do not support running other nodes */
BOOT_CODE VISIBLE void
boot_node(void)
{
    fail("SMP not supported");
}

BOOT_CODE static void
start_cpu(cpu_id_t cpu_id, paddr_t boot_fun_paddr)
{
    /* memory fence needed before starting the other CPU */
    x86_mfence();

    /* starting the other CPU */
    apic_send_init_ipi(cpu_id);
    apic_send_startup_ipi(cpu_id, boot_fun_paddr);
}

static BOOT_CODE bool_t
try_boot_sys(
    unsigned long multiboot_magic,
    multiboot_info_t* mbi
)
{
    /* ==== following code corresponds to the "select" in abstract specification ==== */

    acpi_rsdt_t* acpi_rsdt; /* physical address of ACPI root */
    paddr_t mods_end_paddr; /* physical address where boot modules end */
    paddr_t load_paddr;
    word_t i;
    p_region_t ui_p_regs;
    multiboot_module_t *modules = (multiboot_module_t*)(word_t)mbi->mod_list;
    cpu_id_t cpus[16];
    uint32_t num_cpus;

    if (multiboot_magic != MULTIBOOT_MAGIC) {
        printf("Boot loader not multiboot compliant\n");
        return false;
    }
    cmdline_parse((const char *)(word_t)mbi->cmdline, &cmdline_opt);

    if ((mbi->flags & MULTIBOOT_INFO_MEM_FLAG) == 0) {
        printf("Boot loader did not provide information about physical memory size\n");
        return false;
    }

    assert(_boot_cpu_end - _boot_cpu_start < 0x400);
    if ((mbi->mem_lower << 10) < BOOT_NODE_PADDR + 0x400) {
        printf("Need at least 513K of available lower physical memory\n");
        return false;
    }

    /* copy CPU bootup code to lower memory */
    memcpy((void*)BOOT_NODE_PADDR, _boot_cpu_start, _boot_cpu_end - _boot_cpu_start);

    /* calculate available physical memory (above 1M) */
    boot_state.avail_p_reg.start = 0x100000;
    boot_state.avail_p_reg.end = ROUND_DOWN(boot_state.avail_p_reg.start + (mbi->mem_upper << 10), PAGE_BITS);

    /* check maximum seL4 can use */
    if (boot_state.avail_p_reg.end > PADDR_TOP) {
        boot_state.avail_p_reg.end = PADDR_TOP;
    }

    printf("Physical memory usable by seL4: start=0x%lx end=0x%lx size=0x%lx\n",
           boot_state.avail_p_reg.start,
           boot_state.avail_p_reg.end,
           boot_state.avail_p_reg.end - boot_state.avail_p_reg.start
          );

    boot_state.ki_p_reg.start = PADDR_LOAD;
    boot_state.ki_p_reg.end = pptr_to_paddr(ki_end);

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

    /* Prepare for accepting device regions from here on */
    boot_state.dev_p_regs.count = 0;

    /* get ACPI root table */
    acpi_rsdt = acpi_init();
    if (!acpi_rsdt) {
        return false;
    }

    if (!config_set(CONFIG_IOMMU) || cmdline_opt.disable_iommu) {
        boot_state.num_drhu = 0;
    } else {
        /* query available IOMMUs from ACPI */
        acpi_dmar_scan(
            acpi_rsdt,
            boot_state.drhu_list,
            &boot_state.num_drhu,
            MAX_NUM_DRHU,
            &boot_state.rmrr_list
        );
    }

    /* query available CPUs from ACPI */
    num_cpus = acpi_madt_scan(acpi_rsdt, cpus, sizeof(cpus) / sizeof(cpus[0]), &boot_state.num_ioapic, boot_state.ioapic_paddr);
    if (num_cpus == 0) {
        printf("No CPUs detected\n");
        return false;
    } else {
        printf("Detected %d CPUs. Only just 1\n", num_cpus);
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

    if (!(mbi->flags & MULTIBOOT_INFO_MODS_FLAG)) {
        printf("Boot loader did not provide information about boot modules\n");
        return false;
    }

    printf("Detected %d boot module(s):\n", mbi->mod_count);
    mods_end_paddr = 0;

    for (i = 0; i < mbi->mod_count; i++) {
        printf(
            "  module #%ld: start=0x%x end=0x%x size=0x%x name='%s'\n",
            i,
            modules[i].start,
            modules[i].end,
            modules[i].end - modules[i].start,
            (char *) (long)modules[i].name
        );
        if ((int32_t)(modules[i].end - modules[i].start) <= 0) {
            printf("Invalid boot module size! Possible cause: boot module file not found by QEMU\n");
            return false;
        }
        if (mods_end_paddr < modules[i].end) {
            mods_end_paddr = modules[i].end;
        }
    }
    mods_end_paddr = ROUND_UP(mods_end_paddr, PAGE_BITS);
    assert(mods_end_paddr > boot_state.ki_p_reg.end);

    if (mbi->mod_count < 1) {
        printf("Expect at least one boot module (containing a userland image)\n");
        return false;
    }

    printf("ELF-loading userland images from boot modules:\n");
    load_paddr = mods_end_paddr;

    load_paddr = load_boot_module(modules, load_paddr);
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
    memcpy((void*)ui_p_regs.start, (void*)mods_end_paddr, ui_p_regs.end - ui_p_regs.start);

    /* adjust p_reg and pv_offset to final load address */
    boot_state.ui_info.p_reg.start -= mods_end_paddr - ui_p_regs.start;
    boot_state.ui_info.p_reg.end   -= mods_end_paddr - ui_p_regs.start;
    boot_state.ui_info.pv_offset   -= mods_end_paddr - ui_p_regs.start;

    /* ==== following code corresponds to abstract specification after "select" ==== */

    /* exclude kernel image from available memory */
    assert(boot_state.avail_p_reg.start == boot_state.ki_p_reg.start);
    boot_state.avail_p_reg.start = boot_state.ki_p_reg.end;

    /* exclude userland images from available memory */
    assert(boot_state.avail_p_reg.start == ui_p_regs.start);
    boot_state.avail_p_reg.start = ui_p_regs.end;

    discover_devices();

    printf("Starting node #0\n");
    if (!try_boot_sys_node(cpus[0])) {
        return false;
    }

    if (config_set(CONFIG_IRQ_IOAPIC)) {
        ioapic_init(1, cpus, boot_state.num_ioapic);
    }

    /* No other CPUs to start up right now */
    (void)start_cpu;
    return true;
}

BOOT_CODE VISIBLE void
boot_sys(
    unsigned long multiboot_magic,
    multiboot_info_t* mbi)
{
    bool_t result;
    result = try_boot_sys(multiboot_magic, mbi);

    if (!result) {
        fail("boot_sys failed for some reason :(\n");
    }
}

