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
/* start/end of .ndks section */
extern char _ndks_start[1];
extern char _ndks_end[1];

/* start/end of kernel stack */
extern char _kernel_stack_bottom[1];
extern char _kernel_stack_top[1];

/* kernel entry point */
extern char _start[1];
#endif

/* constants */

#define MULTIBOOT_HIGHMEM_START 0x100000
#define BOOT_NODE_PADDR 0x80000
#define NDKS_SIZE 0x3000
compile_assert(align_ndks_size, IS_ALIGNED(NDKS_SIZE, PAGE_BITS))
compile_assert(max_ndks_size, NDKS_SIZE <= PPTR_KDEV - PPTR_NDKS)

/* type definitions (directly corresponding to abstract specification) */

typedef struct glks {
    p_region_t   avail_p_reg; /* region of available physical memory on platform */
    p_region_t   ki_p_reg;    /* region where the kernel image is in */
    p_region_t   sh_p_reg;    /* region shared between nodes */
    uint32_t     num_nodes;   /* number of nodes */
    cpu_id_t     cpu_list       [CONFIG_MAX_NUM_NODES]; /* CPUs assigned to nodes */
    ui_info_t    ui_info_list   [CONFIG_MAX_NUM_NODES]; /* info about userland images */
    dev_p_regs_t dev_p_regs;  /* device memory regions */
    uint32_t     apic_khz;    /* frequency of APIC/bus */
    uint32_t     num_ioapic;  /* number of IOAPICs detected */
    paddr_t      ioapic_paddr[CONFIG_MAX_NUM_IOAPIC];
    vesa_info_t  vesa_info;   /* vesa information given by multiboot */
    ia32_mem_region_t mem_regions[CONFIG_MAX_MEM_REGIONS]; /* memory regions as given by multiboot */
#ifdef CONFIG_IOMMU
    uint32_t     num_drhu; /* number of IOMMUs */
    paddr_t      drhu_list[MAX_NUM_DRHU]; /* list of physical addresses of the IOMMUs */
    acpi_rmrr_list_t rmrr_list;
#endif
} glks_t;

typedef char ndks_t[NDKS_SIZE];

/* global variables (called var_glks, var_ndks_list in abstract specification) */

BOOT_DATA_GLOB
glks_t glks;

DATA_GLOB ALIGN(BIT(PAGE_BITS))
ndks_t ndks_list[CONFIG_MAX_NUM_NODES];

/* The kernel stack is actually allocated per-node as part of ndks_list, above.
 * The following definition, in conjunction with the linker script, tells the
 * linker to reserve space in virtual memory at the start of the NDKS section.
 */
SECTION(".ndks.stack") ALIGN(BIT(PAGE_BITS))
char kernel_stack_alloc[4096];

/* global variables (not covered by abstract specification) */

BOOT_DATA_GLOB
cmdline_opt_t cmdline_opt;

#ifdef CONFIG_PAE_PAGING
DATA_GLOB ALIGN(BIT(PDPT_BITS))
uint64_t kernel_pdpt_list[CONFIG_MAX_NUM_NODES][BIT(PDPT_BITS)];
#define paging_structure_t uint64_t
#else
/* In non PAE paging we define the pdpt to be the pd. This is just to
 * allow for there to be common boot code for paging structures on
 * both platforms. This common code detects if it is passed a pdpt
 * and pd at the same address, and ignores the pdpt if this happens
 */
#define kernel_pdpt_list kernel_pd_list
#define paging_structure_t uint32_t
#endif

/* the array type is explicit instead of pde_t due to a c-parser limitation */
DATA_GLOB ALIGN(BIT(PD_SIZE_BITS))
paging_structure_t kernel_pd_list[CONFIG_MAX_NUM_NODES][BIT(PD_BITS + PDPT_BITS)];

/* the array type is explicit instead of pte_t due to a c-parser limitation */
DATA_GLOB ALIGN(BIT(PT_SIZE_BITS))
paging_structure_t kernel_pt_list[CONFIG_MAX_NUM_NODES][BIT(PT_BITS)];

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

BOOT_CODE uint16_t
console_port_of_node(node_id_t node_id)
{
    return cmdline_opt.console_port[node_id];
}

BOOT_CODE uint16_t
debug_port_of_node(node_id_t node_id)
{
    return cmdline_opt.debug_port[node_id];
}
#endif

/* functions not modeled in abstract specification */

BOOT_CODE static paddr_t
load_boot_module(node_id_t node, multiboot_module_t* boot_module, paddr_t load_paddr)
{
    Elf32_Header_t* elf_file = (Elf32_Header_t*)boot_module->start;
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

    printf("size=0x%x v_entry=0x%x v_start=0x%x v_end=0x%x ",
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
    glks.ui_info_list[node].pv_offset = load_paddr - v_reg.start;
    glks.ui_info_list[node].p_reg.start = load_paddr;
    load_paddr += v_reg.end - v_reg.start;
    glks.ui_info_list[node].p_reg.end = load_paddr;
    glks.ui_info_list[node].v_entry = elf_file->e_entry;

    printf("p_start=0x%x p_end=0x%x\n",
           glks.ui_info_list[node].p_reg.start,
           glks.ui_info_list[node].p_reg.end
          );

    if (load_paddr > glks.avail_p_reg.end) {
        printf("End of loaded userland image lies outside of usable physical memory\n");
        return 0;
    }

    /* initialise all initial userland memory and load potentially sparse ELF image */
    memzero(
        (void*)glks.ui_info_list[node].p_reg.start,
        glks.ui_info_list[node].p_reg.end - glks.ui_info_list[node].p_reg.start
    );
    elf32_load(elf_file, glks.ui_info_list[node].pv_offset);

    return load_paddr;
}

BOOT_CODE void
insert_dev_p_reg(p_region_t reg)
{
    if (glks.dev_p_regs.count < sizeof(glks.dev_p_regs.list) / sizeof(glks.dev_p_regs.list[0])) {
        glks.dev_p_regs.list[glks.dev_p_regs.count] = reg;
        glks.dev_p_regs.count++;
        printf("\n");
    } else {
        printf(" -> IGNORED! (too many)\n");
    }
}

/* functions directly corresponding to abstract specification */

BOOT_CODE cpu_id_t
cur_cpu_id(void)
{
    cpu_id_t cpu_id;
    paddr_t  esp = pptr_to_paddr(get_current_esp());

    if (esp <= (paddr_t)_boot_stack_top && esp > (paddr_t)_boot_stack_bottom) {
        cpu_id = glks.cpu_list[0];
    } else {
        cpu_id = esp >> 11;
    }

    return cpu_id;
}

BOOT_CODE node_id_t
node_of_cpu(cpu_id_t cpu_id)
{
    node_id_t i;

    for (i = 0; i < glks.num_nodes;  i++) {
        if (glks.cpu_list[i] == cpu_id) {
            return i;
        }
    }
    /* Is it even possible for this to happen? */
    fail("Couldn't find node of CPU");
}

/* split a region of physical memory into n mutually disjoint pieces */

BOOT_CODE static p_region_t
split_region(unsigned int i, unsigned int n, p_region_t reg)
{
    uint32_t offset;
    uint32_t total_frames = (reg.end - reg.start) >> PAGE_BITS;
    uint32_t frames_div = total_frames / n;
    uint32_t frames_mod = total_frames % n;

    if (i < frames_mod) {
        offset = (i * (frames_div + 1)) << PAGE_BITS;
        return (p_region_t) {
            .start = reg.start + offset,
             .end   = reg.start + offset + ((frames_div + 1) << PAGE_BITS)
        };
    } else {
        offset = (frames_mod * (frames_div + 1) + (i - frames_mod) * frames_div) << PAGE_BITS;
        return (p_region_t) {
            .start = reg.start + offset,
             .end   = reg.start + offset + (frames_div << PAGE_BITS)
        };
    }
}

BOOT_CODE static bool_t
lift_ndks(node_id_t node_id)
{
    p_region_t ndks_p_reg;

    ndks_p_reg.start = pptr_to_paddr(ndks_list[node_id]);
    ndks_p_reg.end = ndks_p_reg.start + NDKS_SIZE;

    if (!map_kernel_window(
                (pdpte_t*)kernel_pdpt_list[node_id],
                (pde_t*)kernel_pd_list[node_id],
                (pte_t*)kernel_pt_list[node_id],
                ndks_p_reg
#ifdef CONFIG_IRQ_IOAPIC
                , glks.num_ioapic,
                glks.ioapic_paddr
#endif
#ifdef CONFIG_IOMMU
                , node_id == 0 ? glks.num_drhu : 0,
                glks.drhu_list
#endif
            )) {
        return false;
    }
    write_cr3(pptr_to_paddr(kernel_pdpt_list[node_id]));
    /* Sync up the compilers view of the world here to force the PD to actually
     * be set *right now* instead of delayed */
    asm volatile("" ::: "memory");
    return true;
}

static BOOT_CODE bool_t
try_boot_node(void)
{
    p_region_t boot_mem_reuse_p_reg;

    cpu_id_t   cpu_id  = cur_cpu_id();
    node_id_t  node_id = node_of_cpu(cpu_id);

    uint32_t      num_nodes  = glks.num_nodes;
    ui_info_t     ui_info    = glks.ui_info_list[node_id];
    dev_p_regs_t* dev_p_regs = &glks.dev_p_regs;

    /* calculate this node's available physical memory */
    p_region_t this_avail_p_reg = split_region(node_id, num_nodes, glks.avail_p_reg);

    /* if we only boot up one node, we can reuse boot code/data memory */
    if (num_nodes == 1) {
        boot_mem_reuse_p_reg.start = PADDR_LOAD;
        boot_mem_reuse_p_reg.end = (paddr_t)ki_boot_end - BASE_OFFSET;
    } else {
        boot_mem_reuse_p_reg = P_REG_EMPTY;
    }

    /* map NDKS (node kernel state) into PD/PT and activate PD */
    if (!lift_ndks(node_id)) {
        return false;
    }

    /* initialise NDKS and kernel heap */
    if (!init_node_state(
                this_avail_p_reg,
                glks.sh_p_reg,
                dev_p_regs,
                ui_info,
                boot_mem_reuse_p_reg,
                node_id,
                num_nodes,
                glks.cpu_list,
                /* parameters below not modeled in abstract specification */
                (pdpte_t*)kernel_pdpt_list[node_id],
                (pde_t*)kernel_pd_list[node_id],
                (pte_t*)kernel_pt_list[node_id],
                &glks.vesa_info,
                glks.mem_regions
#ifdef CONFIG_IOMMU
                , cpu_id,
                node_id == 0 ? glks.num_drhu : 0,
                glks.drhu_list,
                &glks.rmrr_list
#endif
            )) {
        return false;
    }

    /* initialise the CPU */
    if (!init_node_cpu(
                glks.apic_khz,
#ifdef CONFIG_IRQ_IOAPIC
                1
#else
                node_id != 0
#endif
            )) {
        return false;
    }
    return true;
}

/* This is the entry function for SMP nodes. Node 0 calls
 * try_boot_node directly */
BOOT_CODE VISIBLE void
boot_node(void)
{
    bool_t result;
    result = try_boot_node();
    if (!result) {
        fail("Failed to start node :(\n");
    }
}

BOOT_CODE static void
start_cpu(cpu_id_t cpu_id, paddr_t boot_fun_paddr)
{
    /* memory fence needed before starting the other CPU */
    ia32_mfence();

    /* starting the other CPU */
    apic_send_init_ipi(cpu_id);
    apic_send_startup_ipi(cpu_id, boot_fun_paddr);
}

static BOOT_CODE bool_t
try_boot_sys(
    unsigned long multiboot_magic,
    multiboot_info_t* mbi,
    uint32_t apic_khz
)
{
    /* ==== following code corresponds to the "select" in abstract specification ==== */

    acpi_rsdt_t* acpi_rsdt; /* physical address of ACPI root */
    paddr_t mods_end_paddr; /* physical address where boot modules end */
    paddr_t load_paddr;
    unsigned int i;
    p_region_t ui_p_regs;
    multiboot_module_t *modules = (multiboot_module_t*)mbi->mod_list;

    glks.num_nodes = 1; /* needed to enable console output */

    if (multiboot_magic != MULTIBOOT_MAGIC) {
        printf("Boot loader not multiboot compliant\n");
        return false;
    }
    cmdline_parse((const char *)mbi->cmdline, &cmdline_opt);

    /* assert correct NDKS location and size */
    assert((uint32_t)_ndks_start == PPTR_NDKS);
    assert(_ndks_end - _ndks_start <= NDKS_SIZE);

    if ((mbi->flags & MULTIBOOT_INFO_MEM_FLAG) == 0) {
        printf("Boot loader did not provide information about physical memory size\n");
        return false;
    }

    assert(_boot_cpu_end - _boot_cpu_start < 0x400);
    if ((mbi->mem_lower << 10) < BOOT_NODE_PADDR + 0x400) {
        printf("Need at least 513K of available lower physical memory\n");
        return false;
    }

    /* copy VESA information from multiboot header */
    if ((mbi->flags & MULTIBOOT_INFO_GRAPHICS_FLAG) == 0) {
        glks.vesa_info.vbe_mode = -1;
        printf("Multiboot gave us no video information :(\n");
    } else {
        glks.vesa_info.vbe_control_info = *mbi->vbe_control_info;
        glks.vesa_info.vbe_mode_info = *mbi->vbe_mode_info;
        glks.vesa_info.vbe_mode = mbi->vbe_mode;
        printf("Got VBE info in multiboot. Current video mode is %d\n", mbi->vbe_mode);
        glks.vesa_info.vbe_interface_seg = mbi->vbe_interface_seg;
        glks.vesa_info.vbe_interface_off = mbi->vbe_interface_off;
        glks.vesa_info.vbe_interface_len = mbi->vbe_interface_len;
    }
    /* copy memory map from multiboot header */
    if ((mbi->flags & MULTIBOOT_INFO_MEM_MAP) != 0) {
        multiboot_memory_map_t *map = (multiboot_memory_map_t*)((uint32_t)mbi->mmap_addr);
        multiboot_memory_map_t *map_end = (multiboot_memory_map_t*)((uint32_t)mbi->mmap_addr + mbi->mmap_length);
        i = 0;
        while (map < map_end && i < CONFIG_MAX_MEM_REGIONS) {
            if (map->type == MULTIBOOT_MEMORY_AVAILABLE) {
                /* We will freely describe memory in the kernel window and leave it up
                 * to userland to not use it. Also taunt the user with
                 * memory that is >4gb that they cannot yet use */
                glks.mem_regions[i].paddr = map->addr;
                glks.mem_regions[i].len = map->len;
                i++;
                printf("Found memory at 0x%x:0x%x - 0x%x:0x%x\n", (uint32_t)(map->addr >> 32), (uint32_t)map->addr, (uint32_t)( (map->addr + map->len) >> 32), (uint32_t)(map->addr + map->len));
            }
            /* The 'size' element in the multiboot struct is technically at offset -4 in the struct
             * so we need to add 4 here for everything to work. Please don't think on this too hard */
            map = (multiboot_memory_map_t*)((uint32_t)map + map->size + 4);
        }
        if (map < map_end) {
            printf("Found > %d memory regions. Consider increasing CONFIG_MAX_MEM_REGIONS\n", CONFIG_MAX_MEM_REGIONS);
        }
    } else {
        printf("Multiboot gave us no memory map :(\n");
        i = 0;
    }
    while (i < CONFIG_MAX_MEM_REGIONS) {
        glks.mem_regions[i].paddr = 0;
        glks.mem_regions[i].len = 0;
        i++;
    }

    /* copy CPU bootup code to lower memory */
    memcpy((void*)BOOT_NODE_PADDR, _boot_cpu_start, _boot_cpu_end - _boot_cpu_start);

    printf("Physical high memory given to seL4: start=0x%x end=0x%x size=0x%x\n",
           MULTIBOOT_HIGHMEM_START,
           mbi->mem_upper << 10,
           (mbi->mem_upper << 10) - MULTIBOOT_HIGHMEM_START);
    /* calculate available physical memory (above 1M) */
    glks.avail_p_reg.start = MULTIBOOT_HIGHMEM_START;
    glks.avail_p_reg.end = ROUND_DOWN(glks.avail_p_reg.start + (mbi->mem_upper << 10), PAGE_BITS);
    if (glks.avail_p_reg.end > PADDR_TOP) {
        glks.avail_p_reg.end = PADDR_TOP;
    }

    printf("Physical memory usable by seL4 (kernel): start=0x%x end=0x%x size=0x%x\n",
           glks.avail_p_reg.start,
           glks.avail_p_reg.end,
           glks.avail_p_reg.end - glks.avail_p_reg.start
          );

    glks.ki_p_reg.start = PADDR_LOAD;
    glks.ki_p_reg.end = pptr_to_paddr(ki_end);

    printf("Kernel loaded to: start=0x%x end=0x%x size=0x%x entry=0x%x\n",
           glks.ki_p_reg.start,
           glks.ki_p_reg.end,
           glks.ki_p_reg.end - glks.ki_p_reg.start,
           (paddr_t)_start
          );
    printf("Kernel stack size: 0x%x\n", _kernel_stack_top - _kernel_stack_bottom);

    glks.apic_khz = apic_khz;
    printf("APIC: Bus frequency is %d MHz\n", glks.apic_khz / 1000);

    /* remapping legacy IRQs to their correct vectors */
    pic_remap_irqs(IRQ_INT_OFFSET);
#ifdef CONFIG_IRQ_IOAPIC
    /* Disable the PIC so that it does not generate any interrupts. We need to
     * do this *before* we initialize the apic */
    pic_disable();
#endif

    /* Prepare for accepting device regions from here on */
    glks.dev_p_regs.count = 0;

    /* get ACPI root table */
    acpi_rsdt = acpi_init();
    if (!acpi_rsdt) {
        return false;
    }

#ifdef CONFIG_IOMMU
    if (cmdline_opt.disable_iommu) {
        glks.num_drhu = 0;
    } else {
        /* query available IOMMUs from ACPI */
        acpi_dmar_scan(
            acpi_rsdt,
            glks.drhu_list,
            &glks.num_drhu,
            MAX_NUM_DRHU,
            &glks.rmrr_list
        );
    }
#endif

    /* query available CPUs from ACPI */
    glks.num_nodes = acpi_madt_scan(acpi_rsdt, glks.cpu_list, CONFIG_MAX_NUM_NODES, &glks.num_ioapic, glks.ioapic_paddr);
    if (glks.num_nodes == 0) {
        printf("No CPUs detected\n");
        return false;
    }
#ifdef CONFIG_IRQ_IOAPIC
    if (glks.num_ioapic == 0) {
        printf("No IOAPICs detected\n");
        return false;
    }
#else
    if (glks.num_ioapic > 0) {
        printf("Detected %d IOAPICs, but configured to use PIC instead\n", glks.num_ioapic);
    }
#endif

    if (glks.num_nodes > cmdline_opt.max_num_nodes) {
        glks.num_nodes = cmdline_opt.max_num_nodes;
    }
    printf("Will boot up %d seL4 node(s)\n", glks.num_nodes);

    if (!(mbi->flags & MULTIBOOT_INFO_MODS_FLAG)) {
        printf("Boot loader did not provide information about boot modules\n");
        return false;
    }

    printf("Detected %d boot module(s):\n", mbi->mod_count);
    mods_end_paddr = 0;

    for (i = 0; i < mbi->mod_count; i++) {
        printf(
            "  module #%d: start=0x%x end=0x%x size=0x%x name='%s'\n",
            i,
            modules[i].start,
            modules[i].end,
            modules[i].end - modules[i].start,
            modules[i].name
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
    assert(mods_end_paddr > glks.ki_p_reg.end);

    if (mbi->mod_count < 1) {
        printf("Expect at least one boot module (containing a userland image)\n");
        return false;
    }

    printf("ELF-loading userland images from boot modules:\n");
    load_paddr = mods_end_paddr;

    for (i = 0; i < mbi->mod_count && i < glks.num_nodes; i++) {
        printf("  module #%d for node #%d: ", i, i);
        load_paddr = load_boot_module(i, modules + i, load_paddr);
        if (!load_paddr) {
            return false;
        }
    }

    for (i = mbi->mod_count; i < glks.num_nodes; i++) {
        printf("  module #%d for node #%d: ", mbi->mod_count - 1, i);
        load_paddr = load_boot_module(i, modules + mbi->mod_count - 1, load_paddr);
        if (!load_paddr) {
            return false;
        }
    }

    /* calculate final location of userland images */
    ui_p_regs.start = glks.ki_p_reg.end;
    ui_p_regs.end = ui_p_regs.start + load_paddr - mods_end_paddr;

    printf(
        "Moving loaded userland images to final location: from=0x%x to=0x%x size=0x%x\n",
        mods_end_paddr,
        ui_p_regs.start,
        ui_p_regs.end - ui_p_regs.start
    );
    memcpy((void*)ui_p_regs.start, (void*)mods_end_paddr, ui_p_regs.end - ui_p_regs.start);

    for (i = 0; i < glks.num_nodes; i++) {
        /* adjust p_reg and pv_offset to final load address */
        glks.ui_info_list[i].p_reg.start -= mods_end_paddr - ui_p_regs.start;
        glks.ui_info_list[i].p_reg.end   -= mods_end_paddr - ui_p_regs.start;
        glks.ui_info_list[i].pv_offset   -= mods_end_paddr - ui_p_regs.start;
    }

    /* ==== following code corresponds to abstract specification after "select" ==== */

    /* exclude kernel image from available memory */
    assert(glks.avail_p_reg.start == glks.ki_p_reg.start);
    glks.avail_p_reg.start = glks.ki_p_reg.end;

    /* exclude userland images from available memory */
    assert(glks.avail_p_reg.start == ui_p_regs.start);
    glks.avail_p_reg.start = ui_p_regs.end;

    /* choose shared memory */
    glks.sh_p_reg.start = glks.avail_p_reg.start;
    glks.sh_p_reg.end = glks.sh_p_reg.start + (cmdline_opt.num_sh_frames << PAGE_BITS);
    if (glks.sh_p_reg.end > glks.avail_p_reg.end || glks.sh_p_reg.end < glks.sh_p_reg.start) {
        printf("Not enough usable physical memory to allocate shared region\n");
        return false;
    }

    /* exclude shared region from available memory */
    assert(glks.avail_p_reg.start == glks.sh_p_reg.start);
    glks.avail_p_reg.start = glks.sh_p_reg.end;

    /* Add in all the memory except for the kernel window as device memory.
     * This is UNSAFE as we are giving user level access to regions of memory,
     * such as the APIC, that it realy should not be able to use. This needs to
     * be fixed by blacklisting regions.
     * We also need to make sure we do not use the address that will get translated
     * to the NULL pptr, or that will be considered a NULL physical address */
    insert_dev_p_reg( (p_region_t) {
        .start = 0x1000, .end = 0x100000
    } );
    insert_dev_p_reg( (p_region_t) {
        .start = glks.avail_p_reg.end, .end = pptr_to_paddr(0)
    });
    /* Specifying zero here for the .end really is correct */
    insert_dev_p_reg( (p_region_t) {
        .start = pptr_to_paddr(0) + 0x1000, .end = 0
    });

    printf("Starting node #0\n");
    if (!try_boot_node()) {
        return false;
    }

#ifdef CONFIG_IRQ_IOAPIC
    /* Now that NDKS have been lifted we can access the IOAPIC and program it */
    ioapic_init(glks.num_nodes, glks.cpu_list, glks.num_ioapic);
#endif

    /* start up other CPUs and initialise their nodes */
    for (i = 1; i < glks.num_nodes; i++) {
        printf("Starting node #%d\n", i);
        start_cpu(glks.cpu_list[i], BOOT_NODE_PADDR);
    }
    return true;
}

BOOT_CODE VISIBLE void
boot_sys(
    unsigned long multiboot_magic,
    multiboot_info_t* mbi,
    uint32_t apic_khz)
{
    bool_t result;
    result = try_boot_sys(multiboot_magic, mbi, apic_khz);

    if (!result) {
        fail("boot_sys failed for some reason :(\n");
    }
}

