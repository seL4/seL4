/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <config.h>
#include <arch/machine.h>
#include <arch/kernel/boot_sys.h>
#include <arch/kernel/smp_sys.h>
#include <arch/kernel/lock.h>

#if CONFIG_MAX_NUM_NODES > 1

/* Index of next AP to boot, BSP has index zero */
BOOT_DATA
volatile word_t smp_aps_index = 1;

/* These are the stacks used in multi processor mode. There are a lot of assumptions
 * on this being page aligned and precisely 4K in size. DO NOT MODIFY */
ALIGN(BIT(PAGE_BITS)) VISIBLE
char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(seL4_PageBits)];

#ifdef CONFIG_USE_LOGICAL_IDS
BOOT_CODE static void
update_logical_id_mappings(void)
{
    cpu_mapping.index_to_logical_id[getCurrentCPUIndex()] = apic_get_logical_id();

    for (int i = 0; i < smp_aps_index; i++) {
        if (apic_get_cluster(cpu_mapping.index_to_logical_id[getCurrentCPUIndex()]) ==
                apic_get_cluster(cpu_mapping.index_to_logical_id[i])) {

            cpu_mapping.other_indexes_in_cluster[getCurrentCPUIndex()] |= BIT(i);
            cpu_mapping.other_indexes_in_cluster[i] |= BIT(getCurrentCPUIndex());
        }
    }
}
#endif /* CONFIG_USE_LOGICAL_IDS */

BOOT_CODE static void
start_cpu(cpu_id_t cpu_id, paddr_t boot_fun_paddr)
{
    /* memory fence needed before starting the other CPU */
    x86_mfence();

    /* starting the other CPU */
    apic_send_init_ipi(cpu_id);
    apic_send_startup_ipi(cpu_id, boot_fun_paddr);
}

BOOT_CODE void
start_boot_aps(void)
{
    /* update cpu mapping for BSP, cpus[0] is always assumed to be BSP */
    cpu_mapping.index_to_cpu_id[getCurrentCPUIndex()] = boot_state.cpus[0];
#ifdef CONFIG_USE_LOGICAL_IDS
    cpu_mapping.index_to_logical_id[getCurrentCPUIndex()] = apic_get_logical_id();
#endif /* CONFIG_USE_LOGICAL_IDS */

    /* startup APs one at a time as we use shared kernel boot stack */
    while (smp_aps_index < boot_state.num_cpus) {
        word_t current_ap_index = smp_aps_index;

        printf("Starting node #%lu with APIC ID %lu \n",
               current_ap_index, boot_state.cpus[current_ap_index]);

        /* update cpu mapping for APs, store APIC ID of the next booting AP
         * as APIC ID are not continoius e.g. 0,2,1,3 for 4 cores with hyperthreading
         * we need to store a mapping to translate the index to real APIC ID */
        cpu_mapping.index_to_cpu_id[current_ap_index] = boot_state.cpus[current_ap_index];
        start_cpu(boot_state.cpus[current_ap_index], BOOT_NODE_PADDR);

        /* wait for current AP to boot up */
        while (smp_aps_index == current_ap_index);
    }
}

BOOT_CODE bool_t
copy_boot_code_aps(uint32_t mem_lower)
{
    assert(boot_cpu_end - boot_cpu_start < 0x400);

    if ((mem_lower << 10) < BOOT_NODE_PADDR + 0x400) {
        printf("Need at least 513K of available lower physical memory\n");
        return false;
    }

    /* copy CPU bootup code to lower memory */
    memcpy((void*)BOOT_NODE_PADDR, boot_cpu_start, boot_cpu_end - boot_cpu_start);
    return true;
}

static BOOT_CODE bool_t
try_boot_node(void)
{
    setCurrentVSpaceRoot(kpptr_to_paddr(X86_GLOBAL_VSPACE_ROOT), 0);
    /* Sync up the compilers view of the world here to force the PD to actually
     * be set *right now* instead of delayed */
    asm volatile("" ::: "memory");

    /* initialise the CPU, make sure legacy interrupts are disabled */
    if (!init_cpu(1)) {
        return false;
    }

#ifdef CONFIG_USE_LOGICAL_IDS
    update_logical_id_mappings();
#endif /* CONFIG_USE_LOGICAL_IDS */
    return true;
}

/* This is the entry function for APs. However, it is not a BOOT_CODE as
 * there is a race between exiting this function and root task running on
 * node #0 to possibly reallocate this memory */
VISIBLE void
boot_node(void)
{
    bool_t result;
    result = try_boot_node();

    if (!result) {
        fail("boot_node failed for some reason :(\n");
    }

    smp_aps_index++;

    /* grab BKL before leaving the kernel */
    NODE_LOCK;

    NODE_STATE(ksCurThread) = NODE_STATE(ksIdleThread);
    NODE_STATE(ksSchedulerAction) = SchedulerAction_ChooseNewThread;
    ARCH_NODE_STATE(x86KScurInterrupt) = int_invalid;

    schedule();
    activateThread();
}

#endif /* CONFIG_MAX_NUM_NODES */
