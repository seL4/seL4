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
#include <machine/io.h>
#include <arch/kernel/apic.h>
#include <arch/model/statedata.h>
#include <arch/linker.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <plat/machine.h>

#include <plat/machine/intel-vtd.h>

BOOT_CODE bool_t platAddDevices(void)
{
    /* remove the MSI region as poking at this is undefined and may allow for
     * the user to generate arbitrary MSI interrupts. Only need to consider
     * this if it would actually be in the user device region */
    if (PADDR_USER_DEVICE_TOP > 0xFFFFFFF8) {
        if (!add_allocated_p_region( (p_region_t) {
        (word_t)0xFFFFFFF8, (word_t)0xFFFFFFF8 + 8
        })) {
            return false;
        }
    }
    return true;
}

/* ============================== timer ============================== */

BOOT_CODE VISIBLE uint32_t
tsc_init(void)
{
    x86_cpu_identity_t *model_info = x86_cpuid_get_model_info();
    uint32_t valid_models[] = { HASWELL_1_MODEL_ID, HASWELL_2_MODEL_ID,
                                HASWELL_3_MODEL_ID, HASWELL_4_MODEL_ID,
                                IVY_BRIDGE_1_MODEL_ID,
                                IVY_BRIDGE_2_MODEL_ID,
                                IVY_BRIDGE_3_MODEL_ID
                              };

    /* try to read the frequency from the platform info MSR */
    if (model_info->family == IA32_PREFETCHER_COMPATIBLE_FAMILIES_ID) {
        for (int i = 0; i < ARRAY_SIZE(valid_models); i++) {
            if (model_info->model == valid_models[i]) {

                /* read tsc freq from the platform info msr */
                uint64_t info = x86_rdmsr(IA32_PLATFORM_INFO_MSR);
                uint32_t ratio = (((uint32_t) info) & 0xFF00) >> 8u;
                return (ratio * 100u); // this gives Mhz
            }
        }
    }

    /* otherwise use the pit to find out the tsc freq */
    pit_init();

    /* wait for pit to wraparound */
    pit_wait_wraparound();

    /* read tsc */
    time_t old_ticks = x86_rdtsc();

    /* measure how many tsc cycles pass while PIT wrapsaround */
    pit_wait_wraparound();

    time_t new_ticks = x86_rdtsc();

    time_t diff = new_ticks - old_ticks;

    /* sanity checks */
    assert((uint32_t) diff == diff);
    assert(new_ticks > old_ticks);

    /* bravo, khz */
    uint32_t cycles_per_ms = (uint32_t) diff / PIT_WRAPAROUND_MS;

    /* finally, return mhz */
    return cycles_per_ms / 1000u;
}
