/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/io.h>
#include <arch/kernel/apic.h>
#include <arch/model/statedata.h>
#include <linker.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <plat/machine.h>

#include <plat/machine/intel-vtd.h>

BOOT_CODE bool_t platAddDevices(void)
{

    /* The last 64 KiByte page of the physical address space is not allowed to
     * be used to to prevent userland from exploiting a corner case. If e.g. a
     * syscall is placed at the very last instruction, the hardware generates a
     * return address of that address +2, which is non-canonical. The kernel
     * loads that invalid address into sysret and this may cause undefined
     * behavior eventually.
     * See also https://sel4.atlassian.net/browse/SELFOUR-1179.
     */
    p_region_t p_reg_reserved = {
        .start = ROUND_DOWN(CONFIG_PHYS_ADDR_TOP, 16),
        .end   = CONFIG_PHYS_ADDR_TOP
    };

    if (!reserve_region(p_reg_reserved)) {
        printf("ERROR: could not reserve last 64k of phys memory "
               "[0x%"SEL4_PRIx_word": 0x%08"SEL4_PRIx_word"\n",
               p_reg_reserved.start, p_reg_reserved.end);
        return false;
    }

#ifndef CONFIG_ARCH_IA32

    /* remove the 8 byte MSI region [0xfffffff8 - ffffffff] at the end of the
     * 32-bit address range, as poking at this area is undefined and may allow
     * for the user to generate arbitrary MSI interrupts. Only need to consider
     * this if it would actually be in the user device region.
     */
    p_region_t p_reg_msi = {
        .start = SEL4_WORD_CONST(0xfffffff8),
        .end   = SEL4_WORD_CONST(0xffffffff)
    };

    if (CONFIG_PHYS_ADDR_TOP > p_reg_msi.start) {
        if (!reserve_region(p_reg_msi)) {
            printf("ERROR: could not reserve MSI region "
                   "[0x%"SEL4_PRIx_word": 0x%08"SEL4_PRIx_word"\n",
                   p_reg_msi.start, p_reg_msi.end);
            return false;
        }
    }

#endif /* not CONFIG_ARCH_IA32 */

    return true;
}

/* ============================== timer ============================== */

#define TSC_FREQ_RETRIES 10

BOOT_CODE static inline uint32_t measure_tsc_khz(void)
{
    /* The frequency is repeatedly measured until the number of TSC
     * ticks in the pit wraparound interval (~50ms) fits in 32 bits.
     * On bare metal, this should succeed immediately, since x86
     * guarantees the number of TSC ticks in a second can be stored
     * in 32 bits. When running in a simulator, it's possible for
     * the emulation (or not) of the PIT and TSC to occasionally
     * allow too many TSC ticks per PIT wraparound. This loop
     * repeatedly measures the TSC ticks per PIT wraparound under
     * the expectation that it will eventually yield a sensible
     * result.
     */
    for (int i = 0; i < TSC_FREQ_RETRIES; i++) {

        /* read tsc */
        uint64_t old_ticks = x86_rdtsc();

        /* measure how many tsc cycles pass while PIT wraps around */
        pit_wait_wraparound();

        uint64_t new_ticks = x86_rdtsc();

        uint64_t diff = new_ticks - old_ticks;

        if ((uint32_t)diff == diff && new_ticks > old_ticks) {
            return (uint32_t)diff / PIT_WRAPAROUND_MS;
        }

        printf("warning: TSC frequency too high (%d retries remaining)\n",
               TSC_FREQ_RETRIES - i - 1);
    }

    fail("TSC frequency too high");

    /* should never get here */
    return 0;
}

BOOT_CODE uint32_t tsc_init(void)
{

#if CONFIG_PC99_TSC_FREQUENCY > 0
    return CONFIG_PC99_TSC_FREQUENCY / 1000000;
#endif

    x86_cpu_identity_t *model_info = x86_cpuid_get_model_info();
    uint32_t valid_models[] = {
        NEHALEM_1_MODEL_ID, NEHALEM_2_MODEL_ID, NEHALEM_3_MODEL_ID,
        SANDY_BRIDGE_1_MODEL_ID, SANDY_BRIDGE_2_MODEL_ID,
        HASWELL_1_MODEL_ID, HASWELL_2_MODEL_ID, HASWELL_3_MODEL_ID, HASWELL_4_MODEL_ID,
        IVY_BRIDGE_1_MODEL_ID, IVY_BRIDGE_2_MODEL_ID, IVY_BRIDGE_3_MODEL_ID,
        /* BROADWELL_1_MODEL_ID is an Atom that doesn't support the MSR */
        BROADWELL_2_MODEL_ID, BROADWELL_3_MODEL_ID, BROADWELL_4_MODEL_ID, BROADWELL_5_MODEL_ID,
        SKYLAKE_1_MODEL_ID, SKYLAKE_2_MODEL_ID
    };

    /* try to read the frequency from the platform info MSR */
    if (model_info->family == IA32_PREFETCHER_COMPATIBLE_FAMILIES_ID) {
        for (int i = 0; i < ARRAY_SIZE(valid_models); i++) {
            if (model_info->model == valid_models[i]) {

                /* read tsc freq from the platform info msr. Under some environments such
                 * as KVM this MSR will cause a GP fault even though it should be there.
                 * As such we perform a 'safe' rdmsr, which will catch a GP fault and
                 * tells through .success whether or not one happened */
                rdmsr_safe_result_t info = x86_rdmsr_safe(IA32_PLATFORM_INFO_MSR);
                if (info.success) {
                    uint32_t ratio = (((uint32_t) info.value) & 0xFF00) >> 8u;
                    /* Ignore hardware that claims a tsc frequency of zero */
                    if (ratio != 0) {
                        /* Convert to MHz */
                        if (model_info->model == NEHALEM_1_MODEL_ID ||
                            model_info->model == NEHALEM_2_MODEL_ID ||
                            model_info->model == NEHALEM_3_MODEL_ID) {
                            return ratio * 13333u / 100u;
                        } else {
                            return ratio * 100u;
                        }
                    }
                }
                /* We just found the matching model, so no point continuing the search */
                break;
            }
        }
    }

    /* otherwise use the pit to find out the tsc freq */
    pit_init();

    /* wait for pit to wraparound */
    pit_wait_wraparound();

    /* count tsc ticks per ms */
    uint32_t tsc_khz = measure_tsc_khz();

    /* finally, return mhz */
    return tsc_khz / 1000u;
}
