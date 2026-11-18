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
    /* remove the MSI region as poking at this is undefined and may allow for
     * the user to generate arbitrary MSI interrupts. Only need to consider
     * this if it would actually be in the user device region */
    if (CONFIG_PADDR_USER_DEVICE_TOP > 0xFFFFFFF8) {
        if (!reserve_region((p_region_t) {
        (word_t)0xFFFFFFF8, (word_t)0xFFFFFFF8 + 8
        })) {
            return false;
        }
    }
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

#define TSC_AND_CRYSTAL_CLK_LEAF 0x15
#define CPUID_PROC_FREQ_LEAF 0x16

BOOT_CODE uint32_t tsc_init(void)
{

#if CONFIG_PC99_TSC_FREQUENCY > 0
    uint32_t config_tsc_mhz = CONFIG_PC99_TSC_FREQUENCY / 1000000;
    printf("tsc configured to %u MHz\n", config_tsc_mhz);
    return config_tsc_mhz;
#endif

    cpu_identity_t *x86_cpuid_identity = x86_cpuid_get_identity();
    if (x86_cpuid_identity->vendor == X86_VENDOR_INTEL) {
        /*
         * See Intel SDM (Sep 2026),
         * section "CPUID.15H -- Time Stamp Counter and Nominal Core Crystal Clock",
         * page "Vol. 1 21-59".
         *
         * > CPUID.15H returns information about the Time Stamp Counter and the Nominal Core Crystal Clock.
         * > • This leaf is valid if MAX_LEAF ≥ 15H.
         * > • This leaf does not contain sub-leaves and provides the same information regardless of the value of ECX. */

        /*
         * Get `MAX_LEAF`, see section "CPUID.00H -- Maximum Input for Basic CPUID and Vendor ID"
         * Page "Vol. 1 21-7"
         */
        uint32_t max_leaf = x86_cpuid_eax(0, 0);
        if (max_leaf >= TSC_AND_CRYSTAL_CLK_LEAF) {
            /*
             * Section "CPUID.15H"
             * > Dividing EBX[31:0] by EAX[31:0] provides the ratio of the TSC frequency to the
             * > core crystal clock frequency. The Timestamp Counter frequency is computed by
             * > multiplying that ratio by ECX[31:0], such as TSC_frequency = ECX * EBX/EAX.
             */
            uint64_t denominator = x86_cpuid_eax(TSC_AND_CRYSTAL_CLK_LEAF, 0);
            uint64_t numerator = x86_cpuid_ebx(TSC_AND_CRYSTAL_CLK_LEAF, 0);
            uint64_t crystal_hz = x86_cpuid_ecx(TSC_AND_CRYSTAL_CLK_LEAF, 0);

            /* See Table 21-53, `numerator` and `crystal_hz` may not be always enumerated by the silicon.
             * Also check the `denominator` be safe. */
            if (numerator && denominator) {
                if (!crystal_hz && max_leaf >= CPUID_PROC_FREQ_LEAF) {
                    /*
                     * Some CPU models from the Skylake era don't report the crystal frequency,
                     * but we can calculate it by considering the CPU base frequency.
                     * From Linux: https://github.com/torvalds/linux/blob/666539eecdf4db623f5e3c3e55cf9f2dfad24aa4/arch/x86/kernel/tsc.c#L690-L702 */
                    uint64_t proc_base_mhz = x86_cpuid_eax(CPUID_PROC_FREQ_LEAF, 0);
                    crystal_hz = proc_base_mhz * 1000000ull * denominator / numerator;
                }
                if (crystal_hz) {
                    uint64_t tsc_hz = crystal_hz * numerator / denominator;
                    printf("tsc computed from method #1: %llu MHz\n", tsc_hz / 1000000ull);
                    return tsc_hz / 1000000ull;
                }
            }
        }
    }

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
                    uint64_t tsc_mhz;
                    if (ratio != 0) {
                        /* Convert to MHz */
                        if (model_info->model == NEHALEM_1_MODEL_ID ||
                            model_info->model == NEHALEM_2_MODEL_ID ||
                            model_info->model == NEHALEM_3_MODEL_ID) {
                            tsc_mhz = ratio * 13333u / 100u;
                        } else {
                            tsc_mhz = ratio * 100u;
                        }

                        printf("tsc computed from method #2: %llu MHz\n", tsc_mhz);
                        return tsc_mhz;
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

    printf("tsc measured with pit: %u MHz\n", tsc_khz / 1000u);

    /* finally, return mhz */
    return tsc_khz / 1000u;
}
