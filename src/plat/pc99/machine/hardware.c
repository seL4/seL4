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

#define HPET_ADDR 0xFED00000

/* Device discovery. For the pc99 platform we assume a pci bus and the presence of the
 * standard bios regions */
void platAddDevices(void)
{
    /* discover PCI devices and their regions */
    /* pci_scan() calls insert_dev_p_reg() for each device region */
    pci_scan();
    /* Add the text mode (EGA) frame buffer. 1 frame is enough for the
     * standard 80x25 text mode. This whole thing is a bit of a hack */
    insert_dev_p_reg( (p_region_t) {
        BIOS_PADDR_VIDEO_RAM_TEXT_MODE_START, BIOS_PADDR_VIDEO_RAM_TEXT_MODE_START + 0x1000
    } );

    /* add the hpet because we can't currently do ACPI parsing at user level,
     * this is the hardcoded hpet of our local machines */
    insert_dev_p_reg((p_region_t) {
        HPET_ADDR, HPET_ADDR + 0x1000
    });
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
