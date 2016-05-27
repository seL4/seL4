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

/* ============================== interrupts/IRQs ============================== */

/* Handle a platform-reserved IRQ. */
void handleReservedIRQ(irq_t irq)
{
    if (config_set(CONFIG_IOMMU) && irq == irq_iommu) {
        vtd_handle_fault();
        return;
    }
    printf("Received reserved IRQ: %d\n", (int)irq);
}

/* Get the IRQ number currently working on. */
irq_t getActiveIRQ(void)
{
    if (x86KScurInterrupt == int_invalid) {
        return irqInvalid;
    } else {
        return x86KScurInterrupt - IRQ_INT_OFFSET;
    }
}

/* Checks for pending IRQ */
bool_t isIRQPending(void)
{
    if (apic_is_interrupt_pending()) {
        return true;
    }
#ifdef CONFIG_IRQ_PIC
    if (pic_is_irq_pending()) {
        return true;
    }
#endif
    return false;
}

void ackInterrupt(irq_t irq)
{
#ifdef CONFIG_IRQ_PIC
    if (irq <= irq_isa_max) {
        pic_ack_active_irq();
    } else
#endif
    {
        apic_ack_active_interrupt();
    }
}

void handleSpuriousIRQ(void)
{
    /* Do nothing */
}

/* ============================== timer ============================== */
static CONST uint32_t
clz64(uint64_t n)
{
    uint32_t upper_n = (uint32_t) (n >> 32llu);
    uint32_t lz = 0;

    if (upper_n != 0) {
        lz += clzl(upper_n);
    }

    return lz + clzl((uint32_t) n);
}

static CONST uint64_t
div64(uint64_t numerator, uint32_t denominator)
{
    uint64_t c;
    uint64_t quotient;
    uint64_t long_denom;

    quotient = 0llu;
    long_denom = (uint64_t) denominator;

    if (unlikely(denominator > numerator)) {
        return 0;
    }

    assert(denominator > 0);

    /* align denominator to numerator */
    c = 32u + clzl(denominator) - clz64(numerator);
    long_denom = long_denom << c;

    /* perform binary long division */
    while (c < UINT64_MAX) {
        if (numerator >= long_denom) {
            numerator -= long_denom;
            quotient |= (1llu << c);
        }
        c--;
        long_denom = long_denom >> 1llu;
    }

    return quotient;
}

BOOT_CODE VISIBLE uint32_t
tsc_init(void)
{
    uint32_t version_info = x86_cpuid_eax(0x1, 0x0);
    uint32_t tsc_mhz;

    if (MODEL_ID(version_info) == HASWELL_MODEL_ID ||
            MODEL_ID(version_info) == IVY_BRIDGE_MODEL_ID) {
        uint64_t info;
        uint32_t ratio;

        /* read tsc freq from the platform info msr */
        info = x86_rdmsr(MSR_PLATFORM_INFO);
        ratio = (((uint32_t) info) & 0xFF00) >> 8u;
        tsc_mhz = ratio * 100u; // this gives Mhz
    } else {
        /* use the pit to find out the tsc freq */
        time_t old_ticks, new_ticks, diff;
        uint32_t cycles_per_ms;

        pit_init();

        /* wait for pit to wraparound */
        pit_wait_wraparound();

        /* read tsc */
        old_ticks = x86_rdtsc();

        /* measure how many tsc cycles pass while PIT wrapsaround */
        pit_wait_wraparound();

        new_ticks = x86_rdtsc();

        diff = new_ticks - old_ticks;

        /* sanity checks */
        assert((uint32_t) diff == diff);
        assert(new_ticks > old_ticks);

        /* bravo, khz */
        cycles_per_ms = (uint32_t) diff / PIT_WRAPAROUND_MS;

        /* finally, return mhz */
        tsc_mhz = cycles_per_ms / 1000u;
    }

    return tsc_mhz;
}

PURE time_t
getMaxTimerUs(void)
{
    return div64(UINT64_MAX, x86KStscMhz);
}

CONST time_t
getKernelWcetUs(void)
{
    return  10u;
}

PURE ticks_t
getTimerPrecision(void)
{
    return x86KStscMhz;
}

PURE ticks_t
usToTicks(time_t us)
{
    assert(x86KStscMhz > 0);
    assert(us >= getKernelWcetUs() && us <= getMaxTimerUs());
    return us * x86KStscMhz;
}

PURE time_t
ticksToUs(ticks_t ticks)
{
    return div64(ticks, x86KStscMhz);
}

void
ackDeadlineIRQ(void)
{
}

ticks_t
getCurrentTime(void)
{
    return x86_rdtsc();
}

void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurrentTime);
    x86_wrmsr(IA32_TSC_DEADLINE_MSR, deadline);
}


