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

#ifdef CONFIG_IOMMU
#include <plat/machine/intel-vtd.h>
#endif

#define HPET_ADDR 0xFED00000

/* Device discovery. For the pc99 platform we assume a pci bus and the presence of the
 * standard bios regions */
void platAddDevices(void)
{
    /* discover PCI devices and their regions */
    /* pci_scan() calls insert_dev_p_reg() for each device region */
#ifdef CONFIG_IOMMU
    pci_scan(glks.pci_bus_used_bitmap);
#else
    pci_scan(NULL);
#endif
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

/* Enable or disable irq according to the 'mask' flag. */
void maskInterrupt(bool_t mask, irq_t irq)
{
    assert(irq >= irq_controller_min);
    assert(irq <= maxIRQ);

    if (irq <= irq_controller_max) {
#ifdef CONFIG_IRQ_IOAPIC
        ioapic_mask_irq(mask, irq);
#else
        pic_mask_irq(mask, irq);
#endif
    } else {
        /* we can't mask/unmask specific APIC vectors (e.g. MSIs/IPIs) */
    }
}

/* Set mode of an irq */
void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow)
{
#ifdef CONFIG_IRQ_IOAPIC
    assert(irq >= irq_ioapic_min);
    assert(irq <= maxIRQ);

    if (irq <= irq_ioapic_max) {
        ioapic_set_mode(irq, levelTrigger, polarityLow);
    } else {
        /* No mode setting for specific APIC vectors */
    }
#endif
}

/* Handle a platform-reserved IRQ. */
void handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_IOMMU
    if (irq == irq_iommu) {
        vtd_handle_fault();
        return;
    }
#endif
    printf("Received reserved IRQ: %d\n", (int)irq);
}

/* Get the IRQ number currently working on. */
irq_t getActiveIRQ(void)
{
    if (ia32KScurInterrupt == int_invalid) {
        return irqInvalid;
    } else {
        return ia32KScurInterrupt - IRQ_INT_OFFSET;
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
        lz += CLZL(upper_n);
    }

    return lz + CLZL((uint32_t) n);
}

static CONST uint64_t
div64(uint64_t numerator, uint32_t denominator)
{
    uint64_t c;
    uint64_t quotient;
    uint64_t long_denom;

    quotient = 0llu;
    long_denom = (uint64_t) denominator;

    assert(denominator < numerator);
    assert(denominator > 0);

    /* align denominator to numerator */
    c = 32u + CLZL(denominator) - clz64(numerator);
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
    time_t old_ticks, new_ticks, diff;
    uint32_t cycles_per_ms;

    pit_init();

    /* wait for pit to wraparound */
    pit_wait_wraparound();

    /* read tsc */
    old_ticks = ia32_rdtsc();

    /* measure how many tsc cycles pass while PIT wrapsaround */
    pit_wait_wraparound();

    new_ticks = ia32_rdtsc();

    diff = new_ticks - old_ticks;

    /* sanity checks */
    assert((uint32_t) diff == diff);
    assert(new_ticks > old_ticks);

    /* bravo, khz */
    cycles_per_ms = (uint32_t) diff / PIT_WRAPAROUND_MS;

    /* finally, return mhz */
    return cycles_per_ms / 1000u;
}

PURE time_t
getMaxTimerUs(void)
{
    return div64(UINT64_MAX, ia32KStscMhz);
}

CONST time_t
getKernelWcetUs(void)
{
    return  10u;
}

PURE ticks_t
getTimerPrecision(void)
{
    return ia32KStscMhz;
}

PURE ticks_t
usToTicks(time_t us)
{
    assert(ia32KStscMhz > 0);
    assert(us >= getKernelWcetUs() && us <= getMaxTimerUs());
    return us * ia32KStscMhz;
}

void
ackDeadlineIRQ(void)
{
}

ticks_t
getCurrentTime(void)
{
    return ia32_rdtsc();
}

void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurrentTime);
    ia32_wrmsr(IA32_TSC_DEADLINE_MSR, (uint32_t) (deadline >> 32llu), (uint32_t) (deadline));
}


