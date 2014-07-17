/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>

/* pointer to end of kernel image */
/* need a fake array to get the pointer from the linker script */
extern char ki_end[1];

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 128 MiB of memory minus kernel image at its beginning */
    { /* .start = */ (pptr_t)ki_end - physMappingOffset, /* .end = */ 0x88000000 }
};

BOOT_CODE int get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_avail_p_reg(unsigned int i)
{
    return avail_p_regs[i];
}

const p_region_t BOOT_RODATA dev_p_regs[] = {
    /* SoC devices: */
#ifndef DEBUG
    { /* .start = */ UART0_PADDR, /* .end = */ UART0_PADDR + (1 << PAGE_BITS) },
#endif
    { /* .start = */ DMTIMER2_PADDR, /* .end = */ DMTIMER2_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER3_PADDR, /* .end = */ DMTIMER3_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER4_PADDR, /* .end = */ DMTIMER4_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER5_PADDR, /* .end = */ DMTIMER5_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER6_PADDR, /* .end = */ DMTIMER6_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER7_PADDR, /* .end = */ DMTIMER7_PADDR + (1 << PAGE_BITS) },
    /* Board devices. */
    /* TODO: This should ultimately be replaced with a more general solution. */
};

BOOT_CODE int get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_dev_p_reg(unsigned int i)
{
    return dev_p_regs[i];
}


BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: DM Timer 0 */
    map_kernel_frame(
        DMTIMER0_PADDR,
        DMTIMER0_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: INTC */
    map_kernel_frame(
        INTC_PADDR,
        INTC_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#ifdef DEBUG
    /* map kernel device: UART */
    map_kernel_frame(
        UART0_PADDR,
        UART0_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif
}

/*
 * The struct below is used to discourage the compiler from generating literals
 * for every single address we might access.
 */
volatile struct INTC_map {
    uint32_t padding[4];
    uint32_t intcps_sysconfig;
    uint32_t intcps_sysstatus;
    uint32_t padding2[10];
    uint32_t intcps_sir_irq;
    uint32_t intcps_sir_fiq;
    uint32_t intcps_control;
    uint32_t intcps_protection;
    uint32_t intcps_idle;
    uint32_t padding3[3];
    uint32_t intcps_irq_priority;
    uint32_t intcps_fiq_priority;
    uint32_t intcps_threshold;
    uint32_t padding4[5];
    struct {
        uint32_t intcps_itr;
        uint32_t intcps_mir;
        uint32_t intcps_mir_clear;
        uint32_t intcps_mir_set;
        uint32_t intcps_isr_set;
        uint32_t intcps_isr_clear;
        uint32_t intcps_pending_irq;
        uint32_t intcps_pending_fiq;
    } intcps_n[3];
    uint32_t padding5[8];
    uint32_t intcps_ilr[96];
} *intc = (volatile void*)INTC_PPTR;

/**
   DONT_TRANSLATE
 */

interrupt_t
getActiveIRQ(void)
{
    interrupt_t irq = intc->intcps_sir_irq;
    /* Ignore spurious interrupts. */
    if ((irq & ~0b1111111) == 0) {
        assert(irq <= maxIRQ);
        if (intc->intcps_n[irq / 32].intcps_pending_irq & (1 << (irq & 31))) {
            return irq;
        }
    }

    /* No interrupt. */
    return 0xff;
}

/* Check for pending IRQ */
bool_t isIRQPending(void)
{
    return getActiveIRQ() != 0xff;
}

/* Enable or disable irq according to the 'disable' flag. */
/**
   DONT_TRANSLATE
*/
void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        intc->intcps_n[irq / 32].intcps_mir_set = 1 << (irq & 31);
    } else {
        intc->intcps_n[irq / 32].intcps_mir_clear = 1 << (irq & 31);
    }
}

/* Determine if the given IRQ should be reserved by the kernel. */
bool_t
isReservedIRQ(interrupt_t irq)
{
    return false;
}

/* Handle a platform-reserved IRQ. */
void handleReservedIRQ(irq_t irq)
{
    /* We shouldn't be receiving any reserved IRQs anyway. */
    maskInterrupt(true, irq);

    return;
}

void
ackInterrupt(irq_t irq)
{
    intc->intcps_control = 1;
    /* Ensure the ack has hit the interrupt controller before potentially
     * re-enabling interrupts. */
    dsb();
}

#define TIMER_INTERVAL_MS (CONFIG_TIMER_TICK_MS)

#define TIOCP_CFG_SOFTRESET BIT(0)

#define TIER_MATCHENABLE BIT(0)
#define TIER_OVERFLOWENABLE BIT(1)
#define TIER_COMPAREENABLE BIT(2)

#define TCLR_AUTORELOAD BIT(1)
#define TCLR_COMPAREENABLE BIT(6)
#define TCLR_STARTTIMER BIT(0)

#define TISR_OVF_FLAG (BIT(0) | BIT(1) | BIT(2))

#define TICKS_PER_SECOND 32768 // 32KHz
#define TIMER_INTERVAL_TICKS ((int)(1UL * TIMER_INTERVAL_MS * TICKS_PER_SECOND / 1000))

volatile struct TIMER_map {
    uint32_t tidr; // 00h TIDR Identification Register
    uint32_t padding1[3];
    uint32_t cfg; // 10h TIOCP_CFG Timer OCP Configuration Register
    uint32_t padding2[3];
    uint32_t tieoi; // 20h IRQ_EOI Timer IRQ End-Of-Interrupt Register
    uint32_t tisrr; // 24h IRQSTATUS_RAW Timer IRQSTATUS Raw Register
    uint32_t tisr; // 28h IRQSTATUS Timer IRQSTATUS Register
    uint32_t tier; // 2Ch IRQSTATUS_SET Timer IRQENABLE Set Register
    uint32_t ticr; // 30h IRQSTATUS_CLR Timer IRQENABLE Clear Register
    uint32_t twer; // 34h IRQWAKEEN Timer IRQ Wakeup Enable Register
    uint32_t tclr; // 38h TCLR Timer Control Register
    uint32_t tcrr; // 3Ch TCRR Timer Counter Register
    uint32_t tldr; // 40h TLDR Timer Load Register
    uint32_t ttgr; // 44h TTGR Timer Trigger Register
    uint32_t twps; // 48h TWPS Timer Write Posted Status Register
    uint32_t tmar; // 4Ch TMAR Timer Match Register
    uint32_t tcar1; // 50h TCAR1 Timer Capture Register
    uint32_t tsicr; // 54h TSICR Timer Synchronous Interface Control Register
    uint32_t tcar2; // 58h TCAR2 Timer Capture Register
} *timer = (volatile void*)DMTIMER0_PPTR;

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    timer->tisr = TISR_OVF_FLAG;
    ackInterrupt(DMTIMER0_IRQ);
}

/* Configure dmtimer0 as kernel preemption timer */
/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    int timeout;

    timer->cfg = TIOCP_CFG_SOFTRESET;

    for (timeout = 10000; (timer->cfg & TIOCP_CFG_SOFTRESET) && timeout > 0; timeout--)
        ;
    if (!timeout) {
        return;
    }

    maskInterrupt(/*disable*/ true, DMTIMER0_IRQ);

    /* Set the reload value */
    timer->tldr = 0xFFFFFFFFUL - TIMER_INTERVAL_TICKS;

    /* Enables interrupt on overflow */
    timer->tier = TIER_OVERFLOWENABLE;

    /* Clear the read register */
    timer->tcrr = 0xFFFFFFFFUL - TIMER_INTERVAL_TICKS;

    /* Set autoreload and start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER;
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initIRQController(void)
{
    /* Do nothing */
}

void
handleSpuriousIRQ(void)
{
    /* Do nothing */
}

