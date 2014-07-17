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

#define L2_LINE_SIZE_BITS 5
#define L2_LINE_SIZE BIT(L2_LINE_SIZE_BITS)

#define L2_LINE_START(a) ROUND_DOWN(a, L2_LINE_SIZE_BITS)
#define L2_LINE_INDEX(a) (L2_LINE_START(a)>>L2_LINE_SIZE_BITS)

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 128 MiB of memory minus kernel image at its beginning */
    { .start = 0x80000000, .end = 0x88000000 }
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
    { .start = 0x43f80000, .end = 0x43f81000 }, /* IMX31 I2C 1 */
    { .start = 0x43f84000, .end = 0x43f85000 }, /* IMX31 I2C 3 */
    { .start = 0x43f88000, .end = 0x43f89000 }, /* IMX31 USBOTG */
    { .start = 0x43f8c000, .end = 0x43f8d000 }, /* IMX31 ATA control */
    { .start = UART_PADDR, .end = UART_PADDR + BIT(PAGE_BITS) }, /* IMX31 UART 1 */
    { .start = 0x43f94000, .end = 0x43f95000 }, /* IMX31 UART 2 */
    { .start = 0x43f98000, .end = 0x43f99000 }, /* IMX31 I2C 2 */
    { .start = 0x43f9c000, .end = 0x43f9d000 }, /* IMX31 1-WIRE */
    { .start = 0x43fa0000, .end = 0x43fa1000 }, /* IMX31 SSI 1 */
    { .start = 0x43fa4000, .end = 0x43fa5000 }, /* IMX31 CSPI 1 */
    { .start = 0x43fa8000, .end = 0x43fa9000 }, /* IMX31 KPP */
    { .start = 0x43fac000, .end = 0x43fad000 }, /* IMX31 IOMUXC */
    { .start = 0x43fb0000, .end = 0x43fb1000 }, /* IMX31 UART 4 */
    { .start = 0x43fb4000, .end = 0x43fb5000 }, /* IMX31 UART 5 */

    /* The devices from here up to the SPBA are all accessed through the
     * SPBA (bus arbiter), which must be configured to attach them to either
     * the CPU or the DMA controller. The reset state is to disable everything
     * (except the SPBA itself). This may need to be handled by the kernel, if
     * the kernel is managing the SDMA controller.
     */
    { .start = 0x50004000, .end = 0x50005000 }, /* IMX31 SDHC 1 */
    { .start = 0x50008000, .end = 0x50009000 }, /* IMX31 SDHC 2 */
    { .start = 0x5000c000, .end = 0x5000d000 }, /* IMX31 UART 3 */
    { .start = 0x50010000, .end = 0x50011000 }, /* IMX31 CSPI 2 */
    { .start = 0x50014000, .end = 0x50015000 }, /* IMX31 SSI 2 */
    { .start = 0x50018000, .end = 0x50019000 }, /* IMX31 SIM */
    { .start = 0x5001c000, .end = 0x5001d000 }, /* IMX31 IIM */
    { .start = 0x50020000, .end = 0x50021000 }, /* IMX31 ATA (DMA) */
    { .start = 0x50024000, .end = 0x50025000 }, /* IMX31 MSHC 1 */
    { .start = 0x50028000, .end = 0x50029000 }, /* IMX31 MSHC 2 */
    { .start = 0x5003c000, .end = 0x5003d000 }, /* IMX31 SPBA */

    { .start = 0x53f80000, .end = 0x53f81000 }, /* IMX31 CCM */
    { .start = 0x53f84000, .end = 0x53f85000 }, /* IMX31 CSPI 3 */
    { .start = 0x53f8c000, .end = 0x53f8d000 }, /* IMX31 FIR */
    { .start = 0x53f90000, .end = 0x53f91000 }, /* IMX31 GPT */
    /* EPIT 1 is used by the kernel */
    { .start = 0x53f98000, .end = 0x53f99000 }, /* IMX31 EPIT 2 */
    { .start = 0x53fa4000, .end = 0x53fa8000 }, /* IMX31 GPIO 3 */
    /* This is disabled until we know what it does */
    //{ .start = 0x53fac000, .end = 0x53fad000 }, /* IMX31 SCC */
    { .start = 0x53fb0000, .end = 0x53fb1000 }, /* IMX31 RNGA */
    /* Disabled, because it has an internal DMA controller */
    //{ .start = 0x53fc0000, .end = 0x53fc1000 }, /* IMX31 IPU */
    { .start = 0x53fc4000, .end = 0x53fc5000 }, /* IMX31 AUDMUX */
    /* Disabled, because it has an internal DMA controller */
    //{ .start = 0x53fc8000, .end = 0x53fc9000 }, /* IMX31 MPEG4enc */
    { .start = 0x53fcc000, .end = 0x53fd0000 }, /* IMX31 GPIO 1 */
    { .start = 0x53fd0000, .end = 0x53fd4000 }, /* IMX31 GPIO 2 */
    /* This can access any physical address; the kernel should control it */
    //{ .start = 0x53fd4000, .end = 0x53fd5000 }, /* IMX31 SDMA */
    { .start = 0x53fd8000, .end = 0x53fd9000 }, /* IMX31 RTC */
    /* This can reset the machine if not regularly serviced; it should
     * be disabled or controlled by the kernel
     */
    //{ .start = 0x53fdc000, .end = 0x53fde000 }, /* IMX31 WDOG */
    { .start = 0x53fe0000, .end = 0x53fe1000 }, /* IMX31 PWM */
    { .start = 0x53fec000, .end = 0x53fed000 }, /* IMX31 RTIC */
    { .start = 0xa0000000, .end = 0xa4000000 }, /* IMX31 CS0 (flash) */
    { .start = 0xa8000000, .end = 0xaa000000 }, /* IMX31 CS1 (flash) */
    { .start = 0xb4000000, .end = 0xb6000000 }, /* IMX31 CS4 (FPGA) */
    { .start = 0xb6000000, .end = 0xb8000000 }, /* IMX31 CS5 (enet) */
    { .start = 0xc0000000, .end = 0xc2000000 }  /* IMX31 PCMCIA/CF */
};

BOOT_CODE int get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_dev_p_reg(unsigned int i)
{
    return dev_p_regs[i];
}

/* Memory map for AVIC (Advanced Vectored Interrupt Controller). */
volatile struct avic_map {
    uint32_t intctl;
    uint32_t nimask;
    uint32_t intennum;
    uint32_t intdisnum;
    uint32_t intenableh;
    uint32_t intenablel;
    uint32_t inttypeh;
    uint32_t inttypel;
    uint32_t nipriority[8];
    uint32_t nivecsr;
    uint32_t fivecsr;
    uint32_t intsrch;
    uint32_t intsrcl;
    uint32_t intfrch;
    uint32_t intfrcl;
    uint32_t nipndh;
    uint32_t nipndl;
    uint32_t fipndh;
    uint32_t fipndl;
    uint32_t vector[64];
} *avic = (volatile void *)AVIC_PPTR;

/* Get the active IRQ number from the AVIC.  Returns 0xff if
 * there isn't one. Note this is also known as irqInvalid */
/**
   DONT_TRANSLATE
 */
interrupt_t
getActiveIRQ(void)
{
    return (avic->nivecsr >> 16) & 0xff;
}

/* Check for pending IRQ */
bool_t isIRQPending(void)
{
    return getActiveIRQ() != irqInvalid;
}

/* Enable or disable irq according to the 'disable' flag. */
/**
   DONT_TRANSLATE
*/
void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        avic->intdisnum = irq;
    } else {
        avic->intennum = irq;
    }
}

/* Determine if the given IRQ should be reserved by the kernel. */
bool_t CONST
isReservedIRQ(irq_t irq)
{
    return false;
}

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    printf("Received reserved IRQ: %d\n", (int)irq);
}

void
ackInterrupt(irq_t irq)
{
    /* empty on this platform */
}

/* Memory map for EPIT (Enhanced Periodic Interrupt Timer). */
volatile struct epit_map {
    uint32_t epitcr;
    uint32_t epitsr;
    uint32_t epitlr;
    uint32_t epitcmpr;
    uint32_t epitcnt;
} *epit1 = (volatile void *)EPIT_PPTR;


enum IPGConstants {
    IPG_CLK = 1,
    IPG_CLK_HIGHFREQ = 2,
    IPG_CLK_32K = 3
};

#define TIMER_INTERVAL_MS (CONFIG_TIMER_TICK_MS)
#define TIMER_CLOCK_SRC   IPG_CLK_32K
#define TIMER_CLOCK_HZ    32768
#define TIMER_RELOAD_VAL  (TIMER_CLOCK_HZ * TIMER_INTERVAL_MS / 1000)
#if TIMER_RELOAD_VAL <= 0 || TIMER_RELOAD_VAL > 0xffffffff
#error TIMER_RELOAD_VAL out of range
#endif


BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: EPIT */
    map_kernel_frame(
        EPIT_PADDR,
        EPIT_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: AVIC */
    map_kernel_frame(
        AVIC_PADDR,
        AVIC_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: L2CC */
    map_kernel_frame(
        L2CC_PADDR,
        L2CC_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#ifdef DEBUG
    /* map kernel device: UART */
    map_kernel_frame(
        UART_PADDR,
        UART_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif
}

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    epit1->epitsr = 1;
    /* Timer resets automatically */
}

/* Configure EPIT1 as kernel preemption timer */
/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    epitcr_t epitcr_kludge;

    /* Stop timer */
    epit1->epitcr = 0;

    /* Configure timer */
    epitcr_kludge.words[0] = 0; /* Zero struct */
    epitcr_kludge = epitcr_set_clksrc(epitcr_kludge, TIMER_CLOCK_SRC);
    /* Overwrite counter immediately on write */
    epitcr_kludge = epitcr_set_iovw(epitcr_kludge, 1);
    /* Reload from modulus register */
    epitcr_kludge = epitcr_set_rld(epitcr_kludge, 1);
    /* Enable interrupt */
    epitcr_kludge = epitcr_set_ocien(epitcr_kludge, 1);
    /* Count from modulus value on restart */
    epitcr_kludge = epitcr_set_enmod(epitcr_kludge, 1);
    epit1->epitcr = epitcr_kludge.words[0];

    /* Set counter modulus */
    epit1->epitlr = TIMER_RELOAD_VAL;

    /* Interrupt at zero count */
    epit1->epitcmpr = 0;

    /* Clear pending interrupt */
    epit1->epitsr = 1;

    /* Enable timer */
    epitcr_kludge = epitcr_set_en(epitcr_kludge, 1);
    epit1->epitcr = epitcr_kludge.words[0];
}

static void invalidateL2(void)
{
    /* Invalidate all ways. */
    imx31_l2cc_flush_regs->inv_by_way = 0xff;
    /* Busy-wait for completion. */
    while (imx31_l2cc_flush_regs->inv_by_way);
}

static void finaliseL2Op(void)
{
    /* We sync the l2 cache, which drains the write and eviction
       buffers, to ensure that everything is consistent with RAM. */
    imx31_l2cc_flush_regs->sync = 1;
}

void plat_cleanL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
            index < L2_LINE_INDEX(end) + 1;
            index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->clean_by_pa = line;
    }
    finaliseL2Op();
}

void plat_invalidateL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
            index < L2_LINE_INDEX(end) + 1;
            index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->inv_by_pa = line;
    }

    finaliseL2Op();
}

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
            index < L2_LINE_INDEX(end) + 1;
            index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->clinv_by_pa = line;
    }
    finaliseL2Op();
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initL2Cache(void)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Configure L2 cache */
    imx31_l2cc_ctrl_regs->aux_control = 0x0003001b;

    /* Invalidate the L2 cache */
    invalidateL2();

    /* Enable the L2 cache */
    imx31_l2cc_ctrl_regs->control = 1;
#endif
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

