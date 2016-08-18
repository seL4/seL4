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
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>

#define INTCPS_SYSCONFIG_SOFTRESET BIT(1)
#define INTCPS_SYSSTATUS_RESETDONE BIT(0)


#define TISR_OVERFLOW BIT(1u)
#define TISR_MATCH    BIT(0u)
#define TIER_OVERFLOW BIT(1u)
#define TIER_MATCH    BIT(0u)

#define TIOCP_CFG_SOFTRESET BIT(1u)
#define TCLR_AUTORELOAD     BIT(1u)
#define TCLR_COMPAREENABLE  BIT(6u)
#define TCLR_STARTTIMER     BIT(0u)

/* this is configured and detected by uboot */
#define CLK_MHZ 13
/* constants for division by reciprocal multiplication, for calculation
 * see tools/reciprocal.py */
#define CLK_MAGIC 1321528399llu
#define CLK_SHIFT 34u
compile_assert(magic_will_work, CLK_MHZ == 13u)

static volatile struct TIMER_map {
    uint32_t tidr;   /* GPTIMER_TIDR 0x00 */
    uint32_t padding1[3];
    uint32_t cfg;    /* GPTIMER_CFG 0x10 */
    uint32_t tistat; /* GPTIMER_TISTAT 0x14 */
    uint32_t tisr;   /* GPTIMER_TISR 0x18 */
    uint32_t tier;   /* GPTIMER_TIER 0x1C */
    uint32_t twer;   /* GPTIMER_TWER 0x20 */
    uint32_t tclr;   /* GPTIMER_TCLR 0x24 */
    uint32_t tcrr;   /* GPTIMER_TCRR 0x28 */
    uint32_t tldr;   /* GPTIMER_TLDR 0x2C */
    uint32_t ttgr;   /* GPTIMER_TTGR 0x30 */
    uint32_t twps;   /* GPTIMER_TWPS 0x34 */
    uint32_t tmar;   /* GPTIMER_TMAR 0x38 */
    uint32_t tcar1;  /* GPTIMER_TCAR1 0x3C */
    uint32_t tsicr;  /* GPTIMER_TSICR 0x40 */
    uint32_t tcar2;  /* GPTIMER_TCAR2 0x44 */
    uint32_t tpir;   /* GPTIMER_TPIR 0x48 */
    uint32_t tnir;   /* GPTIMER_TNIR 0x4C */
    uint32_t tcvr;   /* GPTIMER_TCVR 0x50 */
    uint32_t tocr;   /* GPTIMER_TOCR 0x54 */
    uint32_t towr;   /* GPTIMER_TOWR 0x58 */
} *timer = (volatile void*)GPTIMER9_PPTR;

static uint32_t high_bits = 0u;

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    /* Configure gptimer9 as kernel timer */

    /* disable */
    timer->tclr = 0;

    /* perform a soft reset */
    timer->cfg = TIOCP_CFG_SOFTRESET;

    /* wait for reset */
    while (!timer->tistat);

    maskInterrupt(/*disable*/ true, GPT9_IRQ);

    /* Set the reload value */
    timer->tldr = 0u;

    /* Enables interrupt on overflow and match */
    timer->tier |= (TIER_OVERFLOW | TIER_MATCH);

    /* Clear the read register */
    timer->tcrr = 0u;

    /* start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER | TCLR_COMPAREENABLE;
}

void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurrentTime);
    timer->tmar = (uint32_t) deadline;
}

ticks_t
getCurrentTime(void)
{
    bool_t overflow = !!(timer->tisr & TISR_OVERFLOW);
    return (((uint64_t) high_bits + overflow) << 32llu) + timer->tcrr;
}

void
ackDeadlineIRQ(void)
{
    /* check if this is an overflow irq */
    if (timer->tisr & TISR_OVERFLOW) {
        high_bits++;
    }

    /* ack everything */
    timer->tisr = TISR_OVERFLOW | TISR_MATCH;
    assert((timer->tisr & TISR_OVERFLOW) == 0);
    ackInterrupt(GPT9_IRQ);
}

time_t
getMaxTimerUs(void)
{
    return UINT64_MAX / CLK_MAGIC;
}

ticks_t
getTimerPrecision(void)
{
    return 2 * CLK_MHZ;
}

time_t
getKernelWcetUs(void)
{
    return 10u;
}

ticks_t
usToTicks(time_t us)
{
    return (us * CLK_MHZ);
}

time_t
ticksToUs(ticks_t ticks)
{
    /* emulate ticks / CLK_MHZ with multiplication by reciprocal */
    return (ticks * CLK_MAGIC) >> CLK_SHIFT;
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initIRQController(void)
{
    intc->intcps_sysconfig = INTCPS_SYSCONFIG_SOFTRESET;
    while (!(intc->intcps_sysstatus & INTCPS_SYSSTATUS_RESETDONE)) ;
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

