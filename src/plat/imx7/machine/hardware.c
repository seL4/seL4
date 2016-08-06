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

/* co-processor code to read and write GPT */

static void
write_cntp_ctl(uint32_t v)
{
    asm volatile ("mcr p15, 0, %0, c14, c2, 1" ::"r"(v));
}

static void
write_cntp_tval(uint32_t v)
{
    asm volatile  ("mcr p15, 0, %0, c14, c2, 0" :: "r"(v));
}

static uint32_t
read_cntfrq(void)
{
    uint32_t val;
    asm volatile ("mrc  p15, 0, %0, c14, c0, 0" : "=r"(val));
    return val;
}

/* default 8 MHz */
#define GPT_DEFAULT_FREQ        0x7a1200
#define GPT_DEFAULT_FREQ_MHZ    8ull
static uint32_t gpt_cntp_tval = 0;

/* we use the count-down timer of the GPT as the kernel preemption timer */
void
initTimer(void)
{
    uint32_t freq = read_cntfrq();
    uint64_t tval = 0;

    if (freq != GPT_DEFAULT_FREQ) {
        printf("Default timer has a different frequency %x\n", freq);
    }

    tval = (uint64_t)CONFIG_TIMER_TICK_MS * (freq / 1000);
    if (tval > 0xffffffff) {
        printf("timer interval value out of range \n");
        halt();
    }

    gpt_cntp_tval = (uint32_t)tval;

    /* write the value */
    write_cntp_tval(gpt_cntp_tval);

    /* enable the timer */
    write_cntp_ctl(0x1);
}

/* need to reload the count-down value */
void
resetTimer(void)
{
    write_cntp_tval(gpt_cntp_tval);
}


/* Cortex-A7 uses an integrated L2 cache controller */

void
initL2Cache(void)
{
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

