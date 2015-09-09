/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <plat/machine/hardware_gen.h>

/* WARNING: some of these constants are also defined in linker.lds */
#define PADDR_BASE  0x00000000
#define PADDR_LOAD  0x00100000
#define PPTR_BASE   0xe0000000
#define kernelBase  PPTR_BASE
#ifdef CONFIG_PAE_PAGING
#define PPTR_USER_TOP (PPTR_BASE & (~MASK(IA32_1G_bits)))
#else
#define PPTR_USER_TOP (PPTR_BASE & (~MASK(IA32_4M_bits)))
#endif
#if CONFIG_MAX_NUM_TRACE_POINTS > 0
#define PPTR_TOP    (-BIT(LARGE_PAGE_BITS + 1))
#define PPTR_NDKS   (PPTR_TOP + 0x1000 + BIT(LARGE_PAGE_BITS))
#else
#define PPTR_TOP    (-BIT(LARGE_PAGE_BITS))
#define PPTR_NDKS   (PPTR_TOP + 0x1000)
#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */
#define PPTR_KDEV   0xffff0000
#define BASE_OFFSET (PPTR_BASE - PADDR_BASE)
#define PADDR_TOP   (PPTR_TOP - BASE_OFFSET)

void handleReservedIRQ(irq_t irq);
void maskInterrupt(bool_t mask, irq_t irq);
void ackInterrupt(irq_t irq);
irq_t getActiveIRQ(void);
bool_t isIRQPending(void);
void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow);
void resetTimer(void);

void handleSpuriousIRQ(void);

#endif
