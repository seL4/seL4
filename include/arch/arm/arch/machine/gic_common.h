/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <autoconf.h>
#include <stdint.h>
#include <machine/interrupt.h>

/* Shift positions for GICD_SGIR register */
#define GICD_SGIR_SGIINTID_SHIFT          0
#define GICD_SGIR_CPUTARGETLIST_SHIFT     16
#define GICD_SGIR_TARGETLISTFILTER_SHIFT  24

/* Special IRQ's */
#define SPECIAL_IRQ_START 1020u
#define IRQ_NONE          1023u

/* CPU specific IRQ's */
#define SGI_START         0u
#define PPI_START         16u

/* Shared Peripheral Interrupts */
#define SPI_START         32u

#define NUM_PPI SPI_START
#define HW_IRQ_IS_SGI(irq) ((irq) < PPI_START)
#define HW_IRQ_IS_PPI(irq) ((irq) < NUM_PPI)

#if defined ENABLE_SMP_SUPPORT
/* In this case irq_t is a struct with an hw irq field and target core field.
 * The following macros convert between (target_core, hw_irq) <-> irq_t <-> cnode index.
 * IRQ_IS_PPI returns true if hw_irq < 32 which is a property of the GIC.
 * The layout of IRQs into the CNode are all of PPI's for each core first, followed
 * by the global interrupts.  Examples:
 *   core: 0, irq: 12 -> index 12.
 *   core: 2, irq: 16 -> (2 * 32) + 16
 *   core: 1, irq: 33, (4 total cores) -> (4 * 32) + (33-32).
 */
#define IRQ_IS_PPI(_irq) (HW_IRQ_IS_PPI(_irq.irq))
#define CORE_IRQ_TO_IRQT(tgt, _irq) ((irq_t){.irq = (_irq), .target_core = (tgt)})
#define IRQT_TO_IDX(_irq) (HW_IRQ_IS_PPI(_irq.irq) ? \
                                 (irq.target_core)*NUM_PPI + (_irq.irq) : \
                                 (CONFIG_MAX_NUM_NODES-1)*NUM_PPI + (_irq.irq))

#define IDX_TO_IRQT(idx) (((idx) < NUM_PPI*CONFIG_MAX_NUM_NODES) ? \
                        CORE_IRQ_TO_IRQT((idx) / NUM_PPI, (idx) - ((idx)/NUM_PPI)*NUM_PPI): \
                        CORE_IRQ_TO_IRQT(0, (idx) - (CONFIG_MAX_NUM_NODES-1)*NUM_PPI))
#define IRQT_TO_CORE(irqt) (irqt.target_core)
#define IRQT_TO_IRQ(irqt) (irqt.irq)
irq_t irqInvalid = CORE_IRQ_TO_IRQT(-1, -1);

#else
#define IRQ_IS_PPI(irq) HW_IRQ_IS_PPI(irq)
irq_t irqInvalid = (uint16_t) -1;
#endif

/* Setters/getters helpers for hardware irqs */
#define IRQ_REG(IRQ) ((IRQ) >> 5u)
#define IRQ_BIT(IRQ) ((IRQ) & 0x1f)
#define IS_IRQ_VALID(X) (((X) & IRQ_MASK) < SPECIAL_IRQ_START)

/*
 * The only sane way to get an GIC IRQ number that can be properly
 * ACKED later is through the int_ack register. Unfortunately, reading
 * this register changes the interrupt state to pending so future
 * reads will not return the same value For this reason, we have a
 * global variable to store the IRQ number.
 */
extern word_t active_irq[CONFIG_MAX_NUM_NODES];

static inline void handleSpuriousIRQ(void)
{
}

void initIRQController(void);


