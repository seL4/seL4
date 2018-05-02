/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <config.h>
#include <plat_mode/machine/hardware.h>

#if __riscv_xlen == 32
/* Contain the typical location of memory */
#define PADDR_BASE 0x80000000lu
#else
/* The main kernel window will start at the 0 physical address so that it can contain
 * any potential memory that may exist */
#define PADDR_BASE 0x0lu
#endif

/* This represents the physical address that the kernel image will be linked to. This needs to
 * be on a 1gb boundary as we currently require being able to creating a mapping to this address
 * as the largest frame size */
#define PADDR_LOAD 0xC0000000lu

/* The highest valid physical address that can be indexed in the kernel window */
#define PADDR_TOP (KERNEL_BASE - PPTR_BASE + PADDR_BASE)
/* The highest valid physical address that can be used for the kernel image. We offset by
 * PADDR_LOAD as the window for the kernel image is mapped started at PADDR_LOAD */
#define PADDR_HIGH_TOP (-KERNEL_BASE + PADDR_LOAD)

/* Translates from a physical address and a value in the kernel image */
#define KERNEL_BASE_OFFSET (KERNEL_BASE - PADDR_LOAD)

/* Convert our values into general values expected by the common code */
#define kernelBase KERNEL_BASE
#define PPTR_TOP KERNEL_BASE
#define PPTR_USER_TOP PPTR_BASE
#define BASE_OFFSET (PPTR_BASE - PADDR_BASE)

#ifndef __ASSEMBLER__

int get_num_avail_p_regs(void);
p_region_t get_avail_p_reg(unsigned int i);
bool_t add_avail_p_reg(p_region_t reg);
void map_kernel_devices(void);

bool_t CONST isReservedIRQ(irq_t irq);
void handleReservedIRQ(irq_t irq);
void ackInterrupt(irq_t irq);
bool_t isIRQPending(void);
/** MODIFIES: [*] */
void maskInterrupt(bool_t enable, irq_t irq);
/** MODIFIES: */
irq_t getActiveIRQ(void);
/** MODIFIES: [*] */
static inline void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow) { }
/** MODIFIES: [*] */
void initTimer(void);
/* L2 cache control */
/** MODIFIES: [*] */
void initL2Cache(void);

void initIRQController(void);

void handleSpuriousIRQ(void);

/** MODIFIES: [*] */
void plat_cleanL2Range(paddr_t start, paddr_t end);
/** MODIFIES: [*] */
void plat_invalidateL2Range(paddr_t start, paddr_t end);
/** MODIFIES: [*] */
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);

static inline void* CONST
paddr_to_kpptr(paddr_t paddr)
{
    assert(paddr < PADDR_HIGH_TOP);
    assert(paddr >= PADDR_LOAD);
    return (void*)(paddr + KERNEL_BASE_OFFSET);
}

static inline paddr_t CONST
kpptr_to_paddr(void *pptr)
{
    assert((word_t)pptr >= KERNEL_BASE);
    return (paddr_t)pptr - KERNEL_BASE_OFFSET;
}

#endif /* !__ASSEMBLER__ */

#endif
