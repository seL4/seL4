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

#define physBase          0x00000000C0000000llu
#define PADDR_BASE        physBase /* for compatibility with proofs */

#if CONFIG_PT_LEVELS == 2
#define kernelBase        0xffffffff80000000llu
#elif CONFIG_PT_LEVELS == 3
#define kernelBase        0xFFFFFFC000000000llu
#elif CONFIG_PT_LEVELS == 4
#define kernelBase        0xFFFF800000000000llu
#endif /* COFIG_PT_LEVELS */

#define physMappingOffset (kernelBase - physBase)
#define BASE_OFFSET       physMappingOffset
#ifdef CONFIG_ROCKET_CHIP
#define PPTR_TOP         0xFFFFFFFF8FE00000llu
#else
#define PPTR_TOP          0x4FC00000
#endif
#define PADDR_TOP         (PPTR_TOP - BASE_OFFSET)

#ifndef __ASSEMBLER__

int get_num_avail_p_regs(void);
p_region_t get_avail_p_reg(unsigned int i);
int get_num_dev_p_regs(void);
p_region_t get_dev_p_reg(unsigned int i);
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

#endif /* !__ASSEMBLER__ */

#endif
