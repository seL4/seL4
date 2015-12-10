/*
 * Copyright 2015, DornerWorks, Ltd.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#define physBase          0x48000000
#define kernelBase        0xA0000000
#define physMappingOffset (kernelBase - physBase)
#define BASE_OFFSET       physMappingOffset
#define PPTR_TOP          0xfff00000
#define PADDR_TOP         (PPTR_TOP - BASE_OFFSET)


#ifndef __ASSEMBLER__

#include <types.h>
#include <arch/object/structures.h>
#include <plat/machine.h>
#include <plat/machine/hardware_gen.h>

#define PADDR_BASE        physBase /* for compatibility with proofs */

static inline void* CONST
ptrFromPAddr(paddr_t paddr)
{
    return (void*)(paddr + physMappingOffset);
}

static inline paddr_t CONST
addrFromPPtr(void* pptr)
{
    return (paddr_t)pptr - physMappingOffset;
}

int get_num_avail_p_regs(void);
p_region_t get_avail_p_reg(word_t i);
int get_num_dev_p_regs(void);
p_region_t get_dev_p_reg(word_t i);
void map_kernel_devices(void);

bool_t CONST isReservedIRQ(irq_t irq);
void handleReservedIRQ(irq_t irq);

/** MODIFIES: [*] */
void resetTimer(void);
/** MODIFIES: [*] */
void initTimer(void);

/* L2 cache control */
/** MODIFIES: [*] */
void initL2Cache(void);

/** MODIFIES: [*] */
static inline void plat_cleanL2Range(paddr_t start, paddr_t end) {}
/** MODIFIES: [*] */
static inline void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
/** MODIFIES: [*] */
static inline void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

#endif /* !__ASSEMBLER__ */

#endif
