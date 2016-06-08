/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_H
#define __ARCH_MACHINE_H

#include <machine.h>
#include <mode/machine.h>
#include <plat/machine/hardware.h>
#include <arch/types.h>
#include <util.h>

#ifndef __ASSEMBLER__

int get_num_avail_p_regs(void);
p_region_t get_avail_p_reg(word_t i);
int get_num_dev_p_regs(void);
p_region_t get_dev_p_reg(word_t i);
void map_kernel_devices(void);

/** MODIFIES: [*] */
void initL2Cache(void);

void initIRQController(void);

/** MODIFIES: [*] */
static inline void plat_cleanL2Range(paddr_t start, paddr_t end);
/** MODIFIES: [*] */
static inline void plat_invalidateL2Range(paddr_t start, paddr_t end);
/** MODIFIES: [*] */
static inline void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);

/** MODIFIES: [*] */
void cleanInvalidateCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void cleanCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void cleanCacheRange_PoU(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void invalidateCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void invalidateCacheRange_I(word_t start, word_t end, paddr_t pstart);
/** MODIFIES: [*] */
void branchFlushRange(word_t start, word_t end, paddr_t pstart);

/** MODIFIES: [*] */
void clean_D_PoU(void);
/** MODIFIES: [*] */
void cleanInvalidate_D_PoC(void);
/** MODIFIES: [*] */
void cleanCaches_PoU(void);
/** MODIFIES: [*] */
void cleanInvalidateL1Caches(void);

/* Cleaning memory before user-level access */
static inline void clearMemory(word_t* ptr, word_t bits)
{
    memzero(ptr, BIT(bits));
    cleanCacheRange_PoU((word_t)ptr, (word_t)ptr + BIT(bits) - 1,
                        addrFromPPtr(ptr));
}

static inline void clearMemoryRAM(word_t* ptr, word_t bits)
{
    memzero(ptr, BIT(bits));
    cleanCacheRange_RAM((word_t)ptr, (word_t)ptr + BIT(bits) - 1,
                        addrFromPPtr(ptr));
}
#endif /* __ASSEMBLER__ */

#endif /* __ARCH_MACHINE_H */
