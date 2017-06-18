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
#include <plat/machine/hardware.h>
#include <arch/types.h>
#include <util.h>

#ifndef __ASSEMBLER__

int get_num_avail_p_regs(void);
p_region_t get_avail_p_reg(word_t i);
int get_num_dev_p_regs(void);
p_region_t get_dev_p_reg(word_t i);
void map_kernel_devices(void);

void initL2Cache(void);

void initIRQController(void);
void cpu_initLocalIRQController(void);

static inline void plat_cleanL2Range(paddr_t start, paddr_t end);
static inline void plat_invalidateL2Range(paddr_t start, paddr_t end);
static inline void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);
static inline void plat_cleanInvalidateCache(void);

void cleanInvalidateCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
void cleanCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
void cleanCacheRange_PoU(word_t start, word_t end, paddr_t pstart);
void invalidateCacheRange_RAM(word_t start, word_t end, paddr_t pstart);
void invalidateCacheRange_I(word_t start, word_t end, paddr_t pstart);
void branchFlushRange(word_t start, word_t end, paddr_t pstart);

void clean_D_PoU(void);
void cleanInvalidate_D_PoC(void);
void cleanCaches_PoU(void);
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

#ifdef ENABLE_SMP_SUPPORT
static inline void arch_pause(void)
{
    /* TODO */
}
#endif /* ENABLE_SMP_SUPPORT */

static inline void Arch_finaliseInterrupt(void)
{
}

#endif /* __ASSEMBLER__ */

#endif /* __ARCH_MACHINE_H */
