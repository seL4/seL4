/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <machine.h>
#include <plat/machine/hardware.h>
#include <arch/types.h>
#include <util.h>

#ifndef __ASSEMBLER__

void map_kernel_devices(void);

void initL2Cache(void);

void initIRQController(void);
void cpu_initLocalIRQController(void);
void setIRQTrigger(irq_t irq, bool_t trigger);
#ifdef ENABLE_SMP_SUPPORT
void setIRQTarget(irq_t irq, seL4_Word target);
#endif

static inline void plat_cleanL2Range(paddr_t start, paddr_t end);
static inline void plat_invalidateL2Range(paddr_t start, paddr_t end);
static inline void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);
static inline void plat_cleanInvalidateL2Cache(void);

void cleanInvalidateCacheRange_RAM(vptr_t start, vptr_t end, paddr_t pstart);
void cleanCacheRange_RAM(vptr_t start, vptr_t end, paddr_t pstart);
void cleanCacheRange_PoU(vptr_t start, vptr_t end, paddr_t pstart);
void invalidateCacheRange_RAM(vptr_t start, vptr_t end, paddr_t pstart);
void invalidateCacheRange_I(vptr_t start, vptr_t end, paddr_t pstart);
void branchFlushRange(vptr_t start, vptr_t end, paddr_t pstart);

void clean_D_PoU(void);
void cleanInvalidate_D_PoC(void);
void cleanInvalidate_L1D(void);
void cleanCaches_PoU(void);
void cleanInvalidateL1Caches(void);

/* Cleaning memory before user-level access */
static inline void clearMemory(word_t *ptr, word_t bits)
{
    memzero(ptr, BIT(bits));
    cleanCacheRange_RAM((vptr_t)ptr, (vptr_t)ptr + BIT(bits) - 1,
                        addrFromPPtr(ptr));
}

/* Cleaning memory before page table walker access */
static inline void clearMemory_PT(word_t *ptr, word_t bits)
{
    memzero(ptr, BIT(bits));
    cleanCacheRange_PoU((vptr_t)ptr, (vptr_t)ptr + BIT(bits) - 1,
                        addrFromPPtr(ptr));
}

#ifdef ENABLE_SMP_SUPPORT
static inline void arch_pause(void)
{
    /* TODO */
}
#endif /* ENABLE_SMP_SUPPORT */

/* Update the value of the actual regsiter to hold the expected value */
static inline exception_t Arch_setTLSRegister(word_t tls_base)
{
    /* This register is saved and restored on kernel exit and entry so
     * we only update it in the saved context. */
    setRegister(NODE_STATE(ksCurThread), TLS_BASE, tls_base);
    return EXCEPTION_NONE;
}

#endif /* __ASSEMBLER__ */
