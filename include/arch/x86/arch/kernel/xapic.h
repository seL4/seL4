/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <plat_mode/machine/hardware.h>

#ifdef CONFIG_XAPIC
typedef enum _apic_reg_t {
    APIC_ID             = 0x020,
    APIC_VERSION        = 0x030,
    APIC_TASK_PRIO      = 0x080,
    APIC_ARBITR_PRIO    = 0x090,
    APIC_PROC_PRIO      = 0x0A0,
    APIC_EOI            = 0x0B0,
    APIC_LOGICAL_DEST   = 0x0D0,
    APIC_DEST_FORMAT    = 0x0E0,
    APIC_SVR            = 0x0F0,
    APIC_ISR_BASE       = 0x100,
    APIC_TMR_BASE       = 0x180,
    APIC_IRR_BASE       = 0x200,
    APIC_ERR_STATUS     = 0x280,
    APIC_ICR1           = 0x300,
    APIC_ICR2           = 0x310,
    APIC_LVT_TIMER      = 0x320,
    APIC_LVT_THERMAL    = 0x330,
    APIC_LVT_PERF_CNTR  = 0x340,
    APIC_LVT_LINT0      = 0x350,
    APIC_LVT_LINT1      = 0x360,
    APIC_LVT_ERROR      = 0x370,
    APIC_TIMER_COUNT    = 0x380,
    APIC_TIMER_CURRENT  = 0x390,
    APIC_TIMER_DIVIDE   = 0x3E0
} apic_reg_t;

#define XAPIC_LDR_SHIFT             24
#define XAPIC_DFR_FLAT              0xFFFFFFFF

static inline uint32_t apic_read_reg(apic_reg_t reg)
{
    return *(volatile uint32_t *)(PPTR_APIC + reg);
}

static inline void apic_write_reg(apic_reg_t reg, uint32_t val)
{
    *(volatile uint32_t *)(PPTR_APIC + reg) = val;
}

static inline logical_id_t apic_get_logical_id(void)
{
    return apic_read_reg(APIC_LOGICAL_DEST) >> XAPIC_LDR_SHIFT;
}

static inline word_t apic_get_cluster(logical_id_t logical_id)
{
    return 0; /* always return 0 as 'init_xapic_ldr' uses flat cluster */
}

static inline void apic_write_icr(word_t high, word_t low)
{
    apic_write_reg(APIC_ICR2, high);
    apic_write_reg(APIC_ICR1, low);
}

#define IPI_ICR_BARRIER  asm volatile("" ::: "memory")
#define IPI_MEM_BARRIER IPI_ICR_BARRIER
#endif  /* CONFIG_XAPIC */

