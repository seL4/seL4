/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/machine.h>

BOOT_CODE bool_t x2apic_is_enabled(void);

#ifdef CONFIG_X2APIC
typedef enum _apic_reg_t {
    APIC_ID             = 0x802,
    APIC_VERSION        = 0x803,
    APIC_TASK_PRIO      = 0x808,
    APIC_PROC_PRIO      = 0x80A,
    APIC_EOI            = 0x80B,
    APIC_LOGICAL_DEST   = 0x80D,
    APIC_SVR            = 0x80F,
    APIC_ISR_BASE       = 0x810,
    APIC_TMR_BASE       = 0x818,
    APIC_IRR_BASE       = 0x820,
    APIC_ERR_STATUS     = 0x828,
    APIC_ICR            = 0x830,
    APIC_LVT_TIMER      = 0x832,
    APIC_LVT_THERMAL    = 0x833,
    APIC_LVT_PERF_CNTR  = 0x834,
    APIC_LVT_LINT0      = 0x835,
    APIC_LVT_LINT1      = 0x836,
    APIC_LVT_ERROR      = 0x837,
    APIC_TIMER_COUNT    = 0x838,
    APIC_TIMER_CURRENT  = 0x839,
    APIC_TIMER_DIVIDE   = 0x83E
} apic_reg_t;

#define X2APIC_LDR_CLUSTER_SHIFT   16
#define X2APIC_LDR_ID_MASK         16

static inline uint32_t apic_read_reg(apic_reg_t reg)
{
    return x86_rdmsr_low(reg);
}

static inline void apic_write_reg(apic_reg_t reg, uint32_t val)
{
    x86_wrmsr(reg, val);
}

static inline logical_id_t apic_get_logical_id(void)
{
    return apic_read_reg(APIC_LOGICAL_DEST);
}

static inline word_t apic_get_cluster(logical_id_t logical_id)
{
    return logical_id >> X2APIC_LDR_CLUSTER_SHIFT;
}

static inline void apic_write_icr(word_t high, word_t low)
{
    uint64_t icr = ((uint64_t)high << 32) | low;
    x86_wrmsr(APIC_ICR, icr);
}

#define IPI_ICR_BARRIER  asm volatile("mfence" ::: "memory")
#define IPI_MEM_BARRIER  IPI_ICR_BARRIER
#endif /* CONFIG_X2APIC */
