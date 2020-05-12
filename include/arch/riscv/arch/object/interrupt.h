/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

static inline void handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_IRQ_REPORTING
    printf("Received unhandled reserved IRQ: %d\n", (int)irq);
#endif
}

exception_t Arch_decodeIRQControlInvocation(word_t invLabel, word_t length,
                                            cte_t *srcSlot, word_t *buffer);
exception_t Arch_checkIRQ(word_t irq_w);

