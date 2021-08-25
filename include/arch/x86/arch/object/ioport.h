/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#define NUM_IO_PORTS BIT(16)

/* given a pointer to an 8K IO port bitmap, set a range of bits to 0 or 1 based on the `set` parameter */
void setIOPortMask(void *ioport_bitmap, uint16_t low, uint16_t high, bool_t set);

exception_t decodeX86PortInvocation(word_t invLabel, word_t length, cptr_t cptr, cte_t *slot, cap_t cap,
                                    bool_t call, word_t *buffer);
exception_t decodeX86PortControlInvocation(word_t invLabel, word_t length, cptr_t cptr, cte_t *slot, cap_t cap,
                                           word_t *buffer);

/* used to clean up the final capability to an allocated I/O port range */
void freeIOPortRange(uint16_t first_port, uint16_t last_port);

