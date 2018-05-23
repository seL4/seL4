/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_IOPORT_H
#define __ARCH_OBJECT_IOPORT_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#define NUM_IO_PORTS BIT(16)

/* given a pointer to an 8K IO port bitmap, set a range of bits to 0 or 1 based on the `set` parameter */
void setIOPortMask(void *ioport_bitmap, uint16_t low, uint16_t high, bool_t set);

exception_t decodeX86PortInvocation(word_t invLabel, word_t length, cptr_t cptr, cte_t *slot, cap_t cap, extra_caps_t excaps, bool_t call, word_t* buffer);
exception_t decodeX86PortControlInvocation(word_t invLabel, word_t length, cptr_t cptr, cte_t *slot, cap_t cap, extra_caps_t excaps, word_t *buffer);

/* used to clean up the final capability to an allocated I/O port range */
void freeIOPortRange(uint16_t first_port, uint16_t last_port);

#endif
