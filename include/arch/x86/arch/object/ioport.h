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

exception_t decodeIA32PortInvocation(word_t label, unsigned int length, cptr_t cptr, cte_t *slot, cap_t cap, extra_caps_t extraCaps, word_t* buffer);

#endif
