/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <mode/types.h>
#include <stdint.h>

typedef unsigned long word_t;
typedef signed long sword_t;
/* for printf() formatting */
#define _seL4_word_fmt  l

typedef word_t vptr_t;
typedef word_t paddr_t;
typedef word_t pptr_t;
typedef word_t cptr_t;
typedef word_t node_id_t;
typedef word_t cpu_id_t;
typedef word_t dom_t;

/*
 * Hardware ASID/VMID 0 is reserved: setVMRoot() installs the empty global
 * VSpace under hardware ASID/VMID 0 for threads without a valid VSpace,
 * without a TLB flush. That is only sound if no user-managed VSpace runs under
 * ASID/VMID 0, otherwise such a thread hits that VSpace's TLB entries.
 */
enum hwASIDConstants {
    hwASIDReserved = 0,
    hwASIDMin = 1,
    hwASIDMax = 255,
    hwASIDBits = 8
};

typedef struct kernel_frame {
    paddr_t paddr;
    pptr_t pptr;
    int armExecuteNever;
    int userAvailable;
} kernel_frame_t;
