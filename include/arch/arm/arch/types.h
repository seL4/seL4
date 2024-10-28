/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
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

typedef word_t paddr_t;
typedef word_t pptr_t;
typedef word_t cptr_t;
typedef word_t node_id_t;
typedef word_t cpu_id_t;
typedef word_t dom_t;

#if defined(CONFIG_HAVE_CHERI)
typedef __uintcap_t rword_t;
typedef __uintcap_t vptr_t;
#else
typedef word_t rword_t;
typedef word_t vptr_t;
#endif

typedef uint8_t  hw_asid_t;

enum hwASIDConstants {
    hwASIDMax = 255,
    hwASIDBits = 8
};

typedef struct kernel_frame {
    paddr_t paddr;
    pptr_t pptr;
    int armExecuteNever;
    int userAvailable;
} kernel_frame_t;
