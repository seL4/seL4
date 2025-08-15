/*
 * Copyright 2025, Indan Zupancic
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <api/failures.h>

/* The domain is stored in the top 8 bits, duration in the bottom 56 bits.
 * Not using 64 bits for duration also avoids overflows. */
typedef uint64_t dschedule_t;

#define DSCHED_END_MARKER 0
#define DOMAIN_0_MAX_DURATION (UINT64_MAX >> 8)

static inline dom_t dschedule_domain(dschedule_t sched)
{
    return sched >> 56;
}

static inline ticks_t dschedule_duration(dschedule_t sched)
{
    return sched & DOMAIN_0_MAX_DURATION;
}

exception_t decodeDomainInvocation(word_t invLabel, word_t length, word_t *buffer);

extern dschedule_t ksDomSchedule[CONFIG_NUM_DOMAIN_SCHEDULES];
