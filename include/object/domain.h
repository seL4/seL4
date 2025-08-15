/*
 * Copyright 2025, Indan Zupancic
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <api/failures.h>

typedef uint64_t dschedule_t;

extern dschedule_t ksDomSchedule[CONFIG_NUM_DOMAIN_SCHEDULES];

/* The domain is stored in the top 8 bits, duration in the bottom 56 bits. */
#define DSCHED_DOMAIN_BITS 8 // Maximum of 255 domains supported
#define DSCHED_DOMAIN_SHIFT (64 - DSCHED_DOMAIN_BITS)
#define DSCHED_MAX_DURATION (UINT64_MAX >> DSCHED_DOMAIN_BITS)
#define DSCHED_DURATION_MASK DSCHED_MAX_DURATION

static dschedule_t dschedule_make(dom_t domain, uint64_t duration)
{
    return ((uint64_t)domain << DSCHED_DOMAIN_SHIFT) | duration;
}

static inline dom_t dschedule_domain(dschedule_t sched)
{
    return sched >> DSCHED_DOMAIN_SHIFT;
}

static inline ticks_t dschedule_duration(dschedule_t sched)
{
    return sched & DSCHED_DURATION_MASK;
}

static inline bool_t dschedule_is_end_marker(word_t index)
{
    return ksDomSchedule[index] == 0;
}

exception_t decodeDomainInvocation(word_t invLabel, word_t length, word_t *buffer);
