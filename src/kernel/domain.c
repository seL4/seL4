/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <util.h>
#include <object/structures.h>
#include <model/statedata.h>

/* Domain schedule. The budget length is in ms for non-MCS and ticks for MCS */
dschedule_t ksDomSchedule[CONFIG_NUM_DOMAIN_SCHEDULES] = {
#if CONFIG_WORD_SIZE == 64
    /* Avoid overflow when adding to ksCurTime */
    { .domain = 0, .length = ~0UL / 8 },
#else
    { .domain = 0, .length = ~0 },
#endif
};

