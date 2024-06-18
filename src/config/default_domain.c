/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <util.h>
#include <object/structures.h>
#include <model/statedata.h>

/* Default schedule. The length is in ms */
const dschedule_t ksDomSchedule[] = {
    { .domain = 0, .length = 1 },
};

const word_t ksDomScheduleLength = ARRAY_SIZE(ksDomSchedule);

