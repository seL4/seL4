/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/* ms after which a wraparound occurs (max. 54) */
#define PIT_WRAPAROUND_MS 50

void pit_init(void);
void pit_wait_wraparound(void);

