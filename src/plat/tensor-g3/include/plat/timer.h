/*
 * Copyright 2025, Millpex
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/*
 * Initializes the platform-specific timer. For Tensor G3, this is
 * the ARM Generic Timer.
 */
void initTimer(void);
