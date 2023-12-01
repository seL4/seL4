/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>

#ifdef CONFIG_KERNEL_MCS
bool_t tryRaisingTimeoutFault(tcb_t *tptr, word_t scBadge);
#endif

void handleFault(tcb_t *tptr);

