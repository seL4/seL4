/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/syscall.h>

static inline time_t mode_parseTimeArg(word_t i, word_t *buffer)
{
    return getSyscallArg(i, buffer);
}

static inline word_t mode_setTimeArg(word_t i, time_t time, word_t *buffer, tcb_t *thread)
{
    return setMR(thread, buffer, i, time);
}
