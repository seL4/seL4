/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/syscall.h>
#include <object/structures.h>

static inline time_t mode_parseTimeArg(word_t i, word_t *buffer)
{
    return (((time_t) getSyscallArg(i + 1, buffer) << 32llu) + getSyscallArg(i, buffer));
}

static inline word_t mode_setTimeArg(word_t i, time_t time, word_t *buffer, tcb_t *thread)
{
    setMR(thread, buffer, i, (uint32_t) time);
    return setMR(thread, buffer, i + 1, (uint32_t)(time >> 32llu));
}

