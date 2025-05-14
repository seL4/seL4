/*
 * Copyright (c) 2025, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 * SPDX-License-Identifier: BSD-3-Clause
 */
#pragma once
#include <arch/cheri/cheri.h>
#include <api/failures.h>

void *__capability cheri_sel4_build_cap(void *__capability src, word_t base, word_t address, word_t size,
                                        word_t perms, word_t flags, int sentry, int user);

exception_t decodeCheriWriteRegister(cap_t tcb_cap, word_t length, word_t *buffer);
exception_t decodeCheriReadRegister(cap_t tcb_cap, word_t length, bool_t call, word_t *buffer);
exception_t decodeCheriWriteMemoryCap(word_t length, word_t *buffer);
