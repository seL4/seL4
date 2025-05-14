/*
 * Copyright (c) 2025, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 * SPDX-License-Identifier: BSD-3-Clause
 */
#pragma once
#include <arch/cheri/cheri.h>

void *__capability cheri_sel4_build_cap(void *__capability src, word_t base, word_t address, word_t size,
                                        word_t perms, word_t flags, int sentry, int user);
