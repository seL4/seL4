/*
 * Copyright 2025, UNSW
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/assembler.h>

.section .text, "ax"

BEGIN_FUNC(idle_thread)
    1: wfi
    b 1b
END_FUNC(idle_thread)
