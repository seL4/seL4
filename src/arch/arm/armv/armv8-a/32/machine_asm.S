/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>

#if defined(CONFIG_ARM_CORTEX_A53)

/* A53 hardware does not support TLB locking */
BEGIN_FUNC(lockTLBEntry)
    bx lr
END_FUNC(lockTLBEntry)

#endif

