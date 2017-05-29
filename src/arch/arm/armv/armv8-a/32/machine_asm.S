/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <machine/assembler.h>

#if defined(CONFIG_ARM_CORTEX_A53)

/* A53 hardware does not support TLB locking */
BEGIN_FUNC(lockTLBEntry)
    bx lr
END_FUNC(lockTLBEntry)

#endif

