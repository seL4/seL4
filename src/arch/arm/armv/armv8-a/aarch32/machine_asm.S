/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <machine/assembler.h>

.code 32
.section .text, "ax"

.global lockTLBEntry

#if defined(ARM_CORTEX_A57)
/* A57 hardware does not support TLB locking */
BEGIN_FUNC(lockTLBEntry)
    bx lr
END_FUNC(lockTLBEntry)

#endif

