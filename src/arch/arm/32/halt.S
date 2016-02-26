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

BEGIN_FUNC(halt)
    /* Disable interrupts */
    cpsid iaf

#ifdef DEBUG
    /* print a message */
    ldr r0, =halt_message
    blx puts
#endif

    /* Halt */
1:  mcr p15, 0, r0, c7, c0, 4
    b 1b
END_FUNC(halt)

#ifdef DEBUG
.section .rodata
halt_message:
.asciz "halting..."
#endif

