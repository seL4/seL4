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

BEGIN_FUNC(idle_thread)
    mov r0, #0
1:
#if 0
    /*
     * Idling is currently broken on the KZM with ARM1136.
     * This should be SoC-specific. For now, don't idle, just spin.
     * If anybody cares about power consumption, we can fix it for them.
     */
    mcr p15, 0, r0, c7, c10, 4
    mcr p15, 0, r0, c7, c0, 4
#endif
    b 1b
END_FUNC(idle_thread)
