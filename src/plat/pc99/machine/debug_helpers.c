/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifdef DEBUG

#include <arch/model/statedata.h>
#include <plat/machine/debug_helpers.h>
#include <plat/machine/io.h>

#define DEBUG_PORT ia32KSdebugPort

unsigned char getDebugChar(void)
{
    while ((in8(DEBUG_PORT + 5) & 1) == 0);
    return in8(DEBUG_PORT);
}

void putDebugChar(unsigned char a)
{
    while ((in8(DEBUG_PORT + 5) & 0x20) == 0);
    out8(DEBUG_PORT, a);
}

#endif
