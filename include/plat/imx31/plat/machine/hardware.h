/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#define physBase          0x80000000
#define kernelBase        0xf0000000
#define physMappingOffset (kernelBase - physBase)
#define BASE_OFFSET       physMappingOffset
#define PPTR_TOP          0xfff00000
#define PADDR_TOP         (PPTR_TOP - BASE_OFFSET)

#endif
