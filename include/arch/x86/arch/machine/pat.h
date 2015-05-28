/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_PAT_H
#define __ARCH_MACHINE_PAT_H

#define IA32_PAT_MSR            0x277

#define IA32_PAT_MT_UNCACHEABLE     0x00
#define IA32_PAT_MT_WRITE_COMBINING 0x01
#define IA32_PAT_MT_WRITE_THROUGH   0x04
#define IA32_PAT_MT_WRITE_PROTECTED 0x05
#define IA32_PAT_MT_WRITE_BACK      0x06
#define IA32_PAT_MT_UNCACHED        0x07

#endif
