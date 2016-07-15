/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_TCB_H
#define __ARCH_OBJECT_TCB_H

#include <config.h>
#include <types.h>
#include <object/structures.h>

#ifdef CONFIG_VTX
exception_t decodeSetEPTRoot(cap_t cap, extra_caps_t extraCaps);
void Arch_leaveVMAsyncTransfer(tcb_t *tcb);
#endif

#endif
