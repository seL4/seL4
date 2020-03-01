/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
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
