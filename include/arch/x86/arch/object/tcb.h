/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <object/structures.h>

#ifdef CONFIG_VTX
exception_t decodeSetEPTRoot(cap_t cap);
void Arch_leaveVMAsyncTransfer(tcb_t *tcb);
#endif
