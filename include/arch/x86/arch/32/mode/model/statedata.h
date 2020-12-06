/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once


#include <config.h>
#include <object/structures.h>
#include <arch/types.h>
#include <model/statedata.h>

extern pde_t ia32KSGlobalPD[BIT(PD_INDEX_BITS)];
extern pte_t ia32KSGlobalPT[BIT(PT_INDEX_BITS)];

#ifdef CONFIG_KERNEL_LOG_BUFFER
extern pte_t ia32KSGlobalLogPT[BIT(PT_INDEX_BITS)];
#endif /* CONFIG_KERNEL_LOG_BUFFER */

NODE_STATE_BEGIN(modeNodeState)
/* Current active page directory. This is really just a shadow of CR3 */
NODE_STATE_DECLARE(paddr_t, ia32KSCurrentPD);
NODE_STATE_END(modeNodeState);

