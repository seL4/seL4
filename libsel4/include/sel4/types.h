/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/simple_types.h>
#include <sel4/macros.h>
#include <sel4/arch/types.h>
#include <sel4/sel4_arch/types.h>
#include <sel4/sel4_arch/types_gen.h>
#include <sel4/syscall.h>
#include <sel4/objecttype.h>
#include <sel4/sel4_arch/objecttype.h>
#include <sel4/arch/objecttype.h>
#include <sel4/errors.h>
#include <sel4/constants.h>
#include <sel4/shared_types_gen.h>
#include <sel4/shared_types.h>
#include <sel4/mode/types.h>

#ifdef CONFIG_RETYPE_FAN_OUT_LIMIT
#  define seL4_UntypedRetypeMaxObjects CONFIG_RETYPE_FAN_OUT_LIMIT
#else
#  define seL4_UntypedRetypeMaxObjects 256
#endif

typedef seL4_Word seL4_NodeId;
typedef seL4_Word seL4_PAddr;
typedef seL4_Word seL4_Domain;

typedef seL4_CPtr seL4_CNode;
typedef seL4_CPtr seL4_IRQHandler;
typedef seL4_CPtr seL4_IRQControl;
typedef seL4_CPtr seL4_TCB;
typedef seL4_CPtr seL4_Untyped;
typedef seL4_CPtr seL4_DomainSet;
typedef seL4_CPtr seL4_SchedContext;
typedef seL4_CPtr seL4_SchedControl;

typedef seL4_Uint64 seL4_Time;

#define seL4_NilData 0

#include <sel4/arch/constants.h>
