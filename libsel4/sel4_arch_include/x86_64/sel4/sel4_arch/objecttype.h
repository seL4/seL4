/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

typedef enum _mode_object {
    seL4_X86_PDPTObject = seL4_NonArchObjectTypeCount,
    seL4_X64_PML4Object,
#ifdef CONFIG_HUGE_PAGE
    seL4_X64_HugePageObject,
#endif
    seL4_ModeObjectTypeCount
} seL4_seL4ArchObjectType;

/* allow seL4_X86_PDPTObject and seL4_IA32_PDPTObject to be used interchangeable */
#define seL4_IA32_PDPTObject seL4_X86_PDPTObject

#ifndef CONFIG_HUGE_PAGE
#define seL4_X64_HugePageObject 0xfffffffe
#endif
