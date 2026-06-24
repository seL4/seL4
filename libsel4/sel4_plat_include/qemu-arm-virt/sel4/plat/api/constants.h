/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

/* The QEMU virt platform can emulate various cores */
#if defined(CONFIG_ARM_CORTEX_A15)
#include <sel4/arch/constants_cortex_a15.h>
#elif defined(CONFIG_ARM_CORTEX_A53)
#include <sel4/arch/constants_cortex_a53.h>
#elif defined(CONFIG_ARM_CORTEX_A57)
#include <sel4/arch/constants_cortex_a57.h>
#elif defined(CONFIG_ARM_CORTEX_A72)
#include <sel4/arch/constants_cortex_a72.h>
#else
#error "unsupported core"
#endif


#if CONFIG_WORD_SIZE == 32
/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop CONFIG_USER_TOP

/* Last address in the virtual address space that is accessible to user level */
#define seL4_UserVSpaceTop (CONFIG_USER_TOP - 1)
#endif
