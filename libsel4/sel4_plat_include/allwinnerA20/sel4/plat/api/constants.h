/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/arch/constants_cortex_a7.h>

/* (Deprecated) First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xa0000000

/* Last address in the virtual address space that is accessible to user level */
#define seL4_UserVSpaceTop (seL4_UserTop - 1)
