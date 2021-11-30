/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/arch/constants_cortex_a8.h>

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xe0000000
