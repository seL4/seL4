/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __ARCH_MACHINE_FPU_H
#define __ARCH_MACHINE_FPU_H

#include <mode/machine/fpu.h>

bool_t fpsimd_HWCapTest(void);
bool_t fpsimd_init(void);

#endif /* __ARCH_MACHINE_FPU_H */
