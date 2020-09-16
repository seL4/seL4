/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <machine/capdl.h>

void x86_obj_ioports_print_attrs(cap_t ioports_cap);

#ifdef CONFIG_IOMMU
void x86_obj_iospace_print_attrs(cap_t iospace_cap);
void x86_obj_iopt_print_attrs(cap_t iopt_cap);
#endif

void capDL(void);
