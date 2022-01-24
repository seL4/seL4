/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <sel4/config.h>

#ifdef CONFIG_ENABLE_SMP_SUPPORT
#define ENABLE_SMP_SUPPORT /* Set ENABLE_SMP_SUPPORT for kernel source files */
#define SMP_TERNARY(_smp, _up)      _smp
#else /* not CONFIG_ENABLE_SMP_SUPPORT */
#define SMP_TERNARY(_smp, _up)      _up
#endif /* [not] CONFIG_ENABLE_SMP_SUPPORT */

#define SMP_COND_STATEMENT(_st)     SMP_TERNARY(_st,)
#define UP_COND_STATEMENT(_st)      SMP_TERNARY(,_st)


#ifdef CONFIG_KERNEL_MCS
#define MCS_TERNARY(_mcs, _non_mcs) _mcs
#else /* not CONFIG_KERNEL_MCS */
#define MCS_TERNARY(_mcs, _non_mcs) _non_mcs
#endif /* [not] CONFIG_KERNEL_MCS */

#define MSC_COND_STATEMENT(_st)     MCS_TERNARY(_st,)


#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#ifdef CONFIG_ARM_PA_SIZE_BITS_40
#define AARCH64_VSPACE_S2_START_L1
#endif /* CONFIG_ARM_PA_SIZE_BITS_40 */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
