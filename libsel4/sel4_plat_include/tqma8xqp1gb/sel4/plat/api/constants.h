/*
 * Copyright 2021, Breakaway Consulting Pty. Ltd.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>

/* Cortex A35 Manual Table C7.1 */
#define seL4_NumHWBreakpoints (6)
#define seL4_NumExclusiveBreakpoints (6)
#define seL4_NumExclusiveWatchpoints (4)
#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstWatchpoint (6)
#define seL4_NumDualFunctionMonitors (0)
#endif

#ifdef CONFIG_ARCH_AARCH32
/* Platform support for tqma8xqp1gb is provided for AARCH64 only, even if the
 * Cortex-A35 supports AARCH32 also. Keep this as a build blocker as long as
 * AARCH32 remains untested.
 */
#error "AARCH32 is unsupported"
#endif /* CONFIG_ARCH_AARCH32 */
