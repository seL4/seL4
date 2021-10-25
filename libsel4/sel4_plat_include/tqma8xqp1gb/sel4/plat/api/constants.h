/*
 * Copyright 2021, Breakaway Consulting Pty. Ltd.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/arch/constants_cortex_a35.h>

#ifdef CONFIG_ARCH_AARCH32
/* Platform support for tqma8xqp1gb is provided for AARCH64 only, even if the
 * Cortex-A35 supports AARCH32 also. Keep this as a build blocker as long as
 * AARCH32 remains untested.
 */
#error "AARCH32 is unsupported"
#endif /* CONFIG_ARCH_AARCH32 */
