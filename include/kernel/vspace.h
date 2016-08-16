/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_VSPACE_H
#define __KERNEL_VSPACE_H

#include <config.h>
#include <arch/kernel/vspace.h>

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
exception_t benchmark_arch_map_logBuffer(word_t frame_cptr);
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#endif
