/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#ifndef __KERNEL_MODE_STACK_H
#define __KERNEL_MODE_STACK_H

#include <config.h>
#include <util.h>

#if CONFIG_MAX_NUM_NODES > 1
#define KERNEL_STACK_ALIGNMENT 4096
#else
#define KERNEL_STACK_ALIGNMENT 16
#endif /* CONFIG_MAX_NUM_NODES > 1 */

#endif /* __KERNEL_MODE_STACK_H */
