/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifdef DEBUG

#include <arch/kernel/lock.h>
#include <arch/linker.h>

/* global spinlocks */
lock_t lock_debug DATA_GLOB;

#endif /* DEBUG */
