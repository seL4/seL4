/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_PIT_H
#define __PLAT_MACHINE_PIT_H

/* ms after which a wraparound occurs (max. 54) */
#define PIT_WRAPAROUND_MS 50

void pit_init(void);
void pit_wait_wraparound(void);

#endif
