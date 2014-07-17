/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

/*
 * ARM L2 Cache controller L2C-310
 */

#ifndef __ARCH_MACHINE_L2C_310_H
#define __ARCH_MACHINE_L2C_310_H

#include <arch/types.h>


/** MODIFIES: [*] */
void initL2Cache(void);

/** MODIFIES: [*] */
void plat_cleanCache(void);
/** MODIFIES: [*] */
void plat_cleanL2Range(paddr_t start, paddr_t end);
/** MODIFIES: [*] */
void plat_invalidateL2Range(paddr_t start, paddr_t end);
/** MODIFIES: [*] */
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);

#endif /* !__ARCH_MACHINE_L2C_310_H */
