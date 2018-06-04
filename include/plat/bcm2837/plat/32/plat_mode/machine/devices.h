/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __PLAT_MODE_MACHINE_DEVICES_H
#define __PLAT_MODE_MACHINE_DEVICES_H

/* These devices are used by the seL4 kernel. */
#define UART_PPTR                   0xFFF01000
#define INTC_PPTR                   0xFFF02000
#define TIMER_PPTR                  0xFFF03000
#define ARM_LOCAL_PPTR              0xFFF04000

#endif /* __PLAT_MODE_MACHINE_DEVICES_H */
