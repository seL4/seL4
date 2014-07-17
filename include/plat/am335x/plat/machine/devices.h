/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

/* These devices are used by the kernel. */
#define INTC_PPTR  0xfff01000
#define UART0_PPTR  0xfff02000
#define DMTIMER0_PPTR  0xfff03000

/* Other devices on the SoC. */
#define INTC_PADDR  0x48200000
#define UART0_PADDR  0x44E09000
#define DMTIMER0_PADDR  0x44E05000
#define DMTIMER2_PADDR  0x48040000
#define DMTIMER3_PADDR  0x48042000
#define DMTIMER4_PADDR  0x48044000
#define DMTIMER5_PADDR  0x48046000
#define DMTIMER6_PADDR  0x48048000
#define DMTIMER7_PADDR  0x4804A000

#endif
