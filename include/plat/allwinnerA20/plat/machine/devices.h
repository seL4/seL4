/*
 * Copyright 2015, DornerWorks, Ltd.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

/*
 definitions for memory addresses of allwinner A20 platform
 listed in the order of ascending memory address, similar to the datasheet
 the size must all be powers of 2 and page aligned (4k or 1M).
*/

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

/* These devices are used by the seL4 kernel. */
#define UART0_PPTR                  0xfff01000
#define TIMER0_PPTR                 0xfff02000
#define GIC_DISTRIBUTOR_PPTR        0xfff03000
#define GIC_CONTROLLER_PPTR         0xfff04000
#define ARM_DEBUG_MMAPPING_PPTR     0xfff05000

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       GIC_PADDR + 0x1000
#define GIC_CONTROLLER0_PADDR       GIC_PADDR + 0x2000



#define UART0_PADDR                 0x01C28000

/* CCU, Interrupt, Timer, OWA */
/* Timer = PPTR + 0xC00 */
#define TIMER0_PADDR                0x01C20000

#define GIC_PADDR                   0x01C80000

/* TODO: Add other devices PADDRs */

#define SPI0_PADDR  0x01C05000
#define SPI1_PADDR  0x01C06000

#endif
