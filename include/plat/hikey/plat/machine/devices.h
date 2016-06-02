/*
 * Copyright 2016, General Dynamics C4 Systems
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
#define GIC_DISTRIBUTOR_PPTR        0xfff03000
#define GIC_CONTROLLER_PPTR         0xfff04000

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       GIC_PADDR + 0x1000
#define GIC_CONTROLLER0_PADDR       GIC_PADDR + 0x2000


#define UART0_PADDR                 0xF8015000
#define UART1_PADDR                 0xF7111000
#define UART2_PADDR                 0xF7112000
#define UART3_PADDR                 0xF7113000
#define UART4_PADDR                 0xF7114000
#define GIC_PADDR                   0xf6800000
#define DMTIMER0_PADDR              0xF8008000


#endif
