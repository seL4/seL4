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
 definitions for memory addresses of HiSilicon hi6220 platform
 listed in the order of ascending memory address, similar to the datasheet
 the size must all be powers of 2 and page aligned (4k or 1M).
*/

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

#include <plat_mode/machine/devices.h>

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
#define RTC0_PADDR                  0xF8003000
#define RTC1_PADDR                  0xF8004000
#define DMTIMER0_PADDR              0xF8008000
#define DMTIMER1_PADDR              0xF8009000
#define DMTIMER2_PADDR              0xF800A000
#define DMTIMER3_PADDR              0xF800B000
#define DMTIMER4_PADDR              0xF800C000
#define DMTIMER5_PADDR              0xF800D000
#define DMTIMER6_PADDR              0xF800E000
#define DMTIMER7_PADDR              0xF800F000
#define DMTIMER8_PADDR              0xF8010000


#endif
