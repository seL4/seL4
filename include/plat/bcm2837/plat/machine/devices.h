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

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

/* These devices are used by the seL4 kernel. */
#define BUS_ADDR_OFFSET             0x7E000000
#define PADDDR_OFFSET               0x3F000000

#define INTC_BUSADDR                0x7E00B000
#define UART_BUSADDR                0x7E215000
#define SDHC_BUSADDR                0x7E300000
#define USB2_BUSADDR                0x7E980000
#define SYSTEM_TIMER_BUSADDR        0x7E003000
#define ARM_TIMER_BUSADDR           0x7E00B000

#define UART_PPTR                   0xFFF01000
#define INTC_PPTR                   0xFFF02000
#define TIMER_PPTR                  0xFFF03000
#define ARM_LOCAL_PPTR              0xFFF04000

#define ARM_LOCAL_PADDR             0x40000000

/* We convert from the VC CPU BUS addresses to ARM Physical addresses due to the extra
    VC (Video controller) MMU */
#define INTC_PADDR                  (INTC_BUSADDR-BUS_ADDR_OFFSET+PADDDR_OFFSET)
#define UART_PADDR                  (UART_BUSADDR-BUS_ADDR_OFFSET+PADDDR_OFFSET)
#define SDHC_PADDR                  (SDHC_BUSADDR-BUS_ADDR_OFFSET+PADDDR_OFFSET)
#define USB2_PADDR                  (USB2_BUSADDR-BUS_ADDR_OFFSET+PADDDR_OFFSET)
#define TIMER_PADDR                 (ARM_TIMER_BUSADDR-BUS_ADDR_OFFSET+PADDDR_OFFSET)

#endif /* !__PLAT_MACHINE_DEVICES_H */
