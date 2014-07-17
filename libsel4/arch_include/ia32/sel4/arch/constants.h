/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_ARCH_CONSTANTS_H

#include <autoconf.h>

#define TLS_GDT_ENTRY 6
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY 7
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#ifndef __ASM__
#include <sel4/arch/objecttype.h>
#endif

/* MSI IRQs need to be offset by this value in order to come
 * in along the right vector. Get the seL4_IRQHandler for
 * the irq number you want, then add IRQ_OFFSET to it when
 * programming the device */
#define IRQ_OFFSET 0x20

/* Range for MSI irqs */
#define MSI_MIN 0x10
#define MSI_MAX 0x1d

#endif
