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

#include <sel4/sel4_arch/constants.h>

#ifndef __ASM__
#include <sel4/sel4_arch/objecttype.h>
#include <sel4/arch/objecttype.h>
#endif

/* Currently MSIs do not go through a vt-d translation by
 * the kernel, therefore when the user programs an MSI they
 * need to know how the 'vector' they allocated relates to
 * the actual vector table. In this case if they allocate
 * vector X they need to program their MSI to interrupt
 * vector X + IRQ_OFFSET */
#define IRQ_OFFSET (0x20 + 16)

/* When allocating vectors for IOAPIC or MSI interrupts,
 * this represent the valid range */
#define VECTOR_MIN (0)
#define VECTOR_MAX (109)

/* Legacy definitions */
#define MSI_MIN VECTOR_MIN
#define MSI_MAX VECTOR_MAX

#endif
