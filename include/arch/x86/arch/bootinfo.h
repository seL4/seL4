/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_BOOTINFO_H
#define __ARCH_BOOTINFO_H

#define MAX_NUM_FREEMEM_REG 16

/*
 * The maximum number of reserved regions we have is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel device:
 *      - ioapics (CONFIG_MAX_NUM_IOAPIC)
 *      - iommus (MAX_NUM_DRHU)
 *      - apic (1)
 *      - the reserved MSI region (1)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + CONFIG_MAX_NUM_IOAPIC + MAX_NUM_DRHU + 2)

#endif
