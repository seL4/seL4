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

#ifndef __ARCH_KERNEL_SMP_SYS_H_
#define __ARCH_KERNEL_SMP_SYS_H_

/* Lower memory address to copy APs boot code in real mode. Actual memory starts at
 * 0x500 but we need to round up to a page aligned address in order to send the
 * startup IPI */
#define BOOT_NODE_PADDR 0x1000
/* Limit of memory region we can copy the AP to */
#define BOOT_NODE_MAX_PADDR 0x7bff

#ifdef ENABLE_SMP_SUPPORT
void boot_node(void);
BOOT_CODE void start_boot_aps(void);
BOOT_CODE bool_t copy_boot_code_aps(uint32_t mem_lower);
#endif /* ENABLE_SMP_SUPPORT */

#endif /* __ARCH_KERNEL_SMP_SYS_H_ */
