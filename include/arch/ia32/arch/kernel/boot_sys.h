/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_BOOT_SYS_H
#define __ARCH_KERNEL_BOOT_SYS_H

#include <arch/kernel/multiboot.h>

bool_t in_boot_phase(void);
cpu_id_t cur_cpu_id(void);
node_id_t node_of_cpu(cpu_id_t);
uint16_t console_port_of_node(node_id_t node_id);
uint16_t debug_port_of_node(node_id_t node_id);

void boot_sys(
    unsigned long multiboot_magic,
    multiboot_info_t* mbi,
    uint32_t apic_khz
);
void boot_node(void);
void insert_dev_p_reg(p_region_t reg);

#endif
