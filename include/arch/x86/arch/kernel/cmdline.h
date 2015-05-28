/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_CMDLINE_H
#define __ARCH_KERNEL_CMDLINE_H

#include <config.h>

typedef struct cmdline_opt {
#ifdef DEBUG
    uint16_t console_port[CONFIG_MAX_NUM_NODES];
    uint16_t debug_port[CONFIG_MAX_NUM_NODES];
#endif
#ifdef CONFIG_IOMMU
    bool_t   disable_iommu;
#endif
    uint32_t max_num_nodes;
    uint32_t num_sh_frames;
} cmdline_opt_t;

void cmdline_parse(const char *cmdline, cmdline_opt_t* cmdline_opt);

#endif
