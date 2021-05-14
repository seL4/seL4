/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#pragma once
#include <config.h>

typedef struct cmdline_opt {
#ifdef CONFIG_PRINTING
    uint16_t console_port;
#endif
#if defined(CONFIG_PRINTING) || defined(CONFIG_DEBUG_BUILD)
    uint16_t debug_port;
#endif
    bool_t   disable_iommu;
} cmdline_opt_t;

void cmdline_parse(const char *cmdline, cmdline_opt_t *cmdline_opt);

