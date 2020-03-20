/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define IA32_PAT_MSR            0x277

#define IA32_PAT_MT_UNCACHEABLE     0x00
#define IA32_PAT_MT_WRITE_COMBINING 0x01
#define IA32_PAT_MT_WRITE_THROUGH   0x04
#define IA32_PAT_MT_WRITE_PROTECTED 0x05
#define IA32_PAT_MT_WRITE_BACK      0x06
#define IA32_PAT_MT_UNCACHED        0x07

