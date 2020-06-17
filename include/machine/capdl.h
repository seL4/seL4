/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define ESCAPE               0xaa
#define START                0xff
#define END                  0xbb

#define START_ESCAPE         0xa0
#define ESCAPE_ESCAPE        0xa1
#define END_ESCAPE           0xa2

#define PD_COMMAND           0xf0
#define RQ_COMMAND           0xf1
#define EP_COMMAND           0xf2
#define CN_COMMAND           0xf3
#define TCB_COMMAND          0xfb
#define IRQ_COMMAND          0xf4
#define PT_COMMAND           0xf5
#define ASID_POOL_COMMAND    0xf6
#define IO_PT_COMMAND        0xf7
#define IO_SPACE_COMMAND     0xf8
#define VERSION_COMMAND      0xf9
#define DONE                 0xfa

#define CAPDL_VERSION        0

