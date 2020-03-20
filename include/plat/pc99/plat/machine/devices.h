/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#define PPTR_APIC KDEV_BASE

#define PPTR_IOAPIC_START (PPTR_APIC + BIT(PAGE_BITS))
#define PPTR_DRHU_START (PPTR_IOAPIC_START + BIT(PAGE_BITS) * CONFIG_MAX_NUM_IOAPIC)

#define MAX_NUM_DRHU    ((-PPTR_DRHU_START) >> PAGE_BITS)

