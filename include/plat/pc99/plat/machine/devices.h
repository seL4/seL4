/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#define PPTR_APIC KDEV_BASE

#if defined(CONFIG_VGA_PRINTING)
#define PPTR_VGA    (PPTR_APIC + BIT(PAGE_BITS))
#define PPTR_IOAPIC_START (PPTR_VGA + BIT(PAGE_BITS))
#else
#define PPTR_IOAPIC_START (PPTR_APIC + BIT(PAGE_BITS))
#endif

#define PPTR_DRHU_START (PPTR_IOAPIC_START + BIT(PAGE_BITS) * CONFIG_MAX_NUM_IOAPIC)

#define MAX_NUM_DRHU    ((-PPTR_DRHU_START) >> PAGE_BITS)

