/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

#include <config.h>
#include <plat/machine/hardware.h>

#define PPTR_APIC PPTR_KDEV

/* Whether the IOAPIC exists or not will determine where we start mapping
 * the IOMMUs. It is fine to define the DRHU_START even if CONFIG_IOMMU is not
 * set as it will just pollute the preprocessor namespace */
#ifdef CONFIG_IRQ_IOAPIC
#define PPTR_IOAPIC_START (PPTR_APIC + BIT(PAGE_BITS))
#define PPTR_DRHU_START (PPTR_IOAPIC_START + BIT(PAGE_BITS) * CONFIG_MAX_NUM_IOAPIC)
#else  /* CONFIG_IOAPIC */
#define PPTR_DRHU_START (PPTR_APIC + BIT(PAGE_BITS))
#endif /* CONFIG_IOAPIC */

#ifdef CONFIG_IOMMU
#define MAX_NUM_DRHU    ((-PPTR_DRHU_START) >> PAGE_BITS)
#endif

#endif
