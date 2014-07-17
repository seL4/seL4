/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_H
#define __PLAT_MACHINE_H

/* interrupt vectors (corresponds to IDT entries) */

typedef enum _interrupt_t {
    int_invalid     = -1,
    int_unimpl_dev  = 0x07,
    int_page_fault  = 0x0e,
    int_irq_min     = 0x20, /* First IRQ. */
    int_irq_isa_min = 0x20,
    int_irq_isa_max = 0x2f,
    int_irq_msi_min = 0x30,
    int_irq_msi_max = 0x3d,
    int_iommu       = 0x3e,
    int_timer       = 0x3f,
    int_irq_max     = 0x3f, /* Last IRQ. */
    int_trap_min    = 0x40,
    int_trap_max    = 0xfe,
    int_spurious    = 0xff,
    int_max         = 0xff
} interrupt_t;

#define IRQ_INT_OFFSET 0x20

typedef enum _irq_t {
    irqInvalid  = -1,
    irq_isa_min = int_irq_isa_min - IRQ_INT_OFFSET, /* 0x00 */
    irq_isa_max = int_irq_isa_max - IRQ_INT_OFFSET, /* 0x0f */
    irq_msi_min = int_irq_msi_min - IRQ_INT_OFFSET, /* 0x10 */
    irq_msi_max = int_irq_msi_max - IRQ_INT_OFFSET, /* 0x1d */
    irq_iommu   = int_iommu       - IRQ_INT_OFFSET, /* 0x1e */
    irq_timer   = int_timer       - IRQ_INT_OFFSET, /* 0x1f */
    maxIRQ      = int_timer       - IRQ_INT_OFFSET  /* 0x1f */
} irq_t;

#define BIOS_PADDR_START 0x0e0000
#define BIOS_PADDR_END   0x100000

#define BIOS_PADDR_VIDEO_RAM_START 0x000A0000
/* The text mode framebuffer exists part way into the video ram region */
#define BIOS_PADDR_VIDEO_RAM_TEXT_MODE_START 0x000B8000
#define BIOS_PADDR_IVDEO_RAM_END 0x000C0000

#endif
