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

#define PIC_IRQ_LINES 16
#define IOAPIC_IRQ_LINES 24

/* interrupt vectors (corresponds to IDT entries) */

#define IRQ_INT_OFFSET 0x20

typedef enum _interrupt_t {
    int_invalid        = -1,
    int_unimpl_dev     = 0x07,
    int_page_fault     = 0x0e,
    int_irq_min        = IRQ_INT_OFFSET, /* First IRQ. */
    /* The ISA and IOAPIC interrupts overlap in the interrupt list
     * We define the ISA interrupts first so that the MSI interrupts
     * end up after the IOAPIC block. This means if you are not using
     * the IOAPICs there is a block (the difference between ISA and IOAPIC)
     * of interrupts that are not used */
    int_irq_isa_min    = int_irq_min,
    int_irq_isa_max    = int_irq_min + PIC_IRQ_LINES - 1,
    int_irq_ioapic_min = int_irq_min,
    int_irq_ioapic_max = (int_irq_ioapic_min + (CONFIG_MAX_NUM_IOAPIC * IOAPIC_IRQ_LINES)) - 1,
    int_irq_msi_min,
    int_irq_msi_max    = int_irq_msi_min + 0xd,
    int_iommu,
    int_timer,
    int_irq_max        = int_timer, /* Last IRQ. */
    int_trap_min,
    int_trap_max       = 0xfe,
    int_spurious       = 0xff,
    int_max            = 0xff
} interrupt_t;

/* Construction of most of the interrupt numbers was relative by padding
 * off previous values. Therefore to ensure we didn't overflow just need
 * to ensure int_trap_min is less than int_trap_max */
compile_assert(interrupt_numbers_not_overflow, int_trap_min < int_trap_max)


typedef enum _irq_t {
    irqInvalid  = -1,
    irq_ioapic_min = int_irq_ioapic_min - IRQ_INT_OFFSET,
    irq_ioapic_max = int_irq_ioapic_max - IRQ_INT_OFFSET,
    irq_isa_min = int_irq_isa_min - IRQ_INT_OFFSET,
    irq_isa_max = int_irq_isa_max - IRQ_INT_OFFSET,
    irq_msi_min = int_irq_msi_min - IRQ_INT_OFFSET,
    irq_msi_max = int_irq_msi_max - IRQ_INT_OFFSET,
    irq_iommu   = int_iommu       - IRQ_INT_OFFSET,
    irq_timer   = int_timer       - IRQ_INT_OFFSET,
    maxIRQ      = int_timer       - IRQ_INT_OFFSET
} irq_t;

#define BIOS_PADDR_START 0x0e0000
#define BIOS_PADDR_END   0x100000

#define BIOS_PADDR_VIDEO_RAM_START 0x000A0000
/* The text mode framebuffer exists part way into the video ram region */
#define BIOS_PADDR_VIDEO_RAM_TEXT_MODE_START 0x000B8000
#define BIOS_PADDR_IVDEO_RAM_END 0x000C0000

#endif
