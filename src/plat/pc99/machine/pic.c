/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/linker.h>
#include <plat/machine/io.h>
#include <plat/machine/hardware.h>
#include <plat/machine/pic.h>

/* PIC (i8259) base registers */
#define PIC1_BASE 0x20
#define PIC2_BASE 0xa0

/* Program PIC (i8259) to remap IRQs 0-15 to interrupt vectors starting at 'interrupt' */
BOOT_CODE void
pic_remap_irqs(interrupt_t interrupt)
{
    out8(PIC1_BASE, 0x11);
    out8(PIC2_BASE, 0x11);
    out8(PIC1_BASE + 1, interrupt);
    out8(PIC2_BASE + 1, interrupt + 8);
    out8(PIC1_BASE + 1, 0x04);
    out8(PIC2_BASE + 1, 0x02);
    out8(PIC1_BASE + 1, 0x01);
    out8(PIC2_BASE + 1, 0x01);
    out8(PIC1_BASE + 1, 0x0);
    out8(PIC2_BASE + 1, 0x0);
}

void pic_mask_irq(bool_t mask, irq_t irq)
{
    uint8_t  bit_mask;
    uint16_t pic_port;

    assert(irq >= irq_isa_min);
    assert(irq <= irq_isa_max);

    if (irq < 8) {
        bit_mask = BIT(irq);
        pic_port = PIC1_BASE + 1;
    } else {
        bit_mask = BIT(irq - 8);
        pic_port = PIC2_BASE + 1;
    }

    if (mask) {
        /* Disables the interrupt */
        out8(pic_port, (in8(pic_port) | bit_mask));
    } else {
        /* Enables the interrupt */
        out8(pic_port, (in8(pic_port) & ~bit_mask));
    }
}

bool_t pic_is_irq_pending(void)
{
    /* Interrupt Request Register (IRR) - holds pending IRQs */
    uint8_t irr;

    /* Send to PIC1's OCW3, in order to read IRR from next inb instruction */
    out8(PIC1_BASE, 0x0a);

    /* Read IRR */
    irr = in8(PIC1_BASE);

    /* Since slave PIC is connected to IRQ2 of master PIC,
     * there is no need to check IRR of slave PIC.
     */
    return irr != 0;
}

void pic_ack_active_irq(void)
{
    if (getActiveIRQ() >= irq_isa_min + 8) {
        /* ack slave PIC */
        out8(PIC2_BASE, 0x20);
    }
    /* ack master PIC */
    out8(PIC1_BASE, 0x20);
}
