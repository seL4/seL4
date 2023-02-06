/*
 * Copyright 2022 tyy team
 *
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * A dummy extio driver for 3A5000 platform.
 * This file will be updated later to support ext io interrupt.
 */

#pragma once

/* Since this is a template, it has a dummy implementation for the trigger API
 * function plic_irq_set_trigger().
 */
#define HAVE_SET_TRIGGER 1

#include <arch/machine/extio.h>
#include <arch/machine.h>

#define UART0_IRQ 2
#define KEYBOARD_IRQ 3

static inline irq_t extio_get_claim(void)
{
    printf("no PLIC present, can't claim any interrupt\n");
    return irqInvalid;
}

static inline void plic_complete_claim(irq_t irq)
{
    printf("no PLIC present, can't complete claim for interrupt %d\n",
           (int)irq);
}

static inline void extio_mask_irq(bool_t disable, irq_t irq)
{    
    if(disable){
        clear_csr_ecfg(BIT(irq));
    }else{
        set_csr_ecfg(BIT(irq));
    }
}

static inline void plic_irq_set_trigger(irq_t irq, bool_t edge_triggered)
{
    printf("no PLIC present, can't set interrupt %d to %s triggered\n",
           (int)irq, edge_triggered ? "edge" : "level");
}

static inline void extioi_init_hart(void)
{
    // printf("no extio present, skip hart specific initialisation\n");

    /* This is an example, seL4 kernel do not handle externel interrupts.
     * The code enable 2~4 extend io interrupt*/
    iocsr_writeq((0x1UL << UART0_IRQ) | (0x1UL << KEYBOARD_IRQ), LOONGARCH_IOCSR_EXTIOI_EN_BASE);

    /* extioi[31:0] map to cpu irq pin INT1, other to INT0 */
    iocsr_writeq(0x01UL,LOONGARCH_IOCSR_EXTIOI_IPMAP_BASE);

    /* extioi IRQ 0-7 route to core 0, use node type 0 */
    iocsr_writeq(0x0UL,LOONGARCH_IOCSR_EXTIOI_ROUTE_BASE);

    /* nodetype0 set to 1, always trigger at node 0 */
    iocsr_writeq(0x1,LOONGARCH_IOCSR_EXTIOI_NODEMAP_BASE);
}

static inline void ls7a_intc_init(void)
{
    // printf("no extio interrupt supported yet. Will be supported later\n");

    /* enable uart0 & keyboard */
    *(volatile unsigned long*)(LS7A_INT_MASK_REG) = ~((0x1UL << UART0_IRQ) | (0x1UL << KEYBOARD_IRQ));

    *(volatile unsigned long*)(LS7A_INT_EDGE_REG) = ((0x1UL << UART0_IRQ) | (0x1UL << KEYBOARD_IRQ));

    /* route to the same irq in extioi */
    *(volatile unsigned char*)(LS7A_INT_HTMSI_VEC_REG + UART0_IRQ) = UART0_IRQ;
    *(volatile unsigned char*)(LS7A_INT_HTMSI_VEC_REG + KEYBOARD_IRQ) = KEYBOARD_IRQ;

    *(volatile unsigned long*)(LS7A_INT_POL_REG) = 0x0UL;
}
