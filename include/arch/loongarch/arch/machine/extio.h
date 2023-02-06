/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn 
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>

/*
 * The "RISC-V Instruction Set Manual Volume II: Privileged Architecture" V1.10
 * defines a platform-level interrupt controller (PLIC), that manages global
 * interrupt on a RISC-V platform. A PLIC takes multiple interrupt sources,
 * usually the platform peripherals, and delivers them to different hart
 * contexts depending on the interrupt routing configuration. A hart is a
 * logical CPU core, a hart context is a privilege level on a given hart,
 * usually M-Mode or S-Mode.
 *
 * If an interrupt is pending for a particular hart context, the PLIC will
 * assert the hart context's external interrupt line. The hart can then issue
 * an interrupt claim to the PLIC, in response it will receive the pending
 * interrupt with the highest priority. In multicore systems, if an interrupt is
 * routed to multiple harts, the first hart to claim it gets to process it and
 * subsequent harts won't see it. Upon claiming, the PLIC will de-assert a hart
 * context's external interrupt line, even if there are more pending interrupt.
 * When a  hart has finished processing a claimed interrupt, it notifies the
 * PLIC about the completion. The PLIC will assert the hart context's external
 * interrupt line if there are more pending interrupts.
 *
 * The RISC-V specification is not clear how interrupt priorities affect
 * asserting a hart context's external interrupt line. The PLIC is supposed to
 * deassert the line upon interrupt claiming. but could asserted it again if an
 * interrupt of a higher priority arrives, even if the current claim has no been
 * completed. This allows interrupt nesting. However, seL4 does not support this
 * and cannot be interrupted when running in kernel mode. To achieve this, it
 * keeps the S-Mode hart context's external interrupts masked until leaving to
 * user space. However, great care must be taken if interrupt priorities are not
 * implemented in the PLIC and the interrupt claim completion does not happen
 * within the kernel's interrupt trap handling. If completion is done when the
 * user mode ISR ack's the interrupt, the user mode driver that implements the
 * ISR can simply block all other platform interrupts by not doing the ack.
 * Luckily, this will not affect the timer used for the preemptive scheduler,
 * because RISC-V has a dedicated timer interrupt that is separate from the
 * external interrupt.
 *
 *
 * This file defines the seL4 kernel's internal API for PLIC access that must be
 * implemented by platform specific PLIC drivers.
 * The implementation here are
 * used on the 'spike' platform, which is just a RISC-V ISA reference
 * implementation that does not have a PLIC to trigger external interrupts.
 */


/*
 * This function is called when an interrupt is pending. It claims an interrupt
 * from the PLIC and returns it. If no interrupt could be claimed 'irqInvalid'
 * is returned.
 *
 * @return     interrupt number or irqInvalid.
 */
static inline irq_t extio_get_claim(void);

/*
 * This function is called to complete a claim process for an interrupt.
 *
 * @param[in]  irq  interrupt to complete.
 */
static inline void plic_complete_claim(irq_t irq);

/*
* This function is called to mask (disable) or unmasks (enable) an interrupt in
* the PLIC.
 *
 * @param[in]  disable  True to mask/disable, False to unmask/enable.
 * @param[in]  irq      interrupt to mask/unmask.
 */
static inline void extio_mask_irq(bool_t disable, irq_t irq);


#ifdef HAVE_SET_TRIGGER
/*
 * If HAVE_SET_TRIGGER is defined, this function is called to configure an
 * interrupt source on the PLIC as edge or level triggered. By default all
 * interrupts should be configures to be edge triggered.
 *
 * @param[in]  irq             interrupt to set trigger mode for.
 * @param[in]  edge_triggered  True for edge triggered, False for level
 *                             triggered.
 */
static inline void plic_irq_set_trigger(irq_t irq, bool_t edge_triggered);
#endif /* HAVE_SET_TRIGGER */

/*
 * This function is called during the boot process to perform hart specific
 * extend io interrupt initialisation. It is called as part of the core local initialisation
 * process and runs before extio_init_controller() is called.
 */
static inline void extioi_init_hart(void);

/*
 * This function is called during the boot process to perform extend io interrupt
 * controller initialisation.
 */
static inline void ls7a_intc_init(void);
