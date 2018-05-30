/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 * Copyright (c) 2018, Hesham Almatary <Hesham.Almatary@cl.cam.ac.uk>
 * All rights reserved.
 *
 * This software was was developed in part by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <model/statedata.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <arch/sbi.h>
#include <arch/encoding.h>

#define MAX_AVAIL_P_REGS 2

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

static word_t BOOT_DATA num_avail_p_regs = 0;
static p_region_t BOOT_DATA avail_p_regs[MAX_AVAIL_P_REGS];

BOOT_CODE int get_num_avail_p_regs(void)
{
    return num_avail_p_regs;
}

BOOT_CODE p_region_t get_avail_p_reg(unsigned int i)
{
    return avail_p_regs[i];
}

BOOT_CODE bool_t add_avail_p_reg(p_region_t reg)
{
    if (num_avail_p_regs == MAX_AVAIL_P_REGS) {
        return false;
    }
    avail_p_regs[num_avail_p_regs] = reg;
    num_avail_p_regs++;
    return true;
}

/**
   DONT_TRANSLATE
 */
interrupt_t
getActiveIRQ(void)
{

    word_t temp = read_csr_env(cause);

    if (!(temp & BIT(CONFIG_WORD_SIZE - 1))) {
        return irqInvalid;
    }

    return (temp & 0xf);
}

/* Check for pending IRQ */
bool_t isIRQPending(void)
{
    return (getActiveIRQ() != irqInvalid);
}

/* Enable or disable irq according to the 'disable' flag. */
/**
   DONT_TRANSLATE
*/
void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        if (irq > 1) {
            clear_csr(sie, BIT(irq));
        }
    } else {
        /* FIXME: currently only account for timer interrupts */
        if (irq == KERNEL_TIMER_IRQ) {
            set_csr_env(ie, BIT(irq));
        } else {
            /* TODO: account for external and PLIC interrupts */
        }
    }
}

/* Determine if the given IRQ should be reserved by the kernel. */
bool_t CONST
isReservedIRQ(irq_t irq)
{
    printf("isReservedIRQ \n");
    return false;
}

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    printf("handleReservedIRQ \n");
}

void
ackInterrupt(irq_t irq)
{
    // don't ack the kernel timer interrupt, see the comment in resetTimer
    // to understand why
    if (irq != KERNEL_TIMER_IRQ) {
        clear_csr_env(ip, BIT(irq));
    }
    //set_csr(scause, 0);

    if (irq == 1) {
        sbi_clear_ipi();
    }
}

static inline uint64_t read_current_timer(void)
{
    UNUSED unsigned long prev_trap_address;
    uint64_t time = 0;

    if (config_set(CONFIG_SEL4_RV_MACHINE)) {
        prev_trap_address = read_csr(mtvec);
        /* Set mtvec to riscv-pk trap address */
        write_csr(mtvec, pk_trap_addr);
    }

    /* read_current_timer reads time[h] CSR, which in the case of Spike traps and emulated by riscv-pk */
    time = get_time();

    if (config_set(CONFIG_SEL4_RV_MACHINE)) {
        /* Write back sel4 trap address to mtvec */
        write_csr(mtvec, prev_trap_address);
    }

    return time;
}

void
resetTimer(void)
{
    uint64_t target;
    // ack the timer interrupt. we do this here as due to slow simulation platform there
    // is a race between us setting the new interrupt here, and the ackInterrupt call in
    // handleInterrupt that will happen at some point after this call to resetTimer
    clear_csr_env(ie, KERNEL_TIMER_IRQ);
    // repeatedly try and set the timer in a loop as otherwise there is a race and we
    // may set a timeout in the past, resulting in it never getting triggered
    do {
        /* This should be set properly relying on the frequency (on real HW) */
        target = read_current_timer() + 0x1fff;
        sbi_set_timer(target);
    } while (read_current_timer() > target);
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    sbi_set_timer(read_current_timer() + 0xfffff);
}

void plat_cleanL2Range(paddr_t start, paddr_t end)
{
}
void plat_invalidateL2Range(paddr_t start, paddr_t end)
{
}

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end)
{
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initL2Cache(void)
{
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initIRQController(void)
{
    /* Do nothing */
}

void
handleSpuriousIRQ(void)
{
    /* Do nothing */
    printf("Superior IRQ!! \n");
}
