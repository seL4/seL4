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
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <arch/sbi.h>

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

    uint64_t temp = 0;
    asm volatile ("csrr %0, scause":"=r" (temp)::);

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
        /* FIXME: currently only account for user/supervisor and timer interrupts */
        if (irq == 4 /* user timer */ || irq == 5 /* supervisor timer */) {
            set_csr(sie, BIT(irq));
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
        clear_csr(sip, BIT(irq));
    }
    //set_csr(scause, 0);

    if (irq == 1) {
        sbi_clear_ipi();
    }
}

static inline uint64_t get_cycles(void)
#if __riscv_xlen == 32
{
    uint32_t nH, nL;
    __asm__ __volatile__ (
        "rdtimeh %0\n"
        "rdtime  %1\n"
        : "=r" (nH), "=r" (nL));
    return ((uint64_t) ((uint64_t) nH << 32)) | (nL);
}
#else
{
    uint64_t n;
    __asm__ __volatile__ (
        "rdtime %0"
        : "=r" (n));
    return n;
}
#endif

static inline int read_current_timer(unsigned long *timer_val)
{
    *timer_val = get_cycles();
    return 0;
}

void
resetTimer(void)
{
    uint64_t target;
    // ack the timer interrupt. we do this here as due to slow simulation platform there
    // is a race between us setting the new interrupt here, and the ackInterrupt call in
    // handleInterrupt that will happen at some point after this call to resetTimer
    clear_csr(sip, KERNEL_TIMER_IRQ);
    // repeatedly try and set the timer in a loop as otherwise there is a race and we
    // may set a timeout in the past, resulting in it never getting triggered
    do {
        /* This should be set properly relying on the frequency (on real HW) */
        target = get_cycles() + 0x1fff;
        sbi_set_timer(target);
    } while (get_cycles() > target);
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    sbi_set_timer(get_cycles() + 0xfffff);
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
