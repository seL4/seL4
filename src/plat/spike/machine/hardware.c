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
#include <util.h>
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

#define STIMER_IP 5
#define STIMER_IE 5
#define STIMER_CAUSE 5
#define SEXTERNAL_IP 9
#define SEXTERNAL_IE 9
#define SEXTERNAL_CAUSE 9

#define RESET_CYCLES ((CONFIG_SPIKE_CLOCK_FREQ / MS_IN_S) * CONFIG_TIMER_TICK_MS)


BOOT_CODE int get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t *get_avail_p_regs(void)
{
    return (p_region_t *) avail_p_regs;
}

BOOT_CODE int get_num_dev_p_regs(void)
{
    if (dev_p_regs != NULL) {
        return (sizeof(dev_p_regs) / sizeof(p_region_t));
    } else {
        return 0;
    }
}

BOOT_CODE p_region_t get_dev_p_reg(word_t i)
{
    /* We need this if guard as some RISC-V configurations don't declare any
     * device regions and some compilers complain about indexing an empty array
     * due to not being able to infer that get_dev_p_reg is only called if
     * dev_p_regs contains entries.
     */
    if (get_num_dev_p_regs() == 0) {
        printf("%s: No devices present.\n", __func__);
        halt();
    }
    return dev_p_regs[i];
}

BOOT_CODE void map_kernel_devices(void)
{
    if (kernel_devices == NULL) {
        return;
    }

    for (int i = 0; i < (sizeof(kernel_devices) / sizeof(paddr_t)); i++) {
        map_kernel_frame(kernel_devices[i], PPTR_KDEV,
                         VMKernelOnly);
    }
}

/**
   DONT_TRANSLATE
 */
interrupt_t getActiveIRQ(void)
{

    uint64_t temp = 0;
    asm volatile("csrr %0, scause":"=r"(temp));

    if (!(temp & BIT(CONFIG_WORD_SIZE - 1))) {
        return irqInvalid;
    }

    return (temp & 0xf);
}

/* Check for pending IRQ */
bool_t isIRQPending(void)
{
    word_t sip = read_sip();
    return (sip & (BIT(STIMER_IP) | BIT(SEXTERNAL_IP)));
}

/* Enable or disable irq according to the 'disable' flag. */
/**
   DONT_TRANSLATE
*/
void maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        if (irq > 1) {
            clear_sie_mask(BIT(irq));
        }
    } else {
        /* FIXME: currently only account for user/supervisor and timer interrupts */
        if (irq == 4 /* user timer */ || irq == 5 /* supervisor timer */) {
            set_sie_mask(BIT(irq));
        } else {
            /* TODO: account for external and PLIC interrupts */
        }
    }
}

/* Determine if the given IRQ should be reserved by the kernel. */
bool_t CONST isReservedIRQ(irq_t irq)
{
    printf("isReservedIRQ \n");
    return false;
}

void ackInterrupt(irq_t irq)
{
    // don't ack the kernel timer interrupt, see the comment in resetTimer
    // to understand why

    if (irq == 1) {
        sbi_clear_ipi();
    }
}

static inline uint64_t get_cycles(void)
#if __riscv_xlen == 32
{
    uint32_t nH, nL;
    asm volatile(
        "rdtimeh %0\n"
        "rdtime  %1\n"
        : "=r"(nH), "=r"(nL));
    return ((uint64_t)((uint64_t) nH << 32)) | (nL);
}
#else
{
    uint64_t n;
    asm volatile(
        "rdtime %0"
        : "=r"(n));
    return n;
}
#endif

static inline int read_current_timer(unsigned long *timer_val)
{
    *timer_val = get_cycles();
    return 0;
}

void resetTimer(void)
{
    uint64_t target;
    // repeatedly try and set the timer in a loop as otherwise there is a race and we
    // may set a timeout in the past, resulting in it never getting triggered
    do {
        target = get_cycles() + RESET_CYCLES;
        sbi_set_timer(target);
    } while (get_cycles() > target);
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void initTimer(void)
{
    sbi_set_timer(get_cycles() + RESET_CYCLES);
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
BOOT_CODE void initL2Cache(void)
{
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void initIRQController(void)
{
    /* Do nothing */
}

void handleSpuriousIRQ(void)
{
    /* Do nothing */
    printf("Superior IRQ!! \n");
}
