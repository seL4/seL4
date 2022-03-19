/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <machine/registerset.h>
#include <machine/timer.h>
#include <arch/machine.h>
#include <arch/smp/ipi.h>

#ifndef CONFIG_KERNEL_MCS
#define RESET_CYCLES ((TIMER_CLOCK_HZ / MS_IN_S) * CONFIG_TIMER_TICK_MS)
#endif /* !CONFIG_KERNEL_MCS */

#define IS_IRQ_VALID(X) (((X)) <= maxIRQ && (X) != irqInvalid)

word_t PURE getRestartPC(tcb_t *thread)
{
    return getRegister(thread, FaultIP);
}

void setNextPC(tcb_t *thread, word_t v)
{
    setRegister(thread, NextIP, v);
}

BOOT_CODE void map_kernel_devices(void)
{
    map_kernel_frame(0x00000000, PLIC_PPTR, VMKernelOnly);

    /* If there are no kernel device frames at all, then kernel_device_frames is
     * NULL. Thus we can't use ARRAY_SIZE(kernel_device_frames) here directly,
     * but have to use NUM_KERNEL_DEVICE_FRAMES that is defined accordingly.
     */
    for (int i = 0; i < NUM_KERNEL_DEVICE_FRAMES; i++) {
        const kernel_frame_t *frame = &kernel_device_frames[i];
        map_kernel_frame(frame->paddr, frame->pptr, VMKernelOnly);
        if (!frame->userAvailable) {
            reserve_region((p_region_t) {
                .start = frame->paddr,
                .end   = frame->paddr + BIT(seL4_LargePageBits)
            });
        }
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

#if CONFIG_RISCV_NUM_VTIMERS > 0
#define VTIMER_MAX_CYCLES   0xffffffffffffffff
#define NUM_VTIMERS     ((CONFIG_RISCV_NUM_VTIMERS + 1))

struct vtimer {
    uint64_t    cycles[NUM_VTIMERS];
    uint64_t    next_cycles;
    uint64_t    next;
    PAD_TO_NEXT_CACHE_LN(sizeof(uint64_t) * (2 + NUM_VTIMERS));
};
static struct vtimer vtimers[CONFIG_MAX_NUM_NODES];

#define KERNEL_PREEMPT_VTIMER 0

static void initVTimer(void)
{
    word_t core_id = CURRENT_CPU_INDEX();
    for (int i = 0; i < NUM_VTIMERS; i++) {
        vtimers[core_id].cycles[i] = VTIMER_MAX_CYCLES;
    }
    vtimers[core_id].next_cycles = VTIMER_MAX_CYCLES;
    vtimers[core_id].next = NUM_VTIMERS;
}

void setVTimer(word_t vtimer, uint64_t cycles)
{
    word_t core_id = CURRENT_CPU_INDEX();
    vtimers[core_id].cycles[vtimer] = cycles;
    if (cycles < vtimers[core_id].next_cycles) {
        vtimers[core_id].next = vtimer;
        vtimers[core_id].next_cycles = cycles;
    }

    uint64_t target = vtimers[core_id].next_cycles;
    if (vtimers[core_id].next_cycles != VTIMER_MAX_CYCLES) {
        while (get_cycles() > target) {
            target += 500;
        }
    }
    sbi_set_timer(target);

    return;
}

static irq_t handleVTimer(void)
{
    word_t core_id = CURRENT_CPU_INDEX();

    assert(vtimers[core_id].next != NUM_VTIMERS);
    irq_t irq = 0;
    if (vtimers[core_id].next == KERNEL_PREEMPT_VTIMER) {
        irq = KERNEL_TIMER_IRQ;
    } else {
        irq = KERNEL_TIMER_IRQ + core_id * CONFIG_RISCV_NUM_VTIMERS + vtimers[core_id].next;
    }
    vtimers[core_id].cycles[vtimers[core_id].next] = vtimers[core_id].next_cycles = VTIMER_MAX_CYCLES;

    /* Now need to scan for the next to set */
    for (int i = 0; i < NUM_VTIMERS; i++) {
        if (vtimers[core_id].cycles[i] < vtimers[core_id].next_cycles) {
            vtimers[core_id].next_cycles = vtimers[core_id].cycles[i];
            vtimers[core_id].next = i;
        }
    }
    if (vtimers[core_id].next_cycles == VTIMER_MAX_CYCLES) {
        /* no next timer */
        vtimers[core_id].next = NUM_VTIMERS;
    }

    uint64_t target = vtimers[core_id].next_cycles;
    if (vtimers[core_id].next_cycles != VTIMER_MAX_CYCLES) {
        while (get_cycles() > target) {
            target += 500;
        }
    }
    sbi_set_timer(target);
    return irq;
}

#endif

/*
 * The following assumes familiarity with RISC-V interrupt delivery and the PLIC.
 * See the RISC-V privileged specification v1.10 and the comment in
 * include/plat/spike/plat/machine.h for more information.
 * RISC-V IRQ handling on seL4 works as follows:
 *
 * On other architectures the kernel masks interrupts between delivering them to
 * userlevel and receiving the acknowledgment invocation. This strategy doesn't
 * work on RISC-V as an IRQ is implicitly masked when it is claimed, until the
 * claim is acknowledged. If we mask and unmask the interrupt at the PLIC while
 * a claim is in progress we sometimes experience IRQ sources not being masked
 * and unmasked as expected. Because of this, we don't mask and unmask IRQs that
 * are for user level, and also call plic_complete_claim for seL4_IRQHandler_Ack.
 */

static irq_t active_irq[CONFIG_MAX_NUM_NODES];


/**
 * Gets the active irq. Returns the same irq if called again before ackInterrupt.
 *
 * This function is called by the kernel to get the interrupt that is currently
 * active. It not interrupt is currently active, it will try to find one and
 * put it in the active state. If no interrupt is found, irqInvalid is returned.
 * It can't be assumed that if isIRQPending() returned true, there will always
 * be an active interrupt then this is called. It may hold in mayn cases, but
 * there are corner cases with level-triggered interrupts or on multicore
 * systems.
 * This function can be called multiple times during one kernel entry. It must
 * guarantee that once one interrupt is reported as active, this interrupt is
 * always returned until ackInterrupt() is called eventually.
 *
 * @return     The active irq or irqInvalid.
 */
static inline irq_t getActiveIRQ(void)
{
    irq_t *active_irq_slot = &active_irq[CURRENT_CPU_INDEX()];

    /* If an interrupt is currently active, then return it. */
    irq_t irq = *active_irq_slot;
    if (IS_IRQ_VALID(irq)) {
        return irq;
    }

    /* No interrupt currently active, find a new one from the sources. */
    word_t sip = read_sip();
    if (sip & BIT(SIP_STIP)) {
#if CONFIG_RISCV_NUM_VTIMERS > 0
        irq = handleVTimer();
#else
        irq = KERNEL_TIMER_IRQ;
#endif
#ifdef ENABLE_SMP_SUPPORT
    } else if (sip & BIT(SIP_SSIP)) {
        irq = ipi_get_irq();
#endif
    } else if (sip & BIT(SIP_SEIP)) {
        /* Even if we say an external interrupt is pending, the PLIC may not
         * return any pending interrupt here in some corner cases. A level
         * triggered interrupt might have been deasserted again or another hard
         * has claimed it in a multicore system.
         */
        irq = plic_get_claim();
        // unmatched writes to claim are ignored by hardware
        plic_complete_claim(irq);
    } else {
        /* Seems none of the known sources has a pending interrupt. This can
         * happen if e.g. if another hart context has claimed the interrupt
         * already.
         */
        irq = irqInvalid;
    }

    /* There is no guarantee that there is a new interrupt. */
    if (!IS_IRQ_VALID(irq)) {
        /* Sanity check: the slot can't hold an interrupt either. */
        assert(!IS_IRQ_VALID(*active_irq_slot));
        return irqInvalid;
    }

    /* A new interrupt is active, remember it. */
    *active_irq_slot = irq;
    return irq;
}

#ifdef HAVE_SET_TRIGGER
/**
 * Sets the irq trigger.
 *
 * setIRQTrigger can change the trigger between edge and level at the PLIC for
 * external interrupts. It is implementation specific as whether the PLIC has
 * support for this operation.
 *
 * @param[in]  irq             The irq
 * @param[in]  edge_triggered  edge triggered otherwise level triggered
 */
void setIRQTrigger(irq_t irq, bool_t edge_triggered)
{
    plic_irq_set_trigger(irq, edge_triggered);
}
#endif

/* isIRQPending is used to determine whether to preempt long running
 * operations at various preemption points throughout the kernel. If this
 * returns true, it means that if the Kernel were to return to user mode, it
 * would then immediately take an interrupt. We check the SIP register for if
 * either a timer interrupt (STIP) or an external interrupt (SEIP) is pending.
 * We don't check software generated interrupts. These are used to perform cross
 * core signalling which isn't currently supported.
 * TODO: Add SSIP check when SMP support is added.
 */
static inline bool_t isIRQPending(void)
{
    word_t sip = read_sip();
    return (sip & (BIT(SIP_SSIP) | BIT(SIP_STIP) | BIT(SIP_SEIP)));
}

/**
 * Disable or enable IRQs.
 *
 * maskInterrupt disables and enables IRQs. When an IRQ is disabled, it should
 * not raise an interrupt on the Kernel's HART context. This either masks the
 * core timer on the sie register or masks an external IRQ at the plic.
 *
 * @param[in]  disable  The disable
 * @param[in]  irq      The irq
 */
static inline void maskInterrupt(bool_t disable, irq_t irq)
{
    assert(IS_IRQ_VALID(irq));
#if CONFIG_RISCV_NUM_VTIMERS > 0
    if (irq >= INTERRUPT_VTIMER_START && irq <= INTERRUPT_VTIMER_END) return;
#endif

    if (irq == KERNEL_TIMER_IRQ) {
        if (disable) {
            clear_sie_mask(BIT(SIE_STIE));
        } else {
            set_sie_mask(BIT(SIE_STIE));
        }
#ifdef ENABLE_SMP_SUPPORT
    } else if (irq == irq_reschedule_ipi || irq == irq_remote_call_ipi) {
        return;
#endif
    } else {
        plic_mask_irq(disable, irq);
    }
}

/**
 * Kernel has dealt with the pending interrupt getActiveIRQ can return next IRQ.
 *
 * ackInterrupt is used by the kernel to indicate it has processed the interrupt
 * delivery and getActiveIRQ is now able to return a different IRQ number. Note
 * that this is called after a notification has been signalled to user level,
 * but before user level has handled the cause.
 *
 * @param[in]  irq   The irq
 */
static inline void ackInterrupt(irq_t irq)
{
    assert(IS_IRQ_VALID(irq));
    active_irq[CURRENT_CPU_INDEX()] = irqInvalid;

    if (irq == KERNEL_TIMER_IRQ) {
        /* Reprogramming the timer has cleared the interrupt. */
        return;
    }
#ifdef ENABLE_SMP_SUPPORT
    if (irq == irq_reschedule_ipi || irq == irq_remote_call_ipi) {
        ipi_clear_irq(irq);
        sbi_clear_ipi();
    }
#endif
}

static inline int read_current_timer(unsigned long *timer_val)
{
    *timer_val = riscv_read_time();
    return 0;
}

#ifndef CONFIG_KERNEL_MCS
void resetTimer(void)
{
    uint64_t target = get_cycles() + RESET_CYCLES;
    // repeatedly try and set the timer in a loop as otherwise there is a race and we
    // may set a timeout in the past, resulting in it never getting triggered
#if CONFIG_RISCV_NUM_VTIMERS > 0
        setVTimer(KERNEL_PREEMPT_VTIMER, target);
#else
    do {
        target = get_cycles() + RESET_CYCLES;
        sbi_set_timer(target);
    } while (get_cycles() > target);
#endif
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void initTimer(void)
{
#if CONFIG_RISCV_NUM_VTIMERS > 0
    initVTimer();
    setVTimer(KERNEL_PREEMPT_VTIMER, get_cycles() + RESET_CYCLES);
#else
    sbi_set_timer(get_cycles() + RESET_CYCLES);
#endif
}
#endif /* !CONFIG_KERNEL_MCS */

BOOT_CODE void initLocalIRQController(void)
{
    printf("Init local IRQ\n");

    /* Init per-hart PLIC */
    plic_init_hart();

    /* Enable timer and external interrupt. If SMP is enabled, then enable the
     * software interrupt also, it is used as IPI between cores. */
    set_sie_mask(BIT(SIE_SEIE) | BIT(SIE_STIE) | SMP_TERNARY(BIT(SIE_SSIE), 0));
}

BOOT_CODE void initIRQController(void)
{
    printf("Initializing PLIC...\n");

    /* Initialize active_irq[] properly to stick to the semantics and play safe.
     * Effectively this is not needed if irqInvalid is zero (which is currently
     * the case) and the array is in the BSS, that is filled with zeros (which
     * the a kernel loader is supposed to do and which the ELF-Loader does).
     */
    for (word_t i = 0; i < ARRAY_SIZE(active_irq); i++) {
        active_irq[i] = irqInvalid;
    }

    plic_init_controller();
}

static inline void handleSpuriousIRQ(void)
{
    /* Do nothing */
    printf("Superior IRQ!! SIP %lx\n", read_sip());
}
