/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 * 
 * Derived from:
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
#include <arch/machine/cache.h>
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

BOOT_CODE void setup_pw(void)
{
    csr_writeq(INIT_CSR_PWCL, LOONGARCH_CSR_PWCL);
    csr_writeq(INIT_CSR_PWCH, LOONGARCH_CSR_PWCH);
}

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
 * active. If no interrupt is currently active, it will try to find one and
 * put it in the active state. If no interrupt is found, irqInvalid is returned.
 * It can't be assumed that if isIRQPending() returned true, there will always
 * be an active interrupt when this is called. It may hold in many cases, but
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

    /* No interrupt currently active, find a new one from the sources. The
     * priorities are: IPI > timer > PMI > HWI7~0 > SWI1~0.
     */
    word_t estat = (unsigned long)read_csr_estat();
    
#ifdef ENABLE_SMP_SUPPORT
    if(estat & BIT(CSR_ESTAT_IS_IPI)){
        //TODO
    }else if(estat & BIT(CSR_ESTAT_IS_TIMER)){
#else
    if(estat & BIT(CSR_ESTAT_IS_TIMER)){
#endif
        irq = KERNEL_TIMER_IRQ;
    }else if(estat & BIT(CSR_ESTAT_IS_PMC)){
        irq = KERNEL_PMC_IRQ;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI7)){
        irq = HW_IRQ7;
    }else if (estat & BIT(CSR_ESTAT_IS_HWI6)){
        irq = HW_IRQ6;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI5)){
        irq = HW_IRQ5;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI4)){
        irq = HW_IRQ4;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI3)){
        irq = HW_IRQ3;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI2)){
        irq = HW_IRQ2;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI1)){
        irq = HW_IRQ1;
    }else if(estat & BIT(CSR_ESTAT_IS_HWI0)){
        irq = HW_IRQ0;
    }else if(estat & BIT(CSR_ESTAT_IS_SWI1)){
        irq = KERNEL_SW_IRQ1;
    }else if(estat & BIT(CSR_ESTAT_IS_SWI0)){
        irq = KERNEL_SW_IRQ0;
    }else{
        // Seems none of the known sources has a pending interrupt.  
        irq=irqInvalid;
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
 * would then immediately take an interrupt. We check the estat register for if
 * either a timer interrupt (TI) or external interrupts (HWI0~7) is pending.
 * We don't check software generated interrupts. These are used to perform cross
 * core signalling which isn't currently supported.
 * TODO: Add IPI check when SMP support is added.
 */
static inline bool_t isIRQPending(void)
{
    word_t word_estat = (unsigned long)read_csr_estat();
    return (word_estat & (BIT(CSR_ESTAT_IS_TIMER)|BIT(CSR_ESTAT_IS_HWI0)|BIT(CSR_ESTAT_IS_HWI1)|BIT(CSR_ESTAT_IS_HWI2)|
                        BIT(CSR_ESTAT_IS_HWI3)|BIT(CSR_ESTAT_IS_HWI4)|BIT(CSR_ESTAT_IS_HWI5)|
                        BIT(CSR_ESTAT_IS_HWI6)|BIT(CSR_ESTAT_IS_HWI7)));
    //word_t sip = read_sip();
    //return (sip & (BIT(SIP_STIP) | BIT(SIP_SEIP)));
}

/**
 * Disable or enable IRQs.
 *
 * maskInterrupt disables and enables IRQs. When an IRQ is disabled, it should
 * not raise an interrupt on the Kernel's HART context. See LOONGARCH_CSR_ECFG
 * register for more information
 *
 * @param[in]  disable  The disable
 * @param[in]  irq      The irq
 */
static inline void maskInterrupt(bool_t disable, irq_t irq)
{
    assert(IS_IRQ_VALID(irq));
    if (irq == KERNEL_TIMER_IRQ){
        if(disable){
            clear_csr_ecfg(BIT(irq));
        }else{
            set_csr_ecfg(BIT(irq));
        }
#ifdef ENABLE_SMP_SUPPORT
    }else if(irq==INTERRUPT_IPI){
        return;
#endif
    }else {
        extio_mask_irq(disable,irq);
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
    if (irq == INTERRUPT_IPI) {
        ipi_clear_irq(irq);
    }
#endif
}

#ifndef CONFIG_KERNEL_MCS
void resetTimer(void)
{
    /* ack */
    w_csr_ticlr(CSR_TINTCLR_TI);
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void initTimer(void)
{
    unsigned long tcfg = RESET_CYCLES | CSR_TCFG_EN | CSR_TCFG_PERIOD;
    w_csr_tcfg(tcfg);
}
#endif /* !CONFIG_KERNEL_MCS */

BOOT_CODE void initLocalIRQController(void)
{
    printf("Initializing local IRQ and extend io interrupt controller...\n");

    /* Currently only enabled timer Interrupt */
    set_csr_ecfg(BIT(ECFG_TIMER));

    /* map extend io interrupt to HW1 of node 0, core 0.*/
    extioi_init_hart();

    /* clear the interrupt stat , clear timer seperately*/
    write_csr_estat(0x0);
    w_csr_ticlr(CSR_TINTCLR_TI);
}

BOOT_CODE void initIRQController(void)
{
    printf("Initializing loongson 7A1000 interrupt controller...\n");

    /* Initialize active_irq[] properly to stick to the semantics and play safe.
     * Effectively this is not needed if irqInvalid is zero (which is currently
     * the case) and the array is in the BSS, that is filled with zeros (which
     * the a kernel loader is supposed to do and which the ELF-Loader does).
     */
    for (word_t i = 0; i < ARRAY_SIZE(active_irq); i++) {
        active_irq[i] = irqInvalid;
    }

    ls7a_intc_init();
}

static inline void handleSpuriousIRQ(void)
{
    /* Do nothing */
    printf("Superior IRQ!! csr_estat: %u\n", read_csr_estat());
}
