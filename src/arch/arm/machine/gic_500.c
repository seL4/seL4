/*
 * Copyright 2019, DornerWorks
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_DORNERWORKS_BSD)
 */
#include <config.h>
#include <types.h>
#include <arch/machine/gic_500.h>

#define IRQ_SET_ALL 0xffffffff

#define RDIST_BANK_SZ 0x00010000

/* Shift positions for GICD_SGIR register */
#define GICD_SGIR_SGIINTID_SHIFT          0
#define GICD_SGIR_CPUTARGETLIST_SHIFT     16
#define GICD_SGIR_TARGETLISTFILTER_SHIFT  24
#ifndef GIC_500_DISTRIBUTOR_PPTR
#error GIC_500_DISTRIBUTOR_PPTR must be defined for virtual memory access to the gic distributer
#else  /* GIC_DISTRIBUTOR_PPTR */
volatile struct gic_dist_map *const gic_dist =
    (volatile struct gic_dist_map *)(GIC_500_DISTRIBUTOR_PPTR);
#endif /* GIC_DISTRIBUTOR_PPTR */

#ifndef GIC_500_REDIST_PPTR
#error GIC_500_REDIST_PPTR must be defined for virtual memory access to the gic redistributer
#else  /* GIC_REDIST_PPTR */
volatile struct gic_rdist_map *const gic_rdist =
    (volatile struct gic_rdist_map *)(GIC_500_REDIST_PPTR);
volatile struct gic_rdist_sgi_ppi_map *const gic_rdist_sgi_ppi =
    (volatile struct gic_rdist_sgi_ppi_map *)(GIC_500_REDIST_PPTR + RDIST_BANK_SZ);
#endif /* GIC_REDIST_PPTR */

#define GIC_DEADLINE_MS 2
#define GIC_REG_WIDTH   32

uint32_t active_irq[CONFIG_MAX_NUM_NODES] = {IRQ_NONE};

/* Wait for completion of a distributor change */
static uint32_t gicv3_do_wait_for_rwp(volatile uint32_t *ctlr_addr)
{
    uint32_t val;
    bool_t waiting = true;
    uint32_t ret = 0;

    uint32_t gpt_cnt_tval = 0;
    uint32_t deadline_ms =  GIC_DEADLINE_MS;
    uint32_t gpt_cnt_ciel;
    SYSTEM_READ_WORD(CNTFRQ, gpt_cnt_tval);
    gpt_cnt_ciel = gpt_cnt_tval + (deadline_ms * TICKS_PER_MS);

    while (waiting) {
        SYSTEM_READ_WORD(CNTFRQ, gpt_cnt_tval);
        val = *ctlr_addr;

        if (gpt_cnt_tval >= gpt_cnt_ciel) {
            printf("GICV3 RWP Timeout after %u ms\n", deadline_ms);
            ret = 1;
            waiting = false;

        } else if (!(val & GICD_CTLR_RWP)) {
            ret = 0;
            waiting = false;
        }
    }
    return ret;
}

static void gicv3_dist_wait_for_rwp(void)
{
    gicv3_do_wait_for_rwp(&gic_dist->ctlr);
}

static void gicv3_redist_wait_for_rwp(void)
{
    gicv3_do_wait_for_rwp(&gic_rdist->ctlr);
}

static void gicv3_enable_sre(void)
{
    uint32_t val = 0;

    /* ICC_SRE_EL1 */
    SYSTEM_READ_WORD(ICC_SRE_EL1, val);
    val |= GICC_SRE_EL1_SRE;

    SYSTEM_WRITE_WORD(ICC_SRE_EL1, val);
    isb();
}


BOOT_CODE static void dist_init(void)
{
    word_t i;
    uint32_t type;
    unsigned int nr_lines;
    uint64_t affinity;
    uint32_t priority;

    /* Disable GIC Distributor */
    gic_dist->ctlr = 0;
    gicv3_dist_wait_for_rwp();

    type = gic_dist->typer;

    nr_lines = GIC_REG_WIDTH * ((type & GICD_TYPE_LINESNR) + 1);

    /* Assume level-triggered */
    for (i = NR_GIC_LOCAL_IRQS; i < nr_lines; i += 16) {
        gic_dist->config[(i / 16)] = 0;
    }

    /* Default priority for global interrupts */
    priority = (GIC_PRI_IRQ << 24 | GIC_PRI_IRQ << 16 | GIC_PRI_IRQ << 8 |
                GIC_PRI_IRQ);
    for (i = NR_GIC_LOCAL_IRQS; i < nr_lines; i += 4) {
        gic_dist->priority[(i / 4)] = priority;
    }
    /* Disable and clear all global interrupts */
    for (i = NR_GIC_LOCAL_IRQS; i < nr_lines; i += 32) {
        gic_dist->enable_clr[(i / 32)] = IRQ_SET_ALL;
        gic_dist->pending_clr[(i / 32)] = IRQ_SET_ALL;
    }

    /* Turn on the distributor */
    gic_dist->ctlr = GICD_CTL_ENABLE | GICD_CTLR_ARE_NS | GICD_CTLR_ENABLE_G1NS | GICD_CTLR_ENABLE_G0;
    gicv3_dist_wait_for_rwp();

    /* Route all global IRQs to this CPU */
    SYSTEM_READ_WORD("mpidr_el1", affinity);
    /* Mask cpu affinity part */
    affinity &= 0xFFFFFF;
    /* Make sure we don't broadcast the interrupt */
    affinity &= ~GICD_IROUTER_SPI_MODE_ANY;

    for (i = NR_GIC_LOCAL_IRQS; i < nr_lines; i++) {
        gic_dist->irouter[i] = affinity;
    }
}

static int gicv3_enable_redist(void)
{
    uint32_t val;
    bool_t waiting = true;
    uint32_t ret = 0;
    uint32_t gpt_cnt_tval = 0;
    uint32_t deadline_ms = GIC_DEADLINE_MS;
    uint32_t gpt_cnt_ciel;
    SYSTEM_READ_WORD(CNTFRQ, gpt_cnt_tval);
    gpt_cnt_ciel = gpt_cnt_tval + (deadline_ms * TICKS_PER_MS);
    /* Wake up this CPU redistributor */
    val = gic_rdist->waker;
    val &= ~GICR_WAKER_ProcessorSleep;
    gic_rdist->waker = val;
    while (waiting) {
        SYSTEM_READ_WORD(CNTFRQ, gpt_cnt_tval);
        val = gic_rdist->waker;
        if (gpt_cnt_tval >= gpt_cnt_ciel) {
            printf("GICV3 Re-distributor enable Timeout after %u ms\n", deadline_ms);
            ret = 1;
            waiting = false;
        } else if (!(val & GICR_WAKER_ChildrenAsleep)) {
            ret = 0;
            waiting = false;
        }
    }
    return ret;
}

static int gicv3_populate_rdist(void)
{
    uint64_t aff;
    uint32_t reg;
    uint64_t typer;

    SYSTEM_READ_WORD("mpidr_el1", aff);
    // Mask MPIDR to just show the register that code is runnig on
    aff &= 0xFFFFFF;

    reg = gic_rdist->pidr2 & GIC_PIDR2_ARCH_MASK;
    if (reg != GIC_PIDR2_ARCH_GICv3 && reg != GIC_PIDR2_ARCH_GICv4) {
        printf("GICv3: CPU0: has no re-distributor!\n");
        return 1;
    }

    typer = gic_rdist->typer;
    /* Compare the affinity bits of GICR_TYPER to the affinity of the current processor */
    if ((typer >> 32) == aff) {
        printf("GICv3: CPU%llu: Found redistributor at addr: %p\n", aff, gic_rdist);
        return 0;
    }

    printf("GICv3: CPU%llu: has no re-distributor!\n", aff);

    return 1;
}

BOOT_CODE static void cpu_iface_init(void)
{
    int i;
    uint32_t priority;

    /* Register ourselves with the rest of the world */
    if (gicv3_populate_rdist()) {
        return;
    }

    if (gicv3_enable_redist()) {
        return;
    }

    /* Deactivate SGIs/PPIs */
    gic_rdist_sgi_ppi->icactiver0 = ~0;

    /* Set priority on PPI and SGI interrupts */
    priority = (GIC_PRI_IRQ << 24 | GIC_PRI_IRQ << 16 | GIC_PRI_IRQ << 8 |
                GIC_PRI_IRQ);
    for (i = 0; i < NR_GIC_LOCAL_IRQS; i += 4) {
        gic_rdist_sgi_ppi->ipriorityrn[i / 4] = priority;
    }

    /*
     * Disable all PPI interrupts, ensure all SGI interrupts are
     * enabled.
     */
    gic_rdist_sgi_ppi->icenabler0 = 0xffff0000;
    gic_rdist_sgi_ppi->isenabler0 = 0x0000ffff;

    /* Set ICFGR1 for PPIs as level-triggered */
    gic_rdist_sgi_ppi->icfgrn_rw = 0x0;

    gicv3_redist_wait_for_rwp();

    /* Enable system registers */
    gicv3_enable_sre();

    /* No priority grouping: ICC_BPR1_EL1 */
    SYSTEM_WRITE_WORD(ICC_BPR1_EL1, 0);

    /* Set priority mask register: ICC_PMR_EL1 */
    SYSTEM_WRITE_WORD(ICC_PMR_EL1, DEFAULT_PMR_VALUE);

    /* EOI drops priority, DIR deactivates the interrupt (mode 1): ICC_CTLR_EL1 */
    SYSTEM_WRITE_WORD(ICC_CTLR_EL1, GICC_CTLR_EL1_EOImode_drop);

    /* Enable Group1 interrupts: ICC_IGRPEN1_EL1 */
    SYSTEM_WRITE_WORD(ICC_IGRPEN1_EL1, 1);

    /* Sync at once at the end of cpu interface configuration */
    isb();
}

BOOT_CODE void initIRQController(void)
{
    dist_init();
}

BOOT_CODE void cpu_initLocalIRQController(void)
{
    cpu_iface_init();
}

#ifdef ENABLE_SMP_SUPPORT
/*
* 25-24: target lister filter
* 0b00 - send the ipi to the CPU interfaces specified in the CPU target list
* 0b01 - send the ipi to all CPU interfaces except the cpu interface.
*        that requrested teh ipi
* 0b10 - send the ipi only to the CPU interface that requested the IPI.
* 0b11 - reserved
*.
* 23-16: CPU targets list
* each bit of CPU target list [7:0] refers to the corresponding CPU interface.
* 3-0:   SGIINTID
* software generated interrupt id, from 0 to 15...
*/
void ipiBroadcast(irq_t irq, bool_t includeSelfCPU)
{
    gic_dist->sgi_control = (!includeSelfCPU << GICD_SGIR_TARGETLISTFILTER_SHIFT) | (irq << GICD_SGIR_SGIINTID_SHIFT);
}

void ipi_send_target(irq_t irq, word_t cpuTargetList)
{
    gic_dist->sgi_control = (cpuTargetList << GICD_SGIR_CPUTARGETLIST_SHIFT) | (irq << GICD_SGIR_SGIINTID_SHIFT);
}
#endif /* ENABLE_SMP_SUPPORT */
