/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#ifdef CONFIG_IRQ_IOAPIC

#include <arch/linker.h>
#include <plat/machine/io.h>
#include <plat/machine/hardware.h>
#include <plat/machine/ioapic.h>

#define IOAPIC_REGSEL 0x00
#define IOAPIC_WINDOW 0x10

#define IOAPIC_REG_IOAPICID 0x00
#define IOAPIC_REG_IOREDTBL 0x10

#define IOREDTBL_LOW(reg) (IOAPIC_REG_IOREDTBL + (reg) * 2)
#define IOREDTBL_HIGH(reg) (IOREDTBL_LOW(reg) + 1)

#define IOREDTBL_LOW_INTERRUPT_MASK BIT(16)
#define IOREDTBL_LOW_TRIGGER_MODE_LEVEL BIT(15)
#define IOREDTBL_LOW_POLARITY_LOW BIT(13)
#define IOREDTBL_LOW_DEST_MODE_LOGCIAL BIT(11)

#define IOAPICID_ID_BITS 4
#define IOAPICID_ID_OFFSET 24

#define IOREDTBL_HIGH_RESERVED_BITS 24

/* Cache what we believe is in the low word of the IOREDTBL. This
 * has all the state of trigger modes etc etc */
static uint32_t ioredtbl_state[IOAPIC_IRQ_LINES * CONFIG_MAX_NUM_IOAPIC];

/* Number of IOAPICs in the system */
static uint32_t num_ioapics = 0;

/* In debug mode we track whether an unmasked vector has
 * had its mode set. This is to catch bad user level code */
#if defined DEBUG || defined RELEASE_PRINTF
static bool_t done_set_mode[IOAPIC_IRQ_LINES * CONFIG_MAX_NUM_IOAPIC] = { 0 };
#endif

static void ioapic_write(uint32_t ioapic, uint32_t reg, uint32_t value)
{
    *(volatile uint32_t*)((uint32_t)(PPTR_IOAPIC_START + ioapic * BIT(PAGE_BITS)) + reg) = value;
}

static uint32_t ioapic_read(uint32_t ioapic, uint32_t reg)
{
    return *(volatile uint32_t*)((uint32_t)(PPTR_IOAPIC_START + ioapic * BIT(PAGE_BITS)) + reg);
}

static bool_t in_list(uint32_t size, cpu_id_t *list, cpu_id_t target)
{
    uint32_t i;
    for (i = 0; i < size; i++) {
        if (list[i] == target) {
            return true;
        }
    }
    return false;
}

static void single_ioapic_init(uint32_t ioapic, cpu_id_t ioapic_id, cpu_id_t delivery_cpu)
{
    uint32_t id_reg;
    uint32_t i;
    /* Write the ID to the ioapic */
    ioapic_write(ioapic, IOAPIC_REGSEL, IOAPIC_REG_IOAPICID);
    id_reg = ioapic_read(ioapic, IOAPIC_WINDOW);
    /* perform mask to preserve the reserved bits */
    id_reg &= ~(MASK(IOAPICID_ID_BITS) << IOAPICID_ID_OFFSET);
    id_reg |= ioapic_id << IOAPICID_ID_OFFSET;
    /* Mask all the IRQs and set default delivery details.
     * attempt to deliberately set a trigger mode and level
     * setting that is LEAST likely to be correct. This is
     * to ensure user code sets it correctly and cannot get
     * away with it happening to be correct */
    for (i = 0; i < IOAPIC_IRQ_LINES; i++) {
        /* Send to desired cpu */
        ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_HIGH(i));
        ioapic_write(ioapic, IOAPIC_WINDOW, (ioapic_read(ioapic, IOAPIC_WINDOW) & MASK(IOREDTBL_HIGH_RESERVED_BITS)) | (delivery_cpu << IOREDTBL_HIGH_RESERVED_BITS));
        /* Mask and set to level trigger high polarity and make the delivery vector */
        ioredtbl_state[i] = IOREDTBL_LOW_INTERRUPT_MASK |
                            IOREDTBL_LOW_TRIGGER_MODE_LEVEL |
                            (i + IRQ_INT_OFFSET);
        ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_LOW(i));
        /* The upper 16 bits are reserved, so we make sure to preserve them */
        ioredtbl_state[i] |= ioapic_read(ioapic, IOAPIC_WINDOW) & ~MASK(16);
        ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_state[i]);
    }
}

/* To guarantee we will be able to find enough free apic ids there needs to be less than
 * 2^4 cpus + ioapics in the system */
compile_assert(ioapic_id_will_not_overflow, CONFIG_MAX_NUM_NODES + CONFIG_MAX_NUM_IOAPIC < 16);

void ioapic_init(uint32_t num_nodes, cpu_id_t *cpu_list, uint32_t num_ioapic)
{
    uint32_t ioapic;
    cpu_id_t ioapic_id = 0;
    num_ioapics = num_ioapic;
    for (ioapic = 0; ioapic < num_ioapic; ioapic++) {
        /* Determine the next free apic ID */
        while (in_list(num_nodes, cpu_list, ioapic_id)) {
            ioapic_id++;
        }
        /* ioapic id field is 4 bits. this assert passing should be
         * guaranteed by the compile assert above this function, hence
         * this does not need to be a run time check */
        assert(ioapic_id < BIT(4));
        /* Init this ioapic */
        single_ioapic_init(ioapic, ioapic_id, cpu_list[0]);
        /* Increment the id */
        ioapic_id++;
    }
}

void ioapic_mask_irq(bool_t mask, irq_t irq)
{
    uint32_t ioapic = irq / IOAPIC_IRQ_LINES;
    uint32_t index = irq % IOAPIC_IRQ_LINES;
    if (ioapic >= num_ioapics) {
        /* silently ignore requests to non existent parts of the interrupt space */
        return;
    }
    if (mask) {
        ioredtbl_state[irq] |= IOREDTBL_LOW_INTERRUPT_MASK;
    } else {
        ioredtbl_state[irq] &= ~IOREDTBL_LOW_INTERRUPT_MASK;
#if defined DEBUG || defined RELEASE_PRINTF
        if (!done_set_mode[irq]) {
            printf("Unmasking IOAPIC source %d on ioapic %d without ever setting its mode!\n", index, ioapic);
            /* Set the flag so we don't repeatedly warn */
            done_set_mode[irq] = 1;
        }
#endif
    }
    ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_LOW(index));
    ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_state[irq]);
}

void ioapic_set_mode(irq_t irq, bool_t levelTrigger, bool_t polarityLow)
{
    uint32_t ioapic = irq / IOAPIC_IRQ_LINES;
    uint32_t index = irq % IOAPIC_IRQ_LINES;
    if (ioapic >= num_ioapics) {
        /* silently ignore requests to non existent parts of the interrupt space */
        return;
    }
    if (levelTrigger) {
        ioredtbl_state[irq] |= IOREDTBL_LOW_TRIGGER_MODE_LEVEL;
    } else {
        ioredtbl_state[irq] &= ~IOREDTBL_LOW_TRIGGER_MODE_LEVEL;
    }
    if (polarityLow) {
        ioredtbl_state[irq] |= IOREDTBL_LOW_POLARITY_LOW;
    } else {
        ioredtbl_state[irq] &= ~IOREDTBL_LOW_POLARITY_LOW;
    }
#if defined DEBUG || defined RELEASE_PRINTF
    done_set_mode[irq] = 1;
#endif
    ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_LOW(index));
    ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_state[irq]);
}

#endif /* CONFIG_IOAPIC */
