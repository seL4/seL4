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
#define IOREDTBL_LOW_TRIGGER_MODE_SHIFT    15
#define IOREDTBL_LOW_POLARITY_LOW BIT(13)
#define IOREDTBL_LOW_POLARITY_SHIFT         13
#define IOREDTBL_LOW_DEST_MODE_LOGCIAL BIT(11)

#define IOAPICID_ID_BITS 4
#define IOAPICID_ID_OFFSET 24

#define IOREDTBL_HIGH_RESERVED_BITS 24

/* Cache what we believe is in the low word of the IOREDTBL. This
 * has all the state of trigger modes etc etc */
static uint32_t ioredtbl_state[IOAPIC_IRQ_LINES * CONFIG_MAX_NUM_IOAPIC];

/* Number of IOAPICs in the system */
static uint32_t num_ioapics = 0;

static void ioapic_write(uint32_t ioapic, word_t reg, uint32_t value)
{
    *(volatile uint32_t*)((word_t)(PPTR_IOAPIC_START + ioapic * BIT(PAGE_BITS)) + reg) = value;
}

static uint32_t ioapic_read(uint32_t ioapic, word_t reg)
{
    return *(volatile uint32_t*)((word_t)(PPTR_IOAPIC_START + ioapic * BIT(PAGE_BITS)) + reg);
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

static void single_ioapic_init(word_t ioapic, cpu_id_t ioapic_id, cpu_id_t delivery_cpu)
{
    uint32_t id_reg;
    uint32_t i;
    /* Write the ID to the ioapic */
    ioapic_write(ioapic, IOAPIC_REGSEL, IOAPIC_REG_IOAPICID);
    id_reg = ioapic_read(ioapic, IOAPIC_WINDOW);
    /* perform mask to preserve the reserved bits */
    id_reg &= ~(MASK(IOAPICID_ID_BITS) << IOAPICID_ID_OFFSET);
    id_reg |= ioapic_id << IOAPICID_ID_OFFSET;
    /* Mask all the IRQs. In doing so we happen to set
     * the vector to 0, which we can assert against in
     * mask_interrupt to ensure a vector is assigned
     * before we unmask */
    for (i = 0; i < IOAPIC_IRQ_LINES; i++) {
        /* Send to desired cpu */
        ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_HIGH(i));
        ioapic_write(ioapic, IOAPIC_WINDOW, (ioapic_read(ioapic, IOAPIC_WINDOW) & MASK(IOREDTBL_HIGH_RESERVED_BITS)) | (delivery_cpu << IOREDTBL_HIGH_RESERVED_BITS));
        /* mask and set 0 vector */
        ioredtbl_state[i] = IOREDTBL_LOW_INTERRUPT_MASK;
        ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_LOW(i));
        /* The upper 16 bits are reserved, so we make sure to preserve them */
        ioredtbl_state[i] |= ioapic_read(ioapic, IOAPIC_WINDOW) & ~MASK(16);
        ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_state[i]);
    }
}

/* To guarantee we will be able to find enough free apic ids there needs to be less than
 * 2^4 cpus + ioapics in the system */
compile_assert(ioapic_id_will_not_overflow, 1 + CONFIG_MAX_NUM_IOAPIC < 16)

static  cpu_id_t ioapic_target_cpu = 0;
void ioapic_init(uint32_t num_nodes, cpu_id_t *cpu_list, uint32_t num_ioapic)
{
    uint32_t ioapic;
    cpu_id_t ioapic_id = 0;
    num_ioapics = num_ioapic;
    ioapic_target_cpu = cpu_list[0];

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

void ioapic_mask(bool_t mask, uint32_t ioapic, uint32_t pin)
{
    int index = ioapic * IOAPIC_IRQ_LINES + pin;
    if (ioapic >= num_ioapics || pin >= IOAPIC_IRQ_LINES) {
        /* silently ignore requests to non existent parts of the interrupt space */
        return;
    }
    if (mask) {
        ioredtbl_state[index] |= IOREDTBL_LOW_INTERRUPT_MASK;
    } else {
        ioredtbl_state[index] &= ~IOREDTBL_LOW_INTERRUPT_MASK;
        /* it should not be possible to be unmasking an interrupt, without
         * it having been mapped to a vector, assert that this is the case */
        assert((ioredtbl_state[index] & 0xff) != 0);
    }
    ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_LOW(pin));
    ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_state[index]);
}

exception_t ioapic_decode_map_pin_to_vector(word_t ioapic, word_t pin, word_t level,
                                            word_t polarity, word_t vector)
{
    if (ioapic >= num_ioapics) {
        userError("Invalid IOAPIC %ld, only have %ld", (long)ioapic, (long)num_ioapics);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = num_ioapics - 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (pin >= IOAPIC_IRQ_LINES) {
        userError("Invalid IOAPIC pin %ld, there are %d pins", (long)pin, IOAPIC_IRQ_LINES);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = IOAPIC_IRQ_LINES - 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (level != 0 && level != 1) {
        userError("Level should be 0 or 1, not %d", (int)level);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (polarity != 0 && polarity != 1) {
        userError("Polarity should be 0 or 1, not %d", (int)polarity);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    return EXCEPTION_NONE;
}

void ioapic_map_pin_to_vector(word_t ioapic, word_t pin, word_t level,
                              word_t polarity, word_t vector)
{
    uint32_t ioredtbl_high = 0;
    uint32_t index = 0;

    index = ioapic * IOAPIC_IRQ_LINES + pin;
    ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_HIGH(pin));
    ioredtbl_high = ioapic_read(ioapic, IOAPIC_WINDOW) & MASK(IOREDTBL_HIGH_RESERVED_BITS);
    /* delivery mode: physical mode only, using APIC ID */
    ioredtbl_high |= (ioapic_target_cpu << IOREDTBL_HIGH_RESERVED_BITS);
    ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_high);
    /* we do not need to add IRQ_INT_OFFSET to the vector here */
    ioredtbl_state[index] = IOREDTBL_LOW_INTERRUPT_MASK |
                            (level << IOREDTBL_LOW_TRIGGER_MODE_SHIFT) |
                            (polarity << IOREDTBL_LOW_POLARITY_SHIFT) |
                            vector;

    ioapic_write(ioapic, IOAPIC_REGSEL, IOREDTBL_LOW(pin));
    /* the upper 16 bits are reserved */
    ioredtbl_state[index] |= ioapic_read(ioapic, IOAPIC_WINDOW) & ~MASK(16);
    ioapic_write(ioapic, IOAPIC_WINDOW, ioredtbl_state[index]);
}
