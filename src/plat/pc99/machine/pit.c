/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/linker.h>
#include <plat/machine/io.h>
#include <plat/machine/pit.h>

/* PIT (i8253) registers */
#define PIT_MODE 0x43
#define PIT_CH0  0x40

/* Count frequency in Hz */
#define PIT_HZ 1193180

PHYS_CODE void
pit_init(void)
{
    uint16_t divisor = (PIT_HZ * PIT_WRAPAROUND_MS) / 1000;

    out8_phys(PIT_MODE, 0x34);          /* Set mode 2 and wait for divisor bytes */
    out8_phys(PIT_CH0, divisor & 0xff); /* Set low byte of divisor */
    out8_phys(PIT_CH0, divisor >> 8);   /* Set high byte of divisor */
}

PHYS_CODE void
pit_wait_wraparound(void)
{
    uint16_t count;
    uint16_t count_old;

    out8_phys(PIT_MODE, 0x00);
    count = in8_phys(PIT_CH0);
    count |= (in8_phys(PIT_CH0) << 8);
    count_old = count;

    while (count <= count_old) {
        count_old = count;
        out8_phys(PIT_MODE, 0x00);
        count = in8_phys(PIT_CH0);
        count |= (in8_phys(PIT_CH0) << 8);
    }
}
