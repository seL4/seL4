/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/kernel/boot_sys.h>
#include <arch/kernel/lock.h>
#include <arch/model/statedata.h>
#include <plat/machine/io.h>

#ifdef DEBUG

static uint16_t get_console_port(void)
{
    if (in_boot_phase()) {
        return console_port_of_node(node_of_cpu(cur_cpu_id()));
    } else {
        return ia32KSconsolePort;
    }
}

void serial_init(uint16_t port)
{
    while (!(in8(port + 5) & 0x60)); /* wait until not busy */

    out8(port + 1, 0x00); /* disable generating interrupts */
    out8(port + 3, 0x80); /* line control register: command: set divisor */
    out8(port,     0x01); /* set low byte of divisor to 0x01 = 115200 baud */
    out8(port + 1, 0x00); /* set high byte of divisor to 0x00 */
    out8(port + 3, 0x03); /* line control register: set 8 bit, no parity, 1 stop bit */
    out8(port + 4, 0x0b); /* modem control register: set DTR/RTS/OUT2 */

    in8(port);     /* clear recevier port */
    in8(port + 5); /* clear line status port */
    in8(port + 6); /* clear modem status port */
}

void console_putchar(char c)
{
    uint16_t port = get_console_port();

    lock_acquire(&lock_debug);

    if (port > 0) {
        while (!(in8(port + 5) & 0x60));
        out8(port, c);
        if (c == '\n') {
            while (!(in8(port + 5) & 0x60));
            out8(port, '\r');
        }
    }

    lock_release(&lock_debug);
}

#endif
