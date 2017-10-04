/*
 * Copyright 2017, DornerWorks
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_DORNERWORKS_GPL)
 */
/*
 * This data was produced by DornerWorks, Ltd. of Grand Rapids, MI, USA under
 * a DARPA SBIR, Contract Number D16PC00107.
 *
 * Approved for Public Release, Distribution Unlimited.
 */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices.h>

#define XUARTPS_SR             0x2C
#define XUARTPS_FIFO           0x30

#define XUARTPS_SR_TXEMPTY     BIT(3)

#define UART_REG(x) ((volatile uint32_t *)(UART_PPTR + (x)))

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void
putDebugChar(unsigned char c)
{
    while (!(*UART_REG(XUARTPS_SR) & XUARTPS_SR_TXEMPTY));
    *UART_REG(XUARTPS_FIFO) = c;
}
#endif

#ifdef CONFIG_DEBUG_BUILD
unsigned char
getDebugChar(void)
{
    while (!(*UART_REG(XUARTPS_SR) & BIT(XUARTPS_SR_TXEMPTY)));
    return *UART_REG(XUARTPS_FIFO);
}
#endif /* CONFIG_DEBUG_BUILD */
