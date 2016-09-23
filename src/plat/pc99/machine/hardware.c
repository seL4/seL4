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
#include <machine/io.h>
#include <arch/kernel/apic.h>
#include <arch/model/statedata.h>
#include <arch/linker.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <plat/machine.h>

#include <plat/machine/intel-vtd.h>

BOOT_CODE bool_t platAddDevices(void)
{
    /* remove the MSI region as poking at this is undefined and may allow for
     * the user to generate arbitrary MSI interrupts. Only need to consider
     * this if it would actually be in the user device region */
    if (PADDR_USER_DEVICE_TOP > 0xFFFFFFF8) {
        if (!add_allocated_p_region( (p_region_t) {
        (word_t)0xFFFFFFF8, (word_t)0xFFFFFFF8 + 8
        })) {
            return false;
        }
    }
    return true;
}

/* ============================== timer ============================== */

void resetTimer(void)
{
    /* not necessary */
}
