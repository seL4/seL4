/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <machine/registerset.h>
#include <arch/machine.h>
#include <plat/machine/hardware.h>

word_t PURE
getRestartPC(tcb_t *thread)
{
    return getRegister(thread, FaultInstruction);
}

void
setNextPC(tcb_t *thread, word_t v)
{
    setRegister(thread, LR_svc, v);
}

BOOT_CODE int
get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE int
get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_dev_p_reg(word_t i)
{
    return dev_p_regs[i];
}

BOOT_CODE p_region_t get_avail_p_reg(word_t i)
{
    return avail_p_regs[i];
}

BOOT_CODE void
map_kernel_devices(void)
{
    for (int i = 0; i < ARRAY_SIZE(kernel_devices); i++) {
        map_kernel_frame(kernel_devices[i].paddr,
                         kernel_devices[i].pptr,
                         VMKernelOnly,
                         vm_attributes_new(kernel_devices[i].armExecuteNever,
                                           false, false));
    }
}

