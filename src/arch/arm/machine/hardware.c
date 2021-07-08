/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <types.h>
#include <machine/registerset.h>
#include <arch/machine.h>
#include <plat/machine/hardware.h>

word_t PURE getRestartPC(tcb_t *thread)
{
    return getRegister(thread, FaultIP);
}

void setNextPC(tcb_t *thread, word_t v)
{
    setRegister(thread, NEXT_PC_REG, v);
}

BOOT_CODE void map_kernel_devices(void)
{
    for (int i = 0; i < ARRAY_SIZE(kernel_devices); i++) {
        const kernel_frame_t *frame = &kernel_devices[i];
        /* all frames are supposed to describe device memory, so they should
         * never be marked as executable.
         */
        assert(frame->armExecuteNever);
        map_kernel_frame(frame->paddr, frame->pptr, VMKernelOnly,
                         vm_attributes_new(frame->armExecuteNever, false,
                                           false));
        if (!frame->userAvailable) {
            reserve_region((p_region_t) {
                .start = frame->paddr,
                .end   = frame->paddr + BIT(PAGE_BITS)
            });
        }
    }
}

