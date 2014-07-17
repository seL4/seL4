/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <api/failures.h>
#include <api/syscall.h>
#include <kernel/thread.h>
#include <arch/object/ipi.h>
#include <arch/kernel/apic.h>
#include <arch/api/invocation.h>

exception_t
decodeIA32IPIInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    node_id_t node_id;
    irq_t     irq;

    if (label != IA32IPISend) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 1) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    node_id = getSyscallArg(0, buffer) & 0xff;
    irq = (getSyscallArg(0, buffer) >> 8) & 0xff;

    if (node_id >= ia32KSNumNodes || irq < irq_ipi_min || irq > irq_ipi_max) {
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* send the IPI */
    apic_send_ipi(ia32KSCPUList[node_id], irq + IRQ_INT_OFFSET);

    /* setup reply message */
    setRegister(ksCurThread, badgeRegister, 0);
    setRegister(ksCurThread, msgInfoRegister,
                wordFromMessageInfo(message_info_new(0, 0, 0, 0)));
    setThreadState(ksCurThread, ThreadState_Restart);

    return EXCEPTION_NONE;
}
