/*
 * Copyright 2021, DornerWorks Ltd.
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#include <config.h>

#ifdef CONFIG_ALLOW_SMC_CALLS
#include <arch/object/smc.h>

compile_assert(n_msgRegisters_less_than_smc_regs, n_msgRegisters <= NUM_SMC_REGS);

static exception_t invokeSMCCall(register_t *buffer, bool_t call)
{
    word_t i;
    seL4_Word arg[NUM_SMC_REGS];
    register_t *ipcBuffer;

    for (i = 0; i < NUM_SMC_REGS; i++) {
        arg[i] = getSyscallArg(i, buffer);
    }

    ipcBuffer = lookupIPCBuffer(true, NODE_STATE(ksCurThread));

    register seL4_Word r0 asm("x0") = arg[0];
    register seL4_Word r1 asm("x1") = arg[1];
    register seL4_Word r2 asm("x2") = arg[2];
    register seL4_Word r3 asm("x3") = arg[3];
    register seL4_Word r4 asm("x4") = arg[4];
    register seL4_Word r5 asm("x5") = arg[5];
    register seL4_Word r6 asm("x6") = arg[6];
    register seL4_Word r7 asm("x7") = arg[7];
    asm volatile("smc #0\n"
                 : "+r"(r0), "+r"(r1), "+r"(r2), "+r"(r3),
                 "+r"(r4), "+r"(r5), "+r"(r6), "+r"(r7)
                 :: "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "memory");

    arg[0] = r0;
    arg[1] = r1;
    arg[2] = r2;
    arg[3] = r3;
    arg[4] = r4;
    arg[5] = r5;
    arg[6] = r6;
    arg[7] = r7;

    if (call) {
        for (i = 0; i < n_msgRegisters; i++) {
            setRegister(NODE_STATE(ksCurThread), msgRegisters[i], arg[i]);
        }

        if (ipcBuffer != NULL) {
            for (; i < NUM_SMC_REGS; i++) {
                ipcBuffer[i + 1] = arg[i];
            }
        }

        setRegister(NODE_STATE(ksCurThread), badgeRegister, 0);
        setRegister(NODE_STATE(ksCurThread), msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, i)));
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Running);
    return EXCEPTION_NONE;
}

exception_t decodeARMSMCInvocation(word_t label, word_t length, cptr_t cptr,
                                   cte_t *srcSlot, cap_t cap, bool_t call, register_t *buffer)
{
    if (label != ARMSMCCall) {
        userError("ARMSMCInvocation: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < NUM_SMC_REGS) {
        userError("ARMSMCCall: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    word_t badge = cap_smc_cap_get_capSMCBadge(cap);
    word_t smc_func_id = getSyscallArg(0, buffer);

    if (badge != 0 && badge != smc_func_id) {
        userError("ARMSMCCall: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSMCCall(buffer, call);
}

#endif
