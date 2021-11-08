/*
 * Copyright 2021, DornerWorks Ltd.
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#include <config.h>

#ifdef CONFIG_ALLOW_SMC_CALLS
#include <arch/object/smc.h>

exception_t decodeARMSMCInvocation(word_t label, unsigned int length, cptr_t cptr,
                                   cte_t *srcSlot, cap_t cap, bool_t call, word_t *buffer)
{
    word_t i;
    seL4_Word arg[NUM_SMC_REGS];
    word_t *ipcBuffer;

    switch (label) {
    case ARMSMCCall:
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

        for (i = 0; i < n_msgRegisters; i++) {
            setRegister(NODE_STATE(ksCurThread), msgRegisters[i], arg[i]);
        }

        if (ipcBuffer != NULL && i < NUM_SMC_REGS) {
            for (; i < NUM_SMC_REGS; i++) {
                ipcBuffer[i + 1] = arg[i];
            }
        }

        setRegister(NODE_STATE(ksCurThread), msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, i)));

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return EXCEPTION_NONE;

    default:
        userError("ARMSMCInvocation: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

#endif
