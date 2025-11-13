#include <config.h>

#ifdef CONFIG_ALLOW_SBI_CALLS

#include <arch/object/sbi.h>

// TODO: duplicated because I don't know the right inlcude for libsel4 types
// TODO: maybe just make this an internal type
typedef struct seL4_RISCV_SBIRet_ {
    seL4_Word error;
    seL4_Word value;
} seL4_RISCV_SBIRet;

compile_assert(n_msgRegisters_less_than_sbi_regs, n_msgRegisters <= NUM_SBI_REGS);

static inline seL4_RISCV_SBIRet doSBICall(word_t args[NUM_SBI_REGS])
{
    register seL4_Word a0 asm("a0") = args[0];
    register seL4_Word a1 asm("a1") = args[1];
    register seL4_Word a2 asm("a2") = args[2];
    register seL4_Word a3 asm("a3") = args[3];
    register seL4_Word a4 asm("a4") = args[4];
    register seL4_Word a5 asm("a5") = args[5];
    register seL4_Word a6 asm("a6") = args[6];
    register seL4_Word a7 asm("a7") = args[7];

    asm volatile("ecall"
                 : "+r"(a0), "+r"(a1)
                 : "r"(a0), "r"(a1), "r"(a2), "r"(a3), "r"(a4), "r"(a5), "r"(a6), "r"(a7)
                 : "memory");

    seL4_RISCV_SBIRet ret;
    ret.error = a0;
    ret.value = a1;

    return ret;
}

static inline exception_t invokeSBICall(word_t *buffer, bool_t call)
{
    word_t i;
    seL4_Word args[NUM_SBI_REGS];
    word_t *ipcBuffer;
    for (i = 0; i < NUM_SBI_REGS; i++) {
        args[i] = getSyscallArg(i, buffer);
    }

    ipcBuffer = lookupIPCBuffer(true, NODE_STATE(ksCurThread));

    seL4_RISCV_SBIRet ret = doSBICall(args);

    if (call) {
        setMR(NODE_STATE(ksCurThread), ipcBuffer, 0, ret.error);
        setMR(NODE_STATE(ksCurThread), ipcBuffer, 1, ret.value);

        setRegister(NODE_STATE(ksCurThread), badgeRegister, 0);
        setRegister(NODE_STATE(ksCurThread), msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, 2)));
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Running);
    return EXCEPTION_NONE;
}


exception_t decodeRISCVSBIInvocation(word_t label, unsigned int length, cptr_t cptr,
                                   cte_t *srcSlot, cap_t cap, bool_t call, word_t *buffer)
{
    if (label != RISCVSBICall) {
        userError("RISCVSBIInvocation: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < NUM_SBI_REGS) {
        userError("RISCVSBICall: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    word_t eid_badge = cap_sbi_cap_get_capSBIEIDBadge(cap);
    word_t fid_badge = cap_sbi_cap_get_capSBIFIDBadge(cap);
    word_t eid = getSyscallArg(7, buffer);
    word_t fid = getSyscallArg(6, buffer);
    if (eid_badge != 0 || fid_badge != 0) {
        if (eid != eid_badge) {
            userError("RISCVSBICall: Illegal operation, invalid EID given (0x%lx), only EID 0x%lx is allowed.", eid, eid_badge);
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (fid != fid_badge) {
            userError("RISCVSBICall: Illegal operation, invalid FID given (0x%lx), only FID 0x%lx is allowed.", fid, fid_badge);
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSBICall(buffer, call);
}

#endif /* CONFIG_ALLOW_SBI_CALLS */
