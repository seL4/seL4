/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <benchmark/benchmark.h>
#include <arch/benchmark.h>
#include <benchmark/benchmark_track.h>
#include <benchmark/benchmark_utilisation.h>
#include <api/syscall.h>
#include <api/failures.h>
#include <api/faults.h>
#include <kernel/cspace.h>
#include <kernel/faulthandler.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <machine/io.h>
#include <plat/machine/hardware.h>
#include <object/interrupt.h>
#include <model/statedata.h>
#include <string.h>
#include <kernel/traps.h>
#include <arch/machine.h>

#ifdef CONFIG_DEBUG_BUILD
#include <arch/machine/capdl.h>
#endif

/* The haskell function 'handleEvent' is split into 'handleXXX' variants
 * for each event causing a kernel entry */

exception_t handleInterruptEntry(void)
{
    irq_t irq;

    irq = getActiveIRQ();
#ifdef CONFIG_KERNEL_MCS
    if (SMP_TERNARY(clh_is_self_in_queue(), 1)) {
        updateTimestamp();
        checkBudget();
    }
#endif

    if (IRQT_TO_IRQ(irq) != IRQT_TO_IRQ(irqInvalid)) {
        handleInterrupt(irq);
        Arch_finaliseInterrupt();
    } else {
#ifdef CONFIG_IRQ_REPORTING
        userError("Spurious interrupt!");
#endif
        handleSpuriousIRQ();
    }

#ifdef CONFIG_KERNEL_MCS
    if (SMP_TERNARY(clh_is_self_in_queue(), 1)) {
#endif
        schedule();
        activateThread();
#ifdef CONFIG_KERNEL_MCS
    }
#endif

    return EXCEPTION_NONE;
}

exception_t handleUnknownSyscall(word_t w)
{
#ifdef CONFIG_PRINTING
    if (w == SysDebugPutChar) {
        kernel_putchar(getRegister(NODE_STATE(ksCurThread), capRegister));
        return EXCEPTION_NONE;
    }
    if (w == SysDebugDumpScheduler) {
#ifdef CONFIG_DEBUG_BUILD
        debug_dumpScheduler();
#endif
        return EXCEPTION_NONE;
    }
#endif
#ifdef CONFIG_DEBUG_BUILD
    if (w == SysDebugHalt) {
        tcb_t *UNUSED tptr = NODE_STATE(ksCurThread);
        printf("Debug halt syscall from user thread %p \"%s\"\n", tptr, TCB_PTR_DEBUG_PTR(tptr)->tcbName);
        halt();
    }
    if (w == SysDebugSnapshot) {
#if defined CONFIG_ARCH_ARM || defined CONFIG_ARCH_X86_64 || defined CONFIG_ARCH_RISCV32
        tcb_t *UNUSED tptr = NODE_STATE(ksCurThread);
        printf("Debug snapshot syscall from user thread %p \"%s\"\n", tptr, TCB_PTR_DEBUG_PTR(tptr)->tcbName);
        capDL();
#else
        printf("Debug snapshot syscall not supported for the current arch\n");
#endif
        return EXCEPTION_NONE;
    }
    if (w == SysDebugCapIdentify) {
        word_t cptr = getRegister(NODE_STATE(ksCurThread), capRegister);
        lookupCapAndSlot_ret_t lu_ret = lookupCapAndSlot(NODE_STATE(ksCurThread), cptr);
        word_t cap_type = cap_get_capType(lu_ret.cap);
        setRegister(NODE_STATE(ksCurThread), capRegister, cap_type);
        return EXCEPTION_NONE;
    }

    if (w == SysDebugNameThread) {
        /* This is a syscall meant to aid debugging, so if anything goes wrong
         * then assume the system is completely misconfigured and halt */
        const char *name;
        word_t len;
        word_t cptr = getRegister(NODE_STATE(ksCurThread), capRegister);
        lookupCapAndSlot_ret_t lu_ret = lookupCapAndSlot(NODE_STATE(ksCurThread), cptr);
        /* ensure we got a TCB cap */
        word_t cap_type = cap_get_capType(lu_ret.cap);
        if (cap_type != cap_thread_cap) {
            userError("SysDebugNameThread: cap is not a TCB, halting");
            halt();
        }
        /* Add 1 to the IPC buffer to skip the message info word */
        name = (const char *)(lookupIPCBuffer(true, NODE_STATE(ksCurThread)) + 1);
        if (!name) {
            userError("SysDebugNameThread: Failed to lookup IPC buffer, halting");
            halt();
        }
        /* ensure the name isn't too long */
        len = strnlen(name, seL4_MsgMaxLength * sizeof(word_t));
        if (len == seL4_MsgMaxLength * sizeof(word_t)) {
            userError("SysDebugNameThread: Name too long, halting");
            halt();
        }
        setThreadName(TCB_PTR(cap_thread_cap_get_capTCBPtr(lu_ret.cap)), name);
        return EXCEPTION_NONE;
    }
#if defined ENABLE_SMP_SUPPORT && defined CONFIG_ARCH_ARM
    if (w == SysDebugSendIPI) {
        seL4_Word target = getRegister(NODE_STATE(ksCurThread), capRegister);
        seL4_Word irq = getRegister(NODE_STATE(ksCurThread), msgInfoRegister);

        if (target > CONFIG_MAX_NUM_NODES) {
            userError("SysDebugSendIPI: Invalid target, halting");
            halt();
        }
        if (irq > 15) {
            userError("SysDebugSendIPI: Invalid IRQ, not a SGI, halting");
            halt();
        }

        ipi_send_target(CORE_IRQ_TO_IRQT(0, irq), BIT(target));
        return EXCEPTION_NONE;
    }
#endif /* ENABLE_SMP_SUPPORT && CONFIG_ARCH_ARM */
#endif /* CONFIG_DEBUG_BUILD */

#ifdef CONFIG_DANGEROUS_CODE_INJECTION
    if (w == SysDebugRun) {
        ((void (*)(void *))getRegister(NODE_STATE(ksCurThread), capRegister))((void *)getRegister(NODE_STATE(ksCurThread),
                                                                                                  msgInfoRegister));
        return EXCEPTION_NONE;
    }
#endif

#ifdef CONFIG_KERNEL_X86_DANGEROUS_MSR
    if (w == SysX86DangerousWRMSR) {
        uint64_t val;
        uint32_t reg = getRegister(NODE_STATE(ksCurThread), capRegister);
        if (CONFIG_WORD_SIZE == 32) {
            val = (uint64_t)getSyscallArg(0, NULL) | ((uint64_t)getSyscallArg(1, NULL) << 32);
        } else {
            val = getSyscallArg(0, NULL);
        }
        x86_wrmsr(reg, val);
        return EXCEPTION_NONE;
    } else if (w == SysX86DangerousRDMSR) {
        uint64_t val;
        uint32_t reg = getRegister(NODE_STATE(ksCurThread), capRegister);
        val = x86_rdmsr(reg);
        int num = 1;
        if (CONFIG_WORD_SIZE == 32) {
            setMR(NODE_STATE(ksCurThread), NULL, 0, val & 0xffffffff);
            setMR(NODE_STATE(ksCurThread), NULL, 1, val >> 32);
            num++;
        } else {
            setMR(NODE_STATE(ksCurThread), NULL, 0, val);
        }
        setRegister(NODE_STATE(ksCurThread), msgInfoRegister, wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, num)));
        return EXCEPTION_NONE;
    }
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
    if (w == SysBenchmarkFlushCaches) {
#ifdef CONFIG_ARCH_ARM
        tcb_t *thread = NODE_STATE(ksCurThread);
        if (getRegister(thread, capRegister)) {
            arch_clean_invalidate_L1_caches(getRegister(thread, msgInfoRegister));
        } else {
            arch_clean_invalidate_caches();
        }
#else
        arch_clean_invalidate_caches();
#endif
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkResetLog) {
#ifdef CONFIG_KERNEL_LOG_BUFFER
        if (ksUserLogBuffer == 0) {
            userError("A user-level buffer has to be set before resetting benchmark.\
                    Use seL4_BenchmarkSetLogBuffer\n");
            setRegister(NODE_STATE(ksCurThread), capRegister, seL4_IllegalOperation);
            return EXCEPTION_SYSCALL_ERROR;
        }

        ksLogIndex = 0;
#endif /* CONFIG_KERNEL_LOG_BUFFER */
#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
        NODE_STATE(benchmark_log_utilisation_enabled) = true;
        benchmark_track_reset_utilisation(NODE_STATE(ksIdleThread));
        NODE_STATE(ksCurThread)->benchmark.schedule_start_time = ksEnter;
        NODE_STATE(ksCurThread)->benchmark.number_schedules++;

        NODE_STATE(benchmark_start_time) = ksEnter;
        NODE_STATE(benchmark_kernel_time) = 0;
        NODE_STATE(benchmark_kernel_number_entries) = 0;
        NODE_STATE(benchmark_kernel_number_schedules) = 1;
        benchmark_arch_utilisation_reset();
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
        setRegister(NODE_STATE(ksCurThread), capRegister, seL4_NoError);
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkFinalizeLog) {
#ifdef CONFIG_KERNEL_LOG_BUFFER
        ksLogIndexFinalized = ksLogIndex;
        setRegister(NODE_STATE(ksCurThread), capRegister, ksLogIndexFinalized);
#endif /* CONFIG_KERNEL_LOG_BUFFER */
#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
        benchmark_utilisation_finalise();
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkSetLogBuffer) {
#ifdef CONFIG_KERNEL_LOG_BUFFER
        word_t cptr_userFrame = getRegister(NODE_STATE(ksCurThread), capRegister);

        if (benchmark_arch_map_logBuffer(cptr_userFrame) != EXCEPTION_NONE) {
            setRegister(NODE_STATE(ksCurThread), capRegister, seL4_IllegalOperation);
            return EXCEPTION_SYSCALL_ERROR;
        }

        setRegister(NODE_STATE(ksCurThread), capRegister, seL4_NoError);
        return EXCEPTION_NONE;
#endif /* CONFIG_KERNEL_LOG_BUFFER */
    }

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    else if (w == SysBenchmarkGetThreadUtilisation) {
        benchmark_track_utilisation_dump();
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkResetThreadUtilisation) {
        word_t tcb_cptr = getRegister(NODE_STATE(ksCurThread), capRegister);
        lookupCap_ret_t lu_ret;
        word_t cap_type;

        lu_ret = lookupCap(NODE_STATE(ksCurThread), tcb_cptr);
        /* ensure we got a TCB cap */
        cap_type = cap_get_capType(lu_ret.cap);
        if (cap_type != cap_thread_cap) {
            userError("SysBenchmarkResetThreadUtilisation: cap is not a TCB, halting");
            return EXCEPTION_NONE;
        }

        tcb_t *tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(lu_ret.cap));

        benchmark_track_reset_utilisation(tcb);
        return EXCEPTION_NONE;
    }
#ifdef CONFIG_DEBUG_BUILD
    else if (w == SysBenchmarkDumpAllThreadsUtilisation) {
        printf("{\n");
        printf("  \"BENCHMARK_TOTAL_UTILISATION\":%lu,\n",
               (word_t)(NODE_STATE(benchmark_end_time) - NODE_STATE(benchmark_start_time)));
        printf("  \"BENCHMARK_TOTAL_KERNEL_UTILISATION\":%lu,\n", (word_t) NODE_STATE(benchmark_kernel_time));
        printf("  \"BENCHMARK_TOTAL_NUMBER_KERNEL_ENTRIES\":%lu,\n", (word_t) NODE_STATE(benchmark_kernel_number_entries));
        printf("  \"BENCHMARK_TOTAL_NUMBER_SCHEDULES\":%lu,\n", (word_t) NODE_STATE(benchmark_kernel_number_schedules));
        printf("  \"BENCHMARK_TCB_\": [\n");
        for (tcb_t *curr = NODE_STATE(ksDebugTCBs); curr != NULL; curr = TCB_PTR_DEBUG_PTR(curr)->tcbDebugNext) {
            printf("    {\n");
            printf("      \"NAME\":\"%s\",\n", TCB_PTR_DEBUG_PTR(curr)->tcbName);
            printf("      \"UTILISATION\":%lu,\n", (word_t) curr->benchmark.utilisation);
            printf("      \"NUMBER_SCHEDULES\":%lu,\n", (word_t) curr->benchmark.number_schedules);
            printf("      \"KERNEL_UTILISATION\":%lu,\n", (word_t) curr->benchmark.kernel_utilisation);
            printf("      \"NUMBER_KERNEL_ENTRIES\":%lu\n", (word_t) curr->benchmark.number_kernel_entries);
            printf("    }");
            if (TCB_PTR_DEBUG_PTR(curr)->tcbDebugNext != NULL) {
                printf(",\n");
            } else {
                printf("\n");
            }
        }
        printf("  ]\n}\n");
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkResetAllThreadsUtilisation) {
        for (tcb_t *curr = NODE_STATE(ksDebugTCBs); curr != NULL; curr = TCB_PTR_DEBUG_PTR(curr)->tcbDebugNext) {
            benchmark_track_reset_utilisation(curr);
        }
        return EXCEPTION_NONE;
    }
#endif /* CONFIG_DEBUG_BUILD */
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

    else if (w == SysBenchmarkNullSyscall) {
        return EXCEPTION_NONE;
    }
#endif /* CONFIG_ENABLE_BENCHMARKS */

    MCS_DO_IF_BUDGET({
#ifdef CONFIG_SET_TLS_BASE_SELF
        if (w == SysSetTLSBase)
        {
            word_t tls_base = getRegister(NODE_STATE(ksCurThread), capRegister);
            /*
             * This updates the real register as opposed to the thread state
             * value. For many architectures, the TLS variables only get
             * updated on a thread switch.
             */
            return Arch_setTLSRegister(tls_base);
        }
#endif
        current_fault = seL4_Fault_UnknownSyscall_new(w);
        handleFault(NODE_STATE(ksCurThread));
    })

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

exception_t handleUserLevelFault(word_t w_a, word_t w_b)
{
    MCS_DO_IF_BUDGET({
        current_fault = seL4_Fault_UserException_new(w_a, w_b);
        handleFault(NODE_STATE(ksCurThread));
    })
    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

exception_t handleVMFaultEvent(vm_fault_type_t vm_faultType)
{
    MCS_DO_IF_BUDGET({

        exception_t status = handleVMFault(NODE_STATE(ksCurThread), vm_faultType);
        if (status != EXCEPTION_NONE)
        {
            handleFault(NODE_STATE(ksCurThread));
        }
    })

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

#ifdef CONFIG_KERNEL_MCS
static exception_t handleInvocation(bool_t isCall, bool_t isBlocking, bool_t canDonate, bool_t firstPhase, cptr_t cptr)
#else
static exception_t handleInvocation(bool_t isCall, bool_t isBlocking)
#endif
{
    seL4_MessageInfo_t info;
    lookupCapAndSlot_ret_t lu_ret;
    word_t *buffer;
    exception_t status;
    word_t length;
    tcb_t *thread;

    thread = NODE_STATE(ksCurThread);

    info = messageInfoFromWord(getRegister(thread, msgInfoRegister));
#ifndef CONFIG_KERNEL_MCS
    cptr_t cptr = getRegister(thread, capRegister);
#endif

    /* faulting section */
    lu_ret = lookupCapAndSlot(thread, cptr);

    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        userError("Invocation of invalid cap #%lu.", cptr);
        current_fault = seL4_Fault_CapFault_new(cptr, false);

        if (isBlocking) {
            handleFault(thread);
        }

        return EXCEPTION_NONE;
    }

    buffer = lookupIPCBuffer(false, thread);

    status = lookupExtraCaps(thread, buffer, info);

    if (unlikely(status != EXCEPTION_NONE)) {
        userError("Lookup of extra caps failed.");
        if (isBlocking) {
            handleFault(thread);
        }
        return EXCEPTION_NONE;
    }

    /* Syscall error/Preemptible section */
    length = seL4_MessageInfo_get_length(info);
    if (unlikely(length > n_msgRegisters && !buffer)) {
        length = n_msgRegisters;
    }
#ifdef CONFIG_KERNEL_MCS
    status = decodeInvocation(seL4_MessageInfo_get_label(info), length,
                              cptr, lu_ret.slot, lu_ret.cap,
                              current_extra_caps, isBlocking, isCall,
                              canDonate, firstPhase, buffer);
#else
    status = decodeInvocation(seL4_MessageInfo_get_label(info), length,
                              cptr, lu_ret.slot, lu_ret.cap,
                              current_extra_caps, isBlocking, isCall,
                              buffer);
#endif

    if (unlikely(status == EXCEPTION_PREEMPTED)) {
        return status;
    }

    if (unlikely(status == EXCEPTION_SYSCALL_ERROR)) {
        if (isCall) {
            replyFromKernel_error(thread);
        }
        return EXCEPTION_NONE;
    }

    if (unlikely(
            thread_state_get_tsType(thread->tcbState) == ThreadState_Restart)) {
        if (isCall) {
            replyFromKernel_success_empty(thread);
        }
        setThreadState(thread, ThreadState_Running);
    }

    return EXCEPTION_NONE;
}

#ifdef CONFIG_KERNEL_MCS
static inline lookupCap_ret_t lookupReply(void)
{
    word_t replyCPtr = getRegister(NODE_STATE(ksCurThread), replyRegister);
    lookupCap_ret_t lu_ret = lookupCap(NODE_STATE(ksCurThread), replyCPtr);
    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        userError("Reply cap lookup failed");
        current_fault = seL4_Fault_CapFault_new(replyCPtr, true);
        handleFault(NODE_STATE(ksCurThread));
        return lu_ret;
    }

    if (unlikely(cap_get_capType(lu_ret.cap) != cap_reply_cap)) {
        userError("Cap in reply slot is not a reply");
        current_fault = seL4_Fault_CapFault_new(replyCPtr, true);
        handleFault(NODE_STATE(ksCurThread));
        lu_ret.status = EXCEPTION_FAULT;
        return lu_ret;
    }

    return lu_ret;
}
#else
static void handleReply(void)
{
    cte_t *callerSlot;
    cap_t callerCap;

    callerSlot = TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbCaller);
    callerCap = callerSlot->cap;

    switch (cap_get_capType(callerCap)) {
    case cap_reply_cap: {
        tcb_t *caller;

        if (cap_reply_cap_get_capReplyMaster(callerCap)) {
            break;
        }
        caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(callerCap));
        /* Haskell error:
         * "handleReply: caller must not be the current thread" */
        assert(caller != NODE_STATE(ksCurThread));
        doReplyTransfer(NODE_STATE(ksCurThread), caller, callerSlot,
                        cap_reply_cap_get_capReplyCanGrant(callerCap));
        return;
    }

    case cap_null_cap:
        userError("Attempted reply operation when no reply cap present.");
        return;

    default:
        break;
    }

    fail("handleReply: invalid caller cap");
}
#endif

#ifdef CONFIG_KERNEL_MCS
static void handleRecv(bool_t isBlocking, bool_t canReply)
#else
static void handleRecv(bool_t isBlocking)
#endif
{
    word_t epCPtr;
    lookupCap_ret_t lu_ret;

    epCPtr = getRegister(NODE_STATE(ksCurThread), capRegister);

    lu_ret = lookupCap(NODE_STATE(ksCurThread), epCPtr);

    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        /* current_lookup_fault has been set by lookupCap */
        current_fault = seL4_Fault_CapFault_new(epCPtr, true);
        handleFault(NODE_STATE(ksCurThread));
        return;
    }

    switch (cap_get_capType(lu_ret.cap)) {
    case cap_endpoint_cap:
        if (unlikely(!cap_endpoint_cap_get_capCanReceive(lu_ret.cap))) {
            current_lookup_fault = lookup_fault_missing_capability_new(0);
            current_fault = seL4_Fault_CapFault_new(epCPtr, true);
            handleFault(NODE_STATE(ksCurThread));
            break;
        }

#ifdef CONFIG_KERNEL_MCS
        cap_t ep_cap = lu_ret.cap;
        cap_t reply_cap = cap_null_cap_new();
        if (canReply) {
            lu_ret = lookupReply();
            if (lu_ret.status != EXCEPTION_NONE) {
                return;
            } else {
                reply_cap = lu_ret.cap;
            }
        }
        receiveIPC(NODE_STATE(ksCurThread), ep_cap, isBlocking, reply_cap);
#else
        deleteCallerCap(NODE_STATE(ksCurThread));
        receiveIPC(NODE_STATE(ksCurThread), lu_ret.cap, isBlocking);
#endif
        break;

    case cap_notification_cap: {
        notification_t *ntfnPtr;
        tcb_t *boundTCB;
        ntfnPtr = NTFN_PTR(cap_notification_cap_get_capNtfnPtr(lu_ret.cap));
        boundTCB = (tcb_t *)notification_ptr_get_ntfnBoundTCB(ntfnPtr);
        if (unlikely(!cap_notification_cap_get_capNtfnCanReceive(lu_ret.cap)
                     || (boundTCB && boundTCB != NODE_STATE(ksCurThread)))) {
            current_lookup_fault = lookup_fault_missing_capability_new(0);
            current_fault = seL4_Fault_CapFault_new(epCPtr, true);
            handleFault(NODE_STATE(ksCurThread));
            break;
        }

        receiveSignal(NODE_STATE(ksCurThread), lu_ret.cap, isBlocking);
        break;
    }
    default:
        current_lookup_fault = lookup_fault_missing_capability_new(0);
        current_fault = seL4_Fault_CapFault_new(epCPtr, true);
        handleFault(NODE_STATE(ksCurThread));
        break;
    }
}

#ifdef CONFIG_KERNEL_MCS
static inline void mcsIRQ(irq_t irq)
{
    if (IRQT_TO_IRQ(irq) == KERNEL_TIMER_IRQ) {
        /* if this is a timer irq we must update the time as we need to reprogram the timer, and we
         * can't lose the time that has just been used by the kernel. */
        updateTimestamp();
    }

    /* at this point we could be handling a timer interrupt which actually ends the current
     * threads timeslice. However, preemption is possible on revoke, which could have deleted
     * the current thread and/or the current scheduling context, rendering them invalid. */
    if (isSchedulable(NODE_STATE(ksCurThread))) {
        /* if the thread is schedulable, the tcb and scheduling context are still valid */
        checkBudget();
    } else if (NODE_STATE(ksCurSC)->scRefillMax) {
        /* otherwise, if the thread is not schedulable, the SC could be valid - charge it if so */
        chargeBudget(NODE_STATE(ksConsumed), false, CURRENT_CPU_INDEX(), true);
    }

}
#else
#define handleRecv(isBlocking, canReply) handleRecv(isBlocking)
#define mcsIRQ(irq)
#define handleInvocation(isCall, isBlocking, canDonate, firstPhase, cptr) handleInvocation(isCall, isBlocking)
#endif

static void handleYield(void)
{
#ifdef CONFIG_KERNEL_MCS
    /* Yield the current remaining budget */
    ticks_t consumed = NODE_STATE(ksCurSC)->scConsumed + NODE_STATE(ksConsumed);
    chargeBudget(refill_head(NODE_STATE(ksCurSC))->rAmount, false, CURRENT_CPU_INDEX(), true);
    /* Manually updated the scConsumed so that the full timeslice isn't added, just what was consumed */
    NODE_STATE(ksCurSC)->scConsumed = consumed;
#else
    tcbSchedDequeue(NODE_STATE(ksCurThread));
    SCHED_APPEND_CURRENT_TCB;
    rescheduleRequired();
#endif
}

exception_t handleSyscall(syscall_t syscall)
{
    exception_t ret;
    irq_t irq;
    MCS_DO_IF_BUDGET({
        switch (syscall)
        {
        case SysSend:
            ret = handleInvocation(false, true, false, false, getRegister(NODE_STATE(ksCurThread), capRegister));
            if (unlikely(ret != EXCEPTION_NONE)) {
                irq = getActiveIRQ();
                if (IRQT_TO_IRQ(irq) != IRQT_TO_IRQ(irqInvalid)) {
                    mcsIRQ(irq);
                    handleInterrupt(irq);
                    Arch_finaliseInterrupt();
                }
            }

            break;

        case SysNBSend:
            ret = handleInvocation(false, false, false, false, getRegister(NODE_STATE(ksCurThread), capRegister));
            if (unlikely(ret != EXCEPTION_NONE)) {
                irq = getActiveIRQ();
                if (IRQT_TO_IRQ(irq) != IRQT_TO_IRQ(irqInvalid)) {
                    mcsIRQ(irq);
                    handleInterrupt(irq);
                    Arch_finaliseInterrupt();
                }
            }
            break;

        case SysCall:
            ret = handleInvocation(true, true, true, false, getRegister(NODE_STATE(ksCurThread), capRegister));
            if (unlikely(ret != EXCEPTION_NONE)) {
                irq = getActiveIRQ();
                if (IRQT_TO_IRQ(irq) != IRQT_TO_IRQ(irqInvalid)) {
                    mcsIRQ(irq);
                    handleInterrupt(irq);
                    Arch_finaliseInterrupt();
                }
            }
            break;

        case SysRecv:
            handleRecv(true, true);
            break;
#ifndef CONFIG_KERNEL_MCS
        case SysReply:
            handleReply();
            break;

        case SysReplyRecv:
            handleReply();
            handleRecv(true, true);
            break;

#else /* CONFIG_KERNEL_MCS */
        case SysWait:
            handleRecv(true, false);
            break;

        case SysNBWait:
            handleRecv(false, false);
            break;
        case SysReplyRecv: {
            cptr_t reply = getRegister(NODE_STATE(ksCurThread), replyRegister);
            ret = handleInvocation(false, false, true, true, reply);
            /* reply cannot error and is not preemptible */
            assert(ret == EXCEPTION_NONE);
            handleRecv(true, true);
            break;
        }

        case SysNBSendRecv: {
            cptr_t dest = getNBSendRecvDest();
            ret = handleInvocation(false, false, true, true, dest);
            if (unlikely(ret != EXCEPTION_NONE)) {
                irq = getActiveIRQ();
                if (IRQT_TO_IRQ(irq) != IRQT_TO_IRQ(irqInvalid)) {
                    mcsIRQ(irq);
                    handleInterrupt(irq);
                    Arch_finaliseInterrupt();
                }
                break;
            }
            handleRecv(true, true);
            break;
        }

        case SysNBSendWait:
            ret = handleInvocation(false, false, true, true, getRegister(NODE_STATE(ksCurThread), replyRegister));
            if (unlikely(ret != EXCEPTION_NONE)) {
                irq = getActiveIRQ();
                if (IRQT_TO_IRQ(irq) != IRQT_TO_IRQ(irqInvalid)) {
                    mcsIRQ(irq);
                    handleInterrupt(irq);
                    Arch_finaliseInterrupt();
                }
                break;
            }
            handleRecv(true, false);
            break;
#endif
        case SysNBRecv:
            handleRecv(false, true);
            break;

        case SysYield:
            handleYield();
            break;

        default:
            fail("Invalid syscall");
        }

    })

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}
