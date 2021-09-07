/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#include <machine/registerset.h>
#include <object/cnode.h>

#ifdef CONFIG_DEBUG_BUILD
/* Maximum length of the tcb name, including null terminator */
#define TCB_NAME_LENGTH (BIT(seL4_TCBBits-1) - (tcbCNodeEntries * sizeof(cte_t)) - sizeof(debug_tcb_t))
compile_assert(tcb_name_fits, TCB_NAME_LENGTH > 0)
#endif

struct tcb_queue {
    tcb_t *head;
    tcb_t *end;
};
typedef struct tcb_queue tcb_queue_t;

static inline unsigned int setMR(tcb_t *receiver, word_t *receiveIPCBuffer,
                                 unsigned int offset, word_t reg)
{
    if (offset >= n_msgRegisters) {
        if (receiveIPCBuffer) {
            receiveIPCBuffer[offset + 1] = reg;
            return offset + 1;
        } else {
            return n_msgRegisters;
        }
    } else {
        setRegister(receiver, msgRegisters[offset], reg);
        return offset + 1;
    }
}

void tcbSchedEnqueue(tcb_t *tcb);
void tcbSchedAppend(tcb_t *tcb);
void tcbSchedDequeue(tcb_t *tcb);

#ifdef CONFIG_DEBUG_BUILD
void tcbDebugAppend(tcb_t *tcb);
void tcbDebugRemove(tcb_t *tcb);
#endif
#ifdef CONFIG_KERNEL_MCS
void tcbReleaseRemove(tcb_t *tcb);
void tcbReleaseEnqueue(tcb_t *tcb);
tcb_t *tcbReleaseDequeue(void);
#endif

#ifdef ENABLE_SMP_SUPPORT
void remoteQueueUpdate(tcb_t *tcb);
void remoteTCBStall(tcb_t *tcb);

#define SCHED_ENQUEUE(_t) do {      \
    tcbSchedEnqueue(_t);            \
    remoteQueueUpdate(_t);          \
} while (0)

#define SCHED_APPEND(_t) do {       \
    tcbSchedAppend(_t);             \
    remoteQueueUpdate(_t);          \
} while (0)

#else
#define SCHED_ENQUEUE(_t)           tcbSchedEnqueue(_t)
#define SCHED_APPEND(_t)            tcbSchedAppend(_t)
#endif /* ENABLE_SMP_SUPPORT */

#define SCHED_ENQUEUE_CURRENT_TCB   tcbSchedEnqueue(NODE_STATE(ksCurThread))
#define SCHED_APPEND_CURRENT_TCB    tcbSchedAppend(NODE_STATE(ksCurThread))

#ifdef CONFIG_KERNEL_MCS
/* Add TCB into the priority ordered endpoint queue */
static inline tcb_queue_t tcbEPAppend(tcb_t *tcb, tcb_queue_t queue)
{
    /* start at the back of the queue as FIFO is the common case */
    tcb_t *before = queue.end;
    tcb_t *after = NULL;

    /* find a place to put the tcb */
    while (unlikely(before != NULL && tcb->tcbPriority > before->tcbPriority)) {
        after = before;
        before = after->tcbEPPrev;
    }

    if (unlikely(before == NULL)) {
        /* insert at head */
        queue.head = tcb;
    } else {
        before->tcbEPNext = tcb;
    }

    if (likely(after == NULL)) {
        /* insert at tail */
        queue.end = tcb;
    } else {
        after->tcbEPPrev = tcb;
    }

    tcb->tcbEPNext = after;
    tcb->tcbEPPrev = before;

    return queue;
}

tcb_queue_t tcbEPDequeue(tcb_t *tcb, tcb_queue_t queue);

#else
tcb_queue_t tcbEPAppend(tcb_t *tcb, tcb_queue_t queue);
tcb_queue_t tcbEPDequeue(tcb_t *tcb, tcb_queue_t queue);

void setupCallerCap(tcb_t *sender, tcb_t *receiver, bool_t canGrant);
void deleteCallerCap(tcb_t *receiver);
#endif

word_t copyMRs(tcb_t *sender, word_t *sendBuf, tcb_t *receiver,
               word_t *recvBuf, word_t n);
exception_t decodeTCBInvocation(word_t invLabel, word_t length, cap_t cap,
                                cte_t *slot, bool_t call, word_t *buffer);
exception_t decodeCopyRegisters(cap_t cap, word_t length, word_t *buffer);
exception_t decodeReadRegisters(cap_t cap, word_t length, bool_t call,
                                word_t *buffer);
exception_t decodeWriteRegisters(cap_t cap, word_t length, word_t *buffer);
exception_t decodeTCBConfigure(cap_t cap, word_t length,
                               cte_t *slot, word_t *buffer);
exception_t decodeSetPriority(cap_t cap, word_t length, word_t *buffer);
exception_t decodeSetMCPriority(cap_t cap, word_t length, word_t *buffer);
#ifdef CONFIG_KERNEL_MCS
exception_t decodeSetSchedParams(cap_t cap, word_t length, cte_t *slot, word_t *buffer);
#else
exception_t decodeSetSchedParams(cap_t cap, word_t length, word_t *buffer);
#endif
exception_t decodeSetIPCBuffer(cap_t cap, word_t length,
                               cte_t *slot, word_t *buffer);
exception_t decodeSetSpace(cap_t cap, word_t length,
                           cte_t *slot, word_t *buffer);
exception_t decodeDomainInvocation(word_t invLabel, word_t length, word_t *buffer);
exception_t decodeBindNotification(cap_t cap);
exception_t decodeUnbindNotification(cap_t cap);
#ifdef CONFIG_KERNEL_MCS
exception_t decodeSetTimeoutEndpoint(cap_t cap, cte_t *slot);
#endif


#ifdef CONFIG_KERNEL_MCS
enum thread_control_caps_flag {
    thread_control_caps_update_ipc_buffer = 0x1,
    thread_control_caps_update_space = 0x2,
    thread_control_caps_update_fault = 0x4,
    thread_control_caps_update_timeout = 0x8,
};

enum thread_control_sched_flag {
    thread_control_sched_update_priority = 0x1,
    thread_control_sched_update_mcp = 0x2,
    thread_control_sched_update_sc = 0x4,
    thread_control_sched_update_fault = 0x8,
};
#else
enum thread_control_flag {
    thread_control_update_priority = 0x1,
    thread_control_update_ipc_buffer = 0x2,
    thread_control_update_space = 0x4,
    thread_control_update_mcp = 0x8,
#ifdef CONFIG_KERNEL_MCS
    thread_control_update_sc = 0x10,
    thread_control_update_fault = 0x20,
    thread_control_update_timeout = 0x40,
#endif
};
#endif

typedef word_t thread_control_flag_t;

exception_t invokeTCB_Suspend(tcb_t *thread);
exception_t invokeTCB_Resume(tcb_t *thread);
#ifdef CONFIG_KERNEL_MCS
exception_t invokeTCB_ThreadControlCaps(tcb_t *target, cte_t *slot,
                                        cap_t fh_newCap, cte_t *fh_srcSlot,
                                        cap_t th_newCap, cte_t *th_srcSlot,
                                        cap_t cRoot_newCap, cte_t *cRoot_srcSlot,
                                        cap_t vRoot_newCap, cte_t *vRoot_srcSlot,
                                        word_t bufferAddr, cap_t bufferCap,
                                        cte_t *bufferSrcSlot,
                                        thread_control_flag_t updateFlags);
exception_t invokeTCB_ThreadControlSched(tcb_t *target, cte_t *slot,
                                         cap_t fh_newCap, cte_t *fh_srcSlot,
                                         prio_t mcp, prio_t priority,
                                         sched_context_t *sc,
                                         thread_control_flag_t updateFlags);
#else
exception_t invokeTCB_ThreadControl(tcb_t *target, cte_t *slot, cptr_t faultep,
                                    prio_t mcp, prio_t priority, cap_t cRoot_newCap,
                                    cte_t *cRoot_srcSlot, cap_t vRoot_newCap,
                                    cte_t *vRoot_srcSlot, word_t bufferAddr,
                                    cap_t bufferCap, cte_t *bufferSrcSlot,
                                    thread_control_flag_t updateFlags);
#endif
exception_t invokeTCB_CopyRegisters(tcb_t *dest, tcb_t *src,
                                    bool_t suspendSource, bool_t resumeTarget,
                                    bool_t transferFrame, bool_t transferInteger,
                                    word_t transferArch);
exception_t invokeTCB_ReadRegisters(tcb_t *src, bool_t suspendSource,
                                    word_t n, word_t arch, bool_t call);
exception_t invokeTCB_WriteRegisters(tcb_t *dest, bool_t resumeTarget,
                                     word_t n, word_t arch, word_t *buffer);
exception_t invokeTCB_NotificationControl(tcb_t *tcb, notification_t *ntfnPtr);

cptr_t PURE getExtraCPtr(word_t *bufferPtr, word_t i);
void setExtraBadge(word_t *bufferPtr, word_t badge, word_t i);

exception_t lookupExtraCaps(tcb_t *thread, word_t *bufferPtr, seL4_MessageInfo_t info);
word_t setMRs_syscall_error(tcb_t *thread, word_t *receiveIPCBuffer);
word_t CONST Arch_decodeTransfer(word_t flags);
exception_t CONST Arch_performTransfer(word_t arch, tcb_t *tcb_src,
                                       tcb_t *tcb_dest);

#ifdef ENABLE_SMP_SUPPORT
void Arch_migrateTCB(tcb_t *thread);
#endif /* ENABLE_SMP_SUPPORT */

#ifdef CONFIG_DEBUG_BUILD
void setThreadName(tcb_t *thread, const char *name);
#endif /* CONFIG_DEBUG_BUILD */

