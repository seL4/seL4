/*
 * Copyright 2016, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SYSCALLS_H
#define __LIBSEL4_SYSCALLS_H
#include <autoconf.h>

/**
 * @defgroup SystemCalls System Calls
 * @{
 *
 * @defgroup GeneralSystemCalls General System Calls
 * @{
 */

/**
 * @xmlonly <manual name="Send" label="sel4_send"/> @endxmlonly
 * @brief Send to a capability
 *
 * @xmlonly
 * See <autoref sec="sys_send"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Recv" label="sel4_recv"/> @endxmlonly
 * @brief Block until a message is received on an endpoint
 *
 * @xmlonly
 * See <autoref sec="sys_recv"/>
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender);

/**
 * @xmlonly <manual name="Call" label="sel4_call"/> @endxmlonly
 * @brief  Call a capability
 *
 * @xmlonly
 * See <autoref sec="sys_call"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Reply" label="sel4_reply"/> @endxmlonly
 * @brief Perform a send to a one-off reply capability stored when
 *        the thread was last called
 *
 * @xmlonly
 * See <autoref sec="sys_reply"/>
 * @endxmlonly
 *
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_Reply(seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Polling Send" label="sel4_nbsend"/> @endxmlonly
 * @brief Perform a polling send to a capability
 *
 * @xmlonly
 * See <autoref sec="sys_nbsend"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Reply Recv" label="sel4_replyrecv"/> @endxmlonly
 * @brief Perform a reply followed by a receive in one system call
 *
 * @xmlonly
 * See <autoref sec="sys_replyrecv"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender);

/**
 * @xmlonly <manual name="NBRecv" label="sel4_nbrecv"/> @endxmlonly
 * @brief Receive a message from an endpoint but do not block
 *        in the case that no messages are pending
 *
 * @xmlonly
 * See <autoref sec="sys_nbrecv"/>
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *                    The sender information is the badge of the
 *                    endpoint capability that was invoked by the
 *                    sender, or the notification word of the
 *                    notification object that was signalled.
 *                    This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word* sender);

/**
 * @xmlonly <manual name="Yield" label="sel4_yield"/> @endxmlonly
 * @brief Donate the remaining timeslice to a thread of the same priority
 *
 * @xmlonly
 * See <autoref sec="sys_yield"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_Yield(void);

/**
 * @xmlonly <manual name="Signal" label="sel4_signal"/> @endxmlonly
 * @brief Signal a notification
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_Send().
 * It is useful for signalling a notification.
 *
 * @xmlonly
 * See the description of <nameref name="seL4_Send"/> in <autoref sec="sys_send"/>.
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 */
LIBSEL4_INLINE_FUNC void
seL4_Signal(seL4_CPtr dest);

/**
 * @xmlonly <manual name="Wait" label="sel4_wait"/> @endxmlonly
 * @brief Perform a receive on a notification object
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_Recv().
 *
 * @xmlonly
 * See the description of <nameref name="seL4_Recv"/> in <autoref sec="sys_recv"/>.
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 */
LIBSEL4_INLINE_FUNC void
seL4_Wait(seL4_CPtr src, seL4_Word *sender);

/**
 * @xmlonly <manual name="Poll" label="sel4_poll"/> @endxmlonly
 * @brief Perform a non-blocking recv on a notification object
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_NBRecv().
 * It is useful for doing a non-blocking wait on a notification.
 *
 * @xmlonly
 * See the description of <nameref name="seL4_NBRecv"/> in <autoref sec="sys_nbrecv"/>.
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Poll(seL4_CPtr src, seL4_Word *sender);

/** @} */

/**
 * @defgroup DebuggingSystemCalls
 * @{
 */
#ifdef CONFIG_PRINTING
/**
 * @xmlonly <manual name="Put Char" label="sel4_debugputchar"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugPutChar(char c);
#endif

#if CONFIG_DEBUG_BUILD
/**
 * @xmlonly <manual name="Halt" label="sel4_debughalt"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugHalt(void);

/**
 * @xmlonly <manual name="Snapshot" label="sel4_debugsnapshot"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugSnapshot(void);

/**
 * @xmlonly <manual name="Cap Identify" label="sel4_debugcapidentify"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap);

/**
 * @xmlonly <manual name="Name Thread" label="sel4_debugnamethread"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name);
#endif

#ifdef CONFIG_DANGEROUS_CODE_INJECTION
/**
 * @xmlonly <manual name="Run" label="sel4_debugrun"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugRun(void (* userfn) (void *), void* userarg);
#endif
/** @} */

/**
 * @defgroup BenchmarkingSystemCalls
 * @{
 */
#ifdef CONFIG_ENABLE_BENCHMARKS
/*
 * TODO: The following comment should be added to the manual when SELFOUR-962 is complete.
 * Currently there are different Benchmarking modes/configs in the kernel:
 * 1) BENCHMARK_TRACEPOINTS: Enable using tracepoints in the kernel and timing code.
 * 2) BENCHMARK_TRACK_KERNEL_ENTRIES: Keep track of information on kernel entries.
 * 3) BENCHMARK_TRACK_UTILISATION: Allow users to get CPU timing info for the system, threads and/or idle thread.
 *
 * BENCHMARK_TRACEPOINTS and BENCHMARK_TRACK_KERNEL_ENTRIES use a log buffer that has to be allocated by the user and mapped
 * to a fixed location in the kernel window.
 * All of timing info are in cycles.
 */

/**
 * @xmlonly <manual name="Reset Log" label="sel4_benchmarkresetlog"/> @endxmlonly
 * @brief Reset benchmark logging.
 *
 * The behaviour of this system call depends on benchmarking mode in action while invoking
 * this system call: 1) BENCHMARK_TRACEPOINTS or BENCHMARK_TRACK_KERNEL_ENTRIES:  resets log index to 0.
 * 2) BENCHMARK_TRACK_UTILISATION: resets benchmark and current thread start time (to the time of invoking
 * this syscall), resets idle thread utilisation to 0, and starts tracking utilisation.
 *
 * @return A `seL4_Error` error if the user-level log buffer has not been set by the user
 *                         (BENCHMARK_TRACEPOINTS/BENCHMARK_TRACK_KERNEL_ENTRIES).
 */
LIBSEL4_INLINE_FUNC seL4_Error
seL4_BenchmarkResetLog(void);

/**
 * @xmlonly <manual name="Finalize Log" label="sel4_benchmarkfinalizelog"/> @endxmlonly
 * @brief Stop benchmark logging.
 *
 * The behaviour of this system call depends on
 * benchmarking mode in action while invoking this system call:
 *         1) BENCHMARK_TRACEPOINTS or BENCHMARK_TRACK_KERNEL_ENTRIES: Sets log buffer's stop index to current index.
 *         2) BENCHMARK_TRACK_UTILISATION: Sets benchmark end time to current time, stops tracking utilisation.
 *
 * @return The index of the final entry in the log buffer (if BENCHMARK_TRACEPOINTS/BENCHMARK_TRACK_KERNEL_ENTRIES are enabled).
 *
 */
LIBSEL4_INLINE_FUNC seL4_Word
seL4_BenchmarkFinalizeLog(void);

/**
 * @xmlonly <manual name="Set Log Buffer" label="sel4_benchmarksetlogbuffer"/> @endxmlonly
 * @brief Set log buffer.
 *
 * Provide a large frame object for the kernel to use as a log-buffer.
 * The object must not be device memory, and must be seL4_LargePageBits in size.
 *
 * @param[in] frame_cptr A capability pointer to a user allocated frame of seL4_LargePage size.
 * @return A `seL4_IllegalOperation` error if frame_cptr is not valid and couldn't set the buffer.
 *
 */
LIBSEL4_INLINE_FUNC seL4_Error
seL4_BenchmarkSetLogBuffer(seL4_Word frame_cptr);

/**
 * @xmlonly <manual name="Null Syscall" label="sel4_benchmarknullsyscall"/> @endxmlonly
 * @brief Null system call that enters and exits the kernel immediately, for timing kernel traps in microbenchmarks.
 *
 *  Used to time kernel traps (in and out).
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkNullSyscall(void);

/**
 * @xmlonly <manual name="Flush Caches" label="sel4_benchmarkflushcaches"/> @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkFlushCaches(void);

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
/**
 * @xmlonly <manual name="Get Thread Utilisation" label="sel4_benchmarkgetthreadutilisation"/> @endxmlonly
 * @brief Get utilisation timing information.
 *
 * Get timing information for the system, requested thread and idle thread. Such information is written
 * into the caller's IPC buffer; see the definition of `benchmark_track_util_ipc_index` enum for more
 * details on the data/format returned on the IPC buffer.
 *
 * @param[in] tcb_cptr TCB cap pointer to a thread to get CPU utilisation for.
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkGetThreadUtilisation(seL4_Word tcb_cptr);

/**
 * @xmlonly <manual name="Reset Thread Utilisation" label="sel4_benchmarkresetthreadutilisation"/> @endxmlonly
 * @brief Reset utilisation timing for a specific thread.
 *
 * Reset the kernel's timing information data (start time and utilisation) for a specific thread.
 *
 * @param[in] tcb_cptr TCB cap pointer to a thread to get CPU utilisation for.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkResetThreadUtilisation(seL4_Word tcb_cptr);
#endif
#endif
/** @} */

/** @} */

#endif /* __LIBSEL4_SYSCALLS_H */
