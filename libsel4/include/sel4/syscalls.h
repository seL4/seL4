/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once
#include <autoconf.h>

#include <sel4/functions.h>

/**
 * @defgroup SystemCalls System Calls
 * @{
 *
 */

#ifdef CONFIG_KERNEL_MCS
#include "syscalls_mcs.h"
#else
#include "syscalls_master.h"
#endif

/**
 * @defgroup DebuggingSystemCalls
 * This section documents debugging system calls available when the kernel is
 * build with the `DEBUG_BUILD` configuration. For any system calls that rely
 * on a kernel serial driver, `PRINTING` must also be enabled.
 *
 * @{
 */
#ifdef CONFIG_PRINTING
/**
 * @xmlonly <manual name="Put Char" label="sel4_debugputchar"/> @endxmlonly
 * @brief Output a single char through the kernel.
 *
 * Use the kernel serial driver to output a single character. This is useful for
 * debugging when a user level serial driver is not available.
 *
 * @param c The character to output.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugPutChar(char c);

/**
 * @xmlonly <manual name="Dump scheduler" label="sel4_dumpscheduler"/> @endxmlonly
 * @brief Output the contents of the kernel scheduler.
 *
 * Dump the state of the all TCB objects to kernel serial output. This system call
 * will output a table containing:
 *    - Address: the address of the TCB object for that thread,
 *    - Name:    the name of the thread (if set),
 *    - IP:      the contents of the instruction pointer the thread is at,
 *    - Priority: the priority of that thread,
 *    - State   : the state of the thread.
 */

LIBSEL4_INLINE_FUNC void
seL4_DebugDumpScheduler(void);
#endif

#if CONFIG_DEBUG_BUILD
/**
 * @xmlonly <manual name="Halt" label="sel4_debughalt"/> @endxmlonly
 * @brief Halt the system.
 *
 * This debugging system call will cause the kernel immediately cease responding to
 * system calls. The kernel will switch permanently to the idle thread with
 * interrupts disabled. Depending on the platform, the kernel may switch
 * the hardware into a low-power state.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugHalt(void);

/**
 * @xmlonly <manual name="Snapshot" label="sel4_debugsnapshot"/> @endxmlonly
 * @brief Output a capDL dump of the current kernel state.
 *
 * This debugging system call will output all of the capabilities in the current
 * kernel using capDL.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugSnapshot(void);

/**
 * @xmlonly <manual name="Cap Identify" label="sel4_debugcapidentify"/> @endxmlonly
 * @brief Identify the type of a capability in the current cspace.
 *
 * This debugging system call returns the type of capability in a capability
 * slot in the current cspace. The type returned is not a libsel4 type, but
 * refers to an internal seL4 type. This can be looked up in a built kernel by
 * looking for the (generated) `enum cap_tag`, type `cap_tag_t`.
 *
 * @param cap A capability slot in the current cspace.
 * @return The type of capability passed in.
 *
 */
LIBSEL4_INLINE_FUNC seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap);

/**
 * @xmlonly <manual name="Name Thread" label="sel4_debugnamethread"/> @endxmlonly
 * @brief Name a thread.
 *
 * Name a thread. This name will then be output by the kernel in all debugging output.
 * Note that the max name length that can be passed to this function is limited by the
 * number of chars that will fit in an IPC message (`seL4_MsgMaxLength` multiplied by the
 * amount of chars that fit in a word). However the name is also truncated in order to fit into a TCB object.
 * For some platforms you may need to increase `seL4_TCBBits` by 1 in a debug build in order to
 * fit a long enough name.
 *
 * @param tcb A capability to the tcb object for the thread to name.
 * @param name The name for the thread.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name);
#if CONFIG_MAX_NUM_NODES > 1 && defined CONFIG_ARCH_ARM
/**
 * @xmlonly <manual name="Send SGI 0-15" label="sel4_debugsendipi"/> @endxmlonly
 * @brief Sends arbitrary SGI.
 *
 * Send an arbitrary SGI (core-specific interrupt 0-15) to the specified target core.
 *
 * @param target The target core ID.
 * @param irq The SGI number (0-15).
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugSendIPI(seL4_Uint8 target, unsigned irq);
#endif
#endif

#ifdef CONFIG_DANGEROUS_CODE_INJECTION
/**
 * @xmlonly <manual name="Run" label="sel4_debugrun"/> @endxmlonly
 * @brief Run a user level function in kernel mode.
 *
 * This extremely dangerous function is for running benchmarking and debugging code that
 * needs to be executed in kernel mode from userlevel. It should never be used in a release kernel.
 * This works because the kernel can access all user mappings of device memory, and does not switch page directories
 * on kernel entry.
 *
 * Unlike the other system calls in this section, `seL4_DebugRun` does not
 * depend on the `DEBUG_BUILD` configuration option, but its own config
 * variable `DANGEROUS_CODE_INJECTION`.
 *
 * @param userfn The address in userspace of the function to run.
 * @param userarg A single argument to pass to the function.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_DebugRun(void (* userfn)(void *), void *userarg);
#endif
/** @} */

/**
 * @defgroup BenchmarkingSystemCalls
 * This section documents system calls available when the kernel is
 * configured with benchmarking enabled.
 * There are several different benchmarking modes which can be configured
 * when building the kernel:
 *     1. `BENCHMARK_TRACEPOINTS`: Enable using tracepoints in the kernel and timing code.
 *     2. `BENCHMARK_TRACK_KERNEL_ENTRIES`: Keep track of information on kernel entries.
 *     3. `BENCHMARK_TRACK_UTILISATION`: Allow users to get CPU timing info for the system, threads and/or idle thread.
 *
 * `BENCHMARK_TRACEPOINTS` and `BENCHMARK_TRACK_KERNEL_ENTRIES` use a log buffer that has to be allocated by the user and mapped
 * to a fixed location in the kernel window.
 * All of timing information is output in cycles.
 *
 * @{
 */
#ifdef CONFIG_ENABLE_BENCHMARKS
/*
  */

/**
 * @xmlonly <manual name="Reset Log" label="sel4_benchmarkresetlog"/> @endxmlonly
 * @brief Reset benchmark logging.
 *
 * The behaviour of this system call depends on benchmarking mode in action while invoking
 * this system call:
 *    1. `BENCHMARK_TRACEPOINTS`: resets the log index to 0,
 *    2. `BENCHMARK_TRACK_KERNEL_ENTRIES`:  as above,
 *    3. `BENCHMARK_TRACK_UTILISATION`: resets benchmark and current thread
 *        start time (to the time of invoking this syscall), resets idle
 *        thread utilisation to 0, and starts tracking utilisation.
 *
 * @return A `seL4_Error` error if the user-level log buffer has not been set by the user
 *                         (`BENCHMARK_TRACEPOINTS`/`BENCHMARK_TRACK_KERNEL_ENTRIES`).
 */
LIBSEL4_INLINE_FUNC seL4_Error
seL4_BenchmarkResetLog(void);

/**
 * @xmlonly <manual name="Finalize Log" label="sel4_benchmarkfinalizelog"/> @endxmlonly
 * @brief Stop benchmark logging.
 *
 * The behaviour of this system call depends on benchmarking mode in action while invoking this system call:
 *    1. `BENCHMARK_TRACEPOINTS`: Sets the final log buffer index to the current index,
 *    2. `BENCHMARK_TRACK_KERNEL_ENTRIES`:  as above,
 *    3. `BENCHMARK_TRACK_UTILISATION`: sets benchmark end time to current time, stops tracking utilisation.
 *
 * @return The index of the final entry in the log buffer (if `BENCHMARK_TRACEPOINTS`/`BENCHMARK_TRACK_KERNEL_ENTRIES` are enabled).
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
 * @return A `seL4_IllegalOperation` error if `frame_cptr` is not valid and couldn't set the buffer.
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
 * @brief Flush hardware caches.
 *
 * Flush all possible hardware caches for this platform.
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkFlushCaches(void);

#ifdef CONFIG_ARCH_ARM
/**
 * @xmlonly <manual name="Flush L1 Caches" label="sel4_benchmarkflushl1caches"/> @endxmlonly
 * @brief Flush L1 caches.
 *
 * Flush L1 caches for this platform (currently only support for ARM). Allow to specify the cache type
 * to be flushed (i.e. instruction cache only, data cache only and both instruction cache and data cache).
 *
 * @param[in] cache_type L1 Cache Type to be flushed
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkFlushL1Caches(seL4_Word cache_type);
#endif

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

#ifdef CONFIG_DEBUG_BUILD
/**
 * @xmlonly <manual name="Dump All Threads Utilisation" label="sel4_benchmarkdumpallthreadsutilisation"/> @endxmlonly
 * @brief Print the current accumulated cycle count for every thread on the current node.
 *
 *  Uses kernel's printf to print number of cycles on each line in the following format: thread_name,thread_cycles
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkDumpAllThreadsUtilisation(void);

/**
 * @xmlonly <manual name="Reset All Threads Utilisation" label="sel4_benchmarkresetallthreadsutilisation"/> @endxmlonly
 * @brief Reset the accumulated cycle count for every thread on the current node.
 *
 *  Reset the cycle count for each thread to 0.
 *
 */
LIBSEL4_INLINE_FUNC void
seL4_BenchmarkResetAllThreadsUtilisation(void);

#endif
#endif
#endif
/** @} */

#ifdef CONFIG_ARCH_X86
/**
 * @defgroup X86SystemCalls X86 System Calls
 * @{
 */

#ifdef CONFIG_VTX
/**
 * @xmlonly <manual name="VMEnter" label="sel4_vmenter"/> @endxmlonly
 * @brief Change current thread to execute from its bound VCPU
 *
 * Changes the execution mode of the current thread from normal TCB execution, to
 * guest execution using its bound VCPU.
 * @xmlonly
 * <docref>For details on VCPUs and execution modes see <autoref label="sec:virt"/>.</docref>
 * @endxmlonly
 *
 * Invoking `seL4_VMEnter` is similar to replying to a fault in that updates to the registers
 * can be given in the message, but unlike a fault no message info
 * @xmlonly
 * <docref>(see <autoref label="sec:messageinfo"/>)</docref>
 * @endxmlonly
 * is sent as the registers are not optional and the number that must be sent is fixed.
 * The mapping of hardware register to message register is
 *      - `SEL4_VMENTER_CALL_EIP_MR` Address to start executing instructions at in the guest mode
 *      - `SEL4_VMENTER_CALL_CONTROL_PPC_MR` New value for the Primary Processor Based VM Execution Controls
 *      - `SEL4_VMENTER_CALL_CONTROL_ENTRY_MR` New value for the VM Entry Controls
 *
 * On return these same three message registers will be filled with the values at the point
 * that the privlidged mode ceased executing. If this function returns with `SEL4_VMENTER_RESULT_FAULT`
 * then the following additional message registers will be filled out
 *      - `SEL4_VMENTER_FAULT_REASON_MR`
 *      - `SEL4_VMENTER_FAULT_QUALIFICATION_MR`
 *      - `SEL4_VMENTER_FAULT_INSTRUCTION_LEN_MR`
 *      - `SEL4_VMENTER_FAULT_GUEST_PHYSICAL_MR`
 *      - `SEL4_VMENTER_FAULT_RFLAGS_MR`
 *      - `SEL4_VMENTER_FAULT_GUEST_INT_MR`
 *      - `SEL4_VMENTER_FAULT_CR3_MR`
 *      - `SEL4_VMENTER_FAULT_EAX`
 *      - `SEL4_VMENTER_FAULT_EBX`
 *      - `SEL4_VMENTER_FAULT_ECX`
 *      - `SEL4_VMENTER_FAULT_EDX`
 *      - `SEL4_VMENTER_FAULT_ESI`
 *      - `SEL4_VMENTER_FAULT_EDI`
 *      - `SEL4_VMENTER_FAULT_EBP`
 *
 * @param[out] sender The address to write sender information to.
 *               If the syscall returns due to receiving a notification
 *               on the bound notification then the sender information
 *               is the badge of the notification capability that was invoked.
 *               This parameter is ignored if `NULL`.
 * @return `SEL4_VMENTER_RESULT_NOTIF` if a notification was received or `SEL4_VMENTER_RESULT_FAULT`
 *  if the guest mode execution faulted for any reason
 */
LIBSEL4_INLINE_FUNC seL4_Word
seL4_VMEnter(seL4_Word *sender);
#endif

/** @} */
#endif

/** @} */

#ifdef CONFIG_SET_TLS_BASE_SELF
/**
 * @xmlonly <manual name="SetTLSBase" label="sel4_settlsbase"/> @endxmlonly
 * @brief Set the TLS base address and register of the currently executing thread.
 *
 * This stores the base address of the TLS region in the register
 * reserved for that purpose on the given platform.
 *
 * Each platform has a specific register reserved for tracking the
 * base address of the TLS region (as sepcified in the ELF standard) in
 * a manner compatible with the TLS method used with that architecture.
 *
 * @param tls_base The new base address to store in the register.
 */
LIBSEL4_INLINE_FUNC void
seL4_SetTLSBase(seL4_Word tls_base);
#endif

