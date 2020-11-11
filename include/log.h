/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_KERNEL_DEBUG_LOG_BUFFER
#include <sel4/log.h>
#include <basic_types.h>
#include <arch/benchmark.h>
#include <model/statedata.h>
#include <arch/model/smp.h>

/* The global logbuffer reference used by the kernel */
extern seL4_LogBuffer ksLogBuffer;
extern bool_t ksLogEnabled;

/* Reset the log buffer to start logging at the beginning */
static inline void logBuffer_reset(void)
{
    if (ksLogBuffer.buffer != NULL) {
        ksLogBuffer.index = 0;
        ksLogEnabled = true;
    }
}

/* Initialise the kernel log buffer with a new memory region */
static inline void logBuffer_init(seL4_Word *buffer, seL4_Word words)
{
    ksLogBuffer = seL4_LogBuffer_new(buffer);
    seL4_LogBuffer_setSize(&ksLogBuffer, words);
    logBuffer_reset();
}

/* Finalise the log buffer and ensure no further events will be written. */
static inline word_t logBuffer_finalize(void)
{
    ksLogEnabled = false;
    return ksLogBuffer.index;
}

/* Clear the log buffer if buffer matches the given address */
static inline void logBuffer_maybeClear(seL4_Word *base_addr)
{
    if (ksLogBuffer.buffer == base_addr) {
        logBuffer_reset();
        logBuffer_finalize();
        ksLogBuffer.buffer = NULL;
    }
}

/* Get a reference to an event of a given size to next be written to a log */
static inline seL4_LogEvent *logBuffer_reserveGeneric(seL4_Word type)
{
    word_t length = seL4_LogType_length(type);
    assert(length >= seL4_Log_Length(None));
    word_t remaining = ksLogBuffer.size - ksLogBuffer.index;
    if (ksLogEnabled && remaining >= length) {
        /* Get a reference to the event and initialise */
        seL4_LogEvent *event = seL4_LogBuffer_event(ksLogBuffer, ksLogBuffer.index);

        /* Event lengths are recorded as one less */
        event->type = type;

        /* Advance the log to the next location */
        ksLogBuffer.index += length;

        return event;
    } else {
        /* Insufficient space in log buffer so finalise */
        logBuffer_finalize();
        return NULL;
    }
}

/* Reserve an event in the buffer for a specific event type */
#define logBuffer_reserve(event) \
    (seL4_Log_Cast(event)(logBuffer_reserveGeneric( \
        seL4_Log_TypeId(event) \
    )))\

/*
 * Definition of log events
 * ========================
 *
 * Each log even must have a log function declared below.
 *
 * The data structures used to describe the events within the log buffer
 * are defined in libsel4/include/sel4/log.h
 */

/* Get the function name used to log an event */
#define debugLog_Function(event) seL4_Log_Function_ ## event

/* Log a particular event to the kernel log buffer */
#define debugLog(event, ...) debugLog_Function(event)(__VA_ARGS__)

/* Log a particular event if a condition holds */
#define debugLogIf(event, cond, ...) if (cond) debugLog(event, __VA_ARGS__)

/* Log an empty event */
static inline void debugLog_Function(None)(void)
{
    logBuffer_reserve(None);
}

/* Log a kernel entry */
static inline void debugLog_Function(Entry)(void)
{
#ifdef CONFIG_KERNEL_DEBUG_LOG_ENTRIES
    seL4_Log_Type(Entry) *event = logBuffer_reserve(Entry);
    if (event != NULL) {
        event->header.data = CURRENT_CPU_INDEX();
        event->timestamp = timestamp();
    }
#endif
}

/* Log a kernel exit */
static inline void debugLog_Function(Exit)(void)
{
#ifdef CONFIG_KERNEL_DEBUG_LOG_ENTRIES
    seL4_Log_Type(Exit) *event = logBuffer_reserve(Exit);
    if (event != NULL) {
        event->header.data = CURRENT_CPU_INDEX();
        event->timestamp = timestamp();
    }
#endif
}

#else /* CONFIG_KERNEL_DEBUG_LOG_BUFFER */
/* With logging disabled, any logging functions become no-ops */
#define debugLog(...)
#define debugLogIf(...)
#define ksLogEnabled false
#endif /* !CONFIG_KERNEL_DEBUG_LOG_BUFFER */
