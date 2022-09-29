/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <sel4/shared_types_gen.h>
#include <arch/api/types.h>
#include <arch/types.h>
#include <sel4/macros.h>
#include <sel4/constants.h>
#include <sel4/shared_types.h>
#include <machine/io.h>

/* seL4_CapRights_t defined in mode/api/shared_types.bf */

typedef word_t prio_t;

/* The kernel uses ticks_t internally to represent time to make it easy to
 * interact with hardware timers. The userland API uses time in micro seconds,
 * which is represented by time_t in the kernel.
 */
typedef uint64_t ticks_t;
typedef uint64_t time_t;

enum domainConstants {
    minDom = 0,
    maxDom = CONFIG_NUM_DOMAINS - 1,
    /* To analyse branches of control flow decisions based on the number of
     * domains without knowing their exact number, verification needs a C name
     * to relate to higher-level specs. */
    numDomains = CONFIG_NUM_DOMAINS
};

struct cap_transfer {
    cptr_t ctReceiveRoot;
    cptr_t ctReceiveIndex;
    word_t ctReceiveDepth;
};
typedef struct cap_transfer cap_transfer_t;

enum ctLimits {
    capTransferDataSize = 3
};

static inline seL4_CapRights_t CONST rightsFromWord(word_t w)
{
    seL4_CapRights_t seL4_CapRights;

    seL4_CapRights.words[0] = w;
    return seL4_CapRights;
}

static inline word_t CONST wordFromRights(seL4_CapRights_t seL4_CapRights)
{
    return seL4_CapRights.words[0] & MASK(seL4_CapRightsBits);
}

static inline cap_transfer_t PURE capTransferFromWords(word_t *wptr)
{
    cap_transfer_t transfer;

    transfer.ctReceiveRoot  = (cptr_t)wptr[0];
    transfer.ctReceiveIndex = (cptr_t)wptr[1];
    transfer.ctReceiveDepth = wptr[2];
    return transfer;
}

static inline seL4_MessageInfo_t CONST messageInfoFromWord_raw(word_t w)
{
    seL4_MessageInfo_t mi;

    mi.words[0] = w;
    return mi;
}

static inline seL4_MessageInfo_t CONST messageInfoFromWord(word_t w)
{
    seL4_MessageInfo_t mi;
    word_t len;

    mi.words[0] = w;

    len = seL4_MessageInfo_get_length(mi);
    if (len > seL4_MsgMaxLength) {
        mi = seL4_MessageInfo_set_length(mi, seL4_MsgMaxLength);
    }

    return mi;
}

static inline word_t CONST wordFromMessageInfo(seL4_MessageInfo_t mi)
{
    return mi.words[0];
}

#ifdef CONFIG_PRINTING
#ifdef CONFIG_COLOUR_PRINTING
#define ANSI_RESET "\033[0m"
#define ANSI_GREEN ANSI_RESET "\033[32m"
#define ANSI_DARK  ANSI_RESET "\033[30;1m"
#else
#define ANSI_RESET ""
#define ANSI_GREEN ANSI_RESET ""
#define ANSI_DARK  ANSI_RESET ""
#endif

/*
 * thread name is only available if the kernel is built in debug mode.
 */
#ifdef CONFIG_DEBUG_BUILD
#define THREAD_NAME TCB_PTR_DEBUG_PTR(NODE_STATE(ksCurThread))->tcbName
#else
#define THREAD_NAME ""
#endif

#ifdef CONFIG_KERNEL_INVOCATION_REPORT_ERROR_IPC
extern struct debug_syscall_error current_debug_error;

#define out_error(...) \
    snprintf((char *)current_debug_error.errorMessage, \
            DEBUG_MESSAGE_MAXLEN * sizeof(word_t), __VA_ARGS__);
#else
#define out_error printf
#endif

/*
 * Print to serial a message helping userspace programmers to determine why the
 * kernel is not performing their requested operation.
 */
#define userError(M, ...) \
    do {                                                                       \
        out_error(ANSI_DARK "<<" ANSI_GREEN "seL4(CPU %" SEL4_PRIu_word ")"    \
                ANSI_DARK " [%s/%d T%p \"%s\" @%lx]: " M ">>" ANSI_RESET "\n", \
                CURRENT_CPU_INDEX(),                                           \
                __func__, __LINE__, NODE_STATE(ksCurThread),                   \
                THREAD_NAME,                                                   \
                (word_t)getRestartPC(NODE_STATE(ksCurThread)),                 \
                ## __VA_ARGS__);                                               \
    } while (0)
#else /* !CONFIG_PRINTING */
#define userError(...)
#endif

