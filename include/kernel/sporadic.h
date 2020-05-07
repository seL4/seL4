/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
/* This header presents the interface for sporadic servers,
 * implemented according to Stankcovich et. al in
 * "Defects of the POSIX Spoardic Server and How to correct them",
 * although without the priority management.
 *
 * Briefly, a sporadic server is a period and a queue of refills. Each
 * refill consists of an amount, and a period. No thread is allowed to consume
 * more than amount ticks per period.
 *
 * The sum of all refill amounts in the refill queue is always the budget of the scheduling context -
 * that is it should never change, unless it is being updated / configured.
 *
 * Every time budget is consumed, that amount of budget is scheduled
 * for reuse in period time. If the refill queue is full (the queue's
 * minimum size is 2, and can be configured by the user per scheduling context
 * above this) the next refill is merged.
 */
#include <config.h>
#include <types.h>
#include <util.h>
#include <object/structures.h>
#include <machine/timer.h>
#include <model/statedata.h>

/* To do an operation in the kernel, the thread must have
 * at least this much budget - see comment on refill_sufficient */
#define MIN_BUDGET_US (2u * getKernelWcetUs() * CONFIG_KERNEL_WCET_SCALE)
#define MIN_BUDGET    (2u * getKernelWcetTicks() * CONFIG_KERNEL_WCET_SCALE)

/* Short hand for accessing refill queue items */
#define REFILL_INDEX(sc, index) (((refill_t *) (SC_REF(sc) + sizeof(sched_context_t)))[index])
#define REFILL_HEAD(sc) REFILL_INDEX((sc), (sc)->scRefillHead)
#define REFILL_TAIL(sc) REFILL_INDEX((sc), (sc)->scRefillTail)


/* Scheduling context objects consist of a sched_context_t at the start, followed by a
 * circular buffer of refills. As scheduling context objects are of variable size, the
 * amount of refill_ts that can fit into a scheduling context object is also variable.
 *
 * @return the maximum number of refill_t data structures that can fit into this
 * specific scheduling context object.
 */
static inline word_t refill_absolute_max(cap_t sc_cap)
{
    return (BIT(cap_sched_context_cap_get_capSCSizeBits(sc_cap)) - sizeof(sched_context_t)) / sizeof(refill_t);
}

/* @return the current amount of empty slots in the refill buffer */
static inline word_t refill_size(sched_context_t *sc)
{
    if (sc->scRefillHead <= sc->scRefillTail) {
        return (sc->scRefillTail - sc->scRefillHead + 1u);
    }
    return sc->scRefillTail + 1u + (sc->scRefillMax - sc->scRefillHead);
}

/* @return true if the circular buffer of refills is current full (all slots in the
 * buffer are currently being used */
static inline bool_t refill_full(sched_context_t *sc)
{
    return refill_size(sc) == sc->scRefillMax;
}

/* @return true if the ciruclar buffer only contains 1 used slot */
static inline bool_t refill_single(sched_context_t *sc)
{
    return sc->scRefillHead == sc->scRefillTail;
}

/* Return the amount of budget this scheduling context
 * has available if usage is charged to it. */
static inline ticks_t refill_capacity(sched_context_t *sc, ticks_t usage)
{
    if (unlikely(usage > REFILL_HEAD(sc).rAmount)) {
        return 0;
    }

    return REFILL_HEAD(sc).rAmount - usage;
}

/*
 * Return true if the head refill has sufficient capacity
 * to enter and exit the kernel after usage is charged to it.
 */
static inline bool_t refill_sufficient(sched_context_t *sc, ticks_t usage)
{
    return refill_capacity(sc, usage) >= MIN_BUDGET;
}

/*
 * Return true if the head refill is eligible to be used.
 * This indicates if the thread bound to the sc can be placed
 * into the scheduler, otherwise it needs to go into the release queue
 * to wait.
 */
static inline bool_t refill_ready(sched_context_t *sc)
{
    return REFILL_HEAD(sc).rTime <= (NODE_STATE_ON_CORE(ksCurTime, sc->scCore) + getKernelWcetTicks());
}

/* Create a new refill in a non-active sc */
#ifdef ENABLE_SMP_SUPPORT
void refill_new(sched_context_t *sc, word_t max_refills, ticks_t budget, ticks_t period, word_t core);
#define REFILL_NEW(sc, max_refills, budget, period, core) refill_new(sc, max_refills, budget, period, core)
#else
void refill_new(sched_context_t *sc, word_t max_refills, ticks_t budget, ticks_t period);
#define REFILL_NEW(sc, max_refills, budget, period, core) refill_new(sc, max_refills, budget, period)
#endif

/* Update refills in an active sc without violating bandwidth constraints */
void refill_update(sched_context_t *sc, ticks_t new_period, ticks_t new_budget, word_t new_max_refills);


/* Charge `usage` to the current scheduling context.
 * This function should only be called only when charging `used` will deplete
 * the head refill, resulting in refill_sufficient failing.
 *
 * @param usage the amount of time to charge.
 * @param capacity the value returned by refill_capacity. At most call sites this
 * has already been calculated so pass the value in rather than calculating it again.
 */
void refill_budget_check(ticks_t used, ticks_t capacity);

/*
 * Charge a the current scheduling context `used` amount from its
 * current refill. This will split the refill, leaving whatever is
 * left over at the head of the refill. This is only called when charging
 * `used` will not deplete the head refill.
 */
void refill_split_check(ticks_t used);

/*
 * This is called when a thread is eligible to start running: it
 * iterates through the refills queue and merges any
 * refills that overlap.
 */
void refill_unblock_check(sched_context_t *sc);

