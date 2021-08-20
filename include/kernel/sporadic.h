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
#if (CONFIG_KERNEL_STATIC_MAX_PERIOD_US) != 0
#define MAX_PERIOD_US (CONFIG_KERNEL_STATIC_MAX_PERIOD_US)
#else
/* The maximum period determines the point at which the scheduling logic
 * will no longer function correctly (UINT64_MAX - 5 * MAX_PERIOD), so
 * we keep the maximum period relatively small to ensure that the system
 * can function for a reasonably long time.
 *
 * Anything below getMaxUsToTicks() / 8 would ensure that time up to
 * 2^63 would still be be valid as 5 * (getMaxUsToTicks()) must be less
 * than 2^62. */
#define MAX_PERIOD_US (getMaxUsToTicks() / 8)
#endif /* CONFIG_KERNEL_STATIC_MAX_PERIOD_US != 0 */
#define MAX_RELEASE_TIME (UINT64_MAX - 5 * usToTicks(MAX_PERIOD_US))

/* Short hand for accessing refill queue items */
static inline refill_t *refill_index(sched_context_t *sc, word_t index)
{
    return ((refill_t *)(SC_REF(sc) + sizeof(sched_context_t))) + index;
}
static inline refill_t *refill_head(sched_context_t *sc)
{
    return refill_index(sc, sc->scRefillHead);
}
static inline refill_t *refill_tail(sched_context_t *sc)
{
    return refill_index(sc, sc->scRefillTail);
}


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
    if (unlikely(usage > refill_head(sc)->rAmount)) {
        return 0;
    }

    return refill_head(sc)->rAmount - usage;
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
    return refill_head(sc)->rTime <= (NODE_STATE_ON_CORE(ksCurTime, sc->scCore) + getKernelWcetTicks());
}

/*
 * Return true if an SC has been successfully configured with parameters
 * that allow for a thread to run.
 */
static inline bool_t sc_active(sched_context_t *sc)
{
    return sc->scRefillMax > 0;
}

/*
 * Return true if a SC has been 'released', if its head refill is
 * sufficient and is in the past.
 */
static inline bool_t sc_released(sched_context_t *sc)
{
    if (sc_active(sc)) {
        /* All refills must all be greater than MIN_BUDGET so this
         * should be true for all active SCs */
        assert(refill_sufficient(sc, 0));
        return refill_ready(sc);
    } else {
        return false;
    }
}

/*
 * Return true if a SC's available refills should be delayed at the
 * point the associated thread becomes runnable (sporadic server).
 */
static inline bool_t sc_sporadic(sched_context_t *sc)
{
    return sc != NULL && sc_active(sc) && sc->scSporadic;
}

/*
 * Return true if a SC's available refills should be delayed at the
 * point the associated thread becomes the current thread (constant
 * bandwidth).
 */
static inline bool_t sc_constant_bandwidth(sched_context_t *sc)
{
    return !sc->scSporadic;
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
 */
void refill_budget_check(ticks_t used);

/*
 * This is called when a thread is eligible to start running: it
 * iterates through the refills queue and merges any
 * refills that overlap.
 */
void refill_unblock_check(sched_context_t *sc);

