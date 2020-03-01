/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

/* functions to manage the circular buffer of
 * sporadic budget replenishments (refills for short).
 *
 * The circular buffer always has at least one item in it.
 *
 * Items are appended at the tail (the back) and
 * removed from the head (the front). Below is
 * an example of a queue with 4 items (h = head, t = tail, x = item, [] = slot)
 * and max size 8.
 *
 * [][h][x][x][t][][][]
 *
 * and another example of a queue with 5 items
 *
 * [x][t][][][][h][x][x]
 *
 * The queue has a minimum size of 1, so it is possible that h == t.
 *
 * The queue is implemented as head + tail rather than head + size as
 * we cannot use the mod operator on all architectures without accessing
 * the fpu or implementing divide.
 */

/* return the index of the next item in the refill queue */
static inline word_t refill_next(sched_context_t *sc, word_t index)
{
    return (index == sc->scRefillMax - 1u) ? (0) : index + 1u;
}

#ifdef CONFIG_PRINTING
/* for debugging */
UNUSED static inline void print_index(sched_context_t *sc, word_t index)
{

    printf("index %lu, Amount: %llx, time %llx\n", index, REFILL_INDEX(sc, index).rAmount,
           REFILL_INDEX(sc, index).rTime);
}

UNUSED static inline void refill_print(sched_context_t *sc)
{
    printf("Head %lu tail %lu\n", sc->scRefillHead, sc->scRefillTail);
    word_t current = sc->scRefillHead;
    /* always print the head */
    print_index(sc, current);

    while (current != sc->scRefillTail) {
        current = refill_next(sc, current);
        print_index(sc, current);
    }

}
#endif /* CONFIG_PRINTING */
#ifdef CONFIG_DEBUG_BUILD
/* check a refill queue is ordered correctly */
static UNUSED bool_t refill_ordered(sched_context_t *sc)
{
    word_t current = sc->scRefillHead;
    word_t next = refill_next(sc, sc->scRefillHead);

    while (current != sc->scRefillTail) {
        if (!(REFILL_INDEX(sc, current).rTime <= REFILL_INDEX(sc, next).rTime)) {
            refill_print(sc);
            return false;
        }
        current = next;
        next = refill_next(sc, current);
    }

    return true;
}

#define REFILL_SANITY_START(sc) ticks_t _sum = refill_sum(sc); assert(refill_ordered(sc));
#define REFILL_SANITY_CHECK(sc, budget) \
    do { \
        assert(refill_sum(sc) == budget); assert(refill_ordered(sc)); \
    } while (0)

#define REFILL_SANITY_END(sc) \
    do {\
        REFILL_SANITY_CHECK(sc, _sum);\
    } while (0)
#else
#define REFILL_SANITY_START(sc)
#define REFILL_SANITY_CHECK(sc, budget)
#define REFILL_SANITY_END(sc)
#endif /* CONFIG_DEBUG_BUILD */

/* compute the sum of a refill queue */
static UNUSED ticks_t refill_sum(sched_context_t *sc)
{
    ticks_t sum = REFILL_HEAD(sc).rAmount;
    word_t current = sc->scRefillHead;

    while (current != sc->scRefillTail) {
        current = refill_next(sc, current);
        sum += REFILL_INDEX(sc, current).rAmount;
    }

    return sum;
}

/* pop head of refill queue */
static inline refill_t refill_pop_head(sched_context_t *sc)
{
    /* queues cannot be smaller than 1 */
    assert(!refill_single(sc));

    UNUSED word_t prev_size = refill_size(sc);
    refill_t refill = REFILL_HEAD(sc);
    sc->scRefillHead = refill_next(sc, sc->scRefillHead);

    /* sanity */
    assert(prev_size == (refill_size(sc) + 1));
    assert(sc->scRefillHead < sc->scRefillMax);
    return refill;
}

/* add item to tail of refill queue */
static inline void refill_add_tail(sched_context_t *sc, refill_t refill)
{
    /* cannot add beyond queue size */
    assert(refill_size(sc) < sc->scRefillMax);

    word_t new_tail = refill_next(sc, sc->scRefillTail);
    sc->scRefillTail = new_tail;
    REFILL_TAIL(sc) = refill;

    /* sanity */
    assert(new_tail < sc->scRefillMax);
}

static inline void maybe_add_empty_tail(sched_context_t *sc)
{
    if (isRoundRobin(sc)) {
        /* add an empty refill - we track the used up time here */
        refill_t empty_tail = { .rTime = NODE_STATE(ksCurTime)};
        refill_add_tail(sc, empty_tail);
        assert(refill_size(sc) == MIN_REFILLS);
    }
}

void refill_new(sched_context_t *sc, word_t max_refills, ticks_t budget, ticks_t period)
{
    sc->scPeriod = period;
    sc->scRefillHead = 0;
    sc->scRefillTail = 0;
    sc->scRefillMax = max_refills;
    assert(budget > MIN_BUDGET);
    /* full budget available */
    REFILL_HEAD(sc).rAmount = budget;
    /* budget can be used from now */
    REFILL_HEAD(sc).rTime = NODE_STATE(ksCurTime);
    maybe_add_empty_tail(sc);
    REFILL_SANITY_CHECK(sc, budget);
}

void refill_update(sched_context_t *sc, ticks_t new_period, ticks_t new_budget, word_t new_max_refills)
{

    /* refill must be initialised in order to be updated - otherwise refill_new should be used */
    assert(sc->scRefillMax > 0);

    /* this is called on an active thread. We want to preserve the sliding window constraint -
     * so over new_period, new_budget should not be exceeded even temporarily */

    /* move the head refill to the start of the list - it's ok as we're going to truncate the
     * list to size 1 - and this way we can't be in an invalid list position once new_max_refills
     * is updated */
    REFILL_INDEX(sc, 0) = REFILL_HEAD(sc);
    sc->scRefillHead = 0;
    /* truncate refill list to size 1 */
    sc->scRefillTail = sc->scRefillHead;
    /* update max refills */
    sc->scRefillMax = new_max_refills;
    /* update period */
    sc->scPeriod = new_period;

    if (refill_ready(sc)) {
        REFILL_HEAD(sc).rTime = NODE_STATE(ksCurTime);
    }

    if (REFILL_HEAD(sc).rAmount >= new_budget) {
        /* if the heads budget exceeds the new budget just trim it */
        REFILL_HEAD(sc).rAmount = new_budget;
        maybe_add_empty_tail(sc);
    } else {
        /* otherwise schedule the rest for the next period */
        refill_t new = { .rAmount = (new_budget - REFILL_HEAD(sc).rAmount),
                         .rTime = REFILL_HEAD(sc).rTime + new_period
                       };
        refill_add_tail(sc, new);
    }

    REFILL_SANITY_CHECK(sc, new_budget);
}

static inline void schedule_used(sched_context_t *sc, refill_t new)
{
    /* schedule the used amount */
    if (new.rAmount < MIN_BUDGET && !refill_single(sc)) {
        /* used amount is to small - merge with last and delay */
        REFILL_TAIL(sc).rAmount += new.rAmount;
        REFILL_TAIL(sc).rTime = MAX(new.rTime, REFILL_TAIL(sc).rTime);
    } else if (new.rTime <= REFILL_TAIL(sc).rTime) {
        REFILL_TAIL(sc).rAmount += new.rAmount;
    } else {
        refill_add_tail(sc, new);
    }
}

void refill_budget_check(ticks_t usage, ticks_t capacity)
{
    sched_context_t *sc = NODE_STATE(ksCurSC);
    /* this function should only be called when the sc is out of budget */
    assert(capacity < MIN_BUDGET || refill_full(sc));
    assert(sc->scPeriod > 0);
    REFILL_SANITY_START(sc);

    if (capacity == 0) {
        while (REFILL_HEAD(sc).rAmount <= usage) {
            /* exhaust and schedule replenishment */
            usage -= REFILL_HEAD(sc).rAmount;
            if (refill_single(sc)) {
                /* update in place */
                REFILL_HEAD(sc).rTime += sc->scPeriod;
            } else {
                refill_t old_head = refill_pop_head(sc);
                old_head.rTime = old_head.rTime + sc->scPeriod;
                schedule_used(sc, old_head);
            }
        }

        /* budget overrun */
        if (usage > 0) {
            /* budget reduced when calculating capacity */
            /* due to overrun delay next replenishment */
            REFILL_HEAD(sc).rTime += usage;
            /* merge front two replenishments if times overlap */
            if (!refill_single(sc) &&
                REFILL_HEAD(sc).rTime + REFILL_HEAD(sc).rAmount >=
                REFILL_INDEX(sc, refill_next(sc, sc->scRefillHead)).rTime) {

                refill_t refill = refill_pop_head(sc);
                REFILL_HEAD(sc).rAmount += refill.rAmount;
                REFILL_HEAD(sc).rTime = refill.rTime;
            }
        }
    }

    capacity = refill_capacity(sc, usage);
    if (capacity > 0 && refill_ready(sc)) {
        refill_split_check(usage);
    }

    /* ensure the refill head is sufficient, such that when we wake in awaken,
     * there is enough budget to run */
    while (REFILL_HEAD(sc).rAmount < MIN_BUDGET || refill_full(sc)) {
        refill_t refill = refill_pop_head(sc);
        REFILL_HEAD(sc).rAmount += refill.rAmount;
        /* this loop is guaranteed to terminate as the sum of
         * rAmount in a refill must be >= MIN_BUDGET */
    }

    REFILL_SANITY_END(sc);
}

void refill_split_check(ticks_t usage)
{
    sched_context_t *sc = NODE_STATE(ksCurSC);
    /* invalid to call this on a NULL sc */
    assert(sc != NULL);
    /* something is seriously wrong if this is called and no
     * time has been used */
    assert(usage > 0);
    assert(usage <= REFILL_HEAD(sc).rAmount);
    assert(sc->scPeriod > 0);

    REFILL_SANITY_START(sc);

    /* first deal with the remaining budget of the current replenishment */
    ticks_t remnant = REFILL_HEAD(sc).rAmount - usage;

    /* set up a new replenishment structure */
    refill_t new = (refill_t) {
        .rAmount = usage, .rTime = REFILL_HEAD(sc).rTime + sc->scPeriod
    };

    if (refill_size(sc) == sc->scRefillMax || remnant < MIN_BUDGET) {
        /* merge remnant with next replenishment - either it's too small
         * or we're out of space */
        if (refill_single(sc)) {
            /* update inplace */
            new.rAmount += remnant;
            REFILL_HEAD(sc) = new;
        } else {
            refill_pop_head(sc);
            REFILL_HEAD(sc).rAmount += remnant;
            schedule_used(sc, new);
        }
        assert(refill_ordered(sc));
    } else {
        /* leave remnant as reduced replenishment */
        assert(remnant >= MIN_BUDGET);
        /* split the head refill  */
        REFILL_HEAD(sc).rAmount = remnant;
        schedule_used(sc, new);
    }

    REFILL_SANITY_END(sc);
}

void refill_unblock_check(sched_context_t *sc)
{

    if (isRoundRobin(sc)) {
        /* nothing to do */
        return;
    }

    /* advance earliest activation time to now */
    REFILL_SANITY_START(sc);
    if (refill_ready(sc)) {
        REFILL_HEAD(sc).rTime = NODE_STATE(ksCurTime);
        NODE_STATE(ksReprogram) = true;

        /* merge available replenishments */
        while (!refill_single(sc)) {
            ticks_t amount = REFILL_HEAD(sc).rAmount;
            if (REFILL_INDEX(sc, refill_next(sc, sc->scRefillHead)).rTime <= NODE_STATE(ksCurTime) + amount) {
                refill_pop_head(sc);
                REFILL_HEAD(sc).rAmount += amount;
                REFILL_HEAD(sc).rTime = NODE_STATE(ksCurTime);
            } else {
                break;
            }
        }

        assert(refill_sufficient(sc, 0));
    }
    REFILL_SANITY_END(sc);
}
