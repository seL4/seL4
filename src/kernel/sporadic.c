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
    printf("Head %lu length %lu\n", sc->scRefillHead, sc->scRefillCount);

    word_t current = sc->scRefillHead;
    word_t seen = 0;
    while (seen != sc->scRefillCount) {
        print_index(sc, current);
        current = refill_next(sc, current);
        seen += 1;
    }

}
#endif /* CONFIG_PRINTING */
#ifdef CONFIG_DEBUG_BUILD
/* check a refill queue is ordered correctly */
static UNUSED bool_t refill_ordered(sched_context_t *sc)
{
    word_t current = sc->scRefillHead;
    word_t next = refill_next(sc, sc->scRefillHead);
    word_t seen = 1;

    while (seen != sc->scRefillCount) {
        if (!(REFILL_INDEX(sc, current).rTime <= REFILL_INDEX(sc, next).rTime)) {
            refill_print(sc);
            return false;
        }
        current = next;
        next = refill_next(sc, current);
        seen += 1;
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
    ticks_t sum = 0;
    word_t current = sc->scRefillHead;
    word_t seen = 0;

    while (seen < sc->scRefillCount) {
        sum += REFILL_INDEX(sc, current).rAmount;
        current = refill_next(sc, current);
        seen += 1;
    }

    return sum;
}

/* pop head of refill queue */
static inline refill_t refill_pop_head(sched_context_t *sc)
{
    /* queues cannot be empty */
    assert(!refill_empty(sc));

    UNUSED word_t prev_size = refill_size(sc);
    refill_t refill = REFILL_HEAD(sc);
    sc->scRefillHead = refill_next(sc, sc->scRefillHead);
    sc->scRefillCount -= 1;

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

    sc->scRefillCount += 1;
    REFILL_TAIL(sc) = refill;
}

#ifdef ENABLE_SMP_SUPPORT
void refill_new(sched_context_t *sc, word_t max_refills, ticks_t budget, ticks_t period, word_t core)
#else
void refill_new(sched_context_t *sc, word_t max_refills, ticks_t budget, ticks_t period)
#endif
{
    sc->scPeriod = period;
    sc->scRefillHead = 0;
    sc->scRefillCount = 1;
    sc->scRefillMax = max_refills;
    assert(budget > MIN_BUDGET);
    /* full budget available */
    REFILL_HEAD(sc).rAmount = budget;
    /* budget can be used from now */
    if (period == 0) {
        REFILL_HEAD(sc).rTime = 0;
    } else {
        REFILL_HEAD(sc).rTime = NODE_STATE_ON_CORE(ksCurTime, core);
    }
    REFILL_SANITY_CHECK(sc, budget);
}

static inline void schedule_used(sched_context_t *sc, refill_t new)
{
    if (refill_empty(sc)) {
        assert(new.rAmount >= MIN_BUDGET);
        refill_add_tail(sc, new);
    } else {
        /* We should never try to append overlapping refills to the tail */
        assert(new.rTime >= REFILL_TAIL(sc).rTime + REFILL_TAIL(sc).rAmount);

        /* schedule the used amount */
        if (new.rAmount < MIN_BUDGET && !refill_full(sc) && REFILL_TAIL(sc).rAmount + new.rAmount >= 2 * MIN_BUDGET) {
            /* Split tail into two parts of at least MIN_BUDGET */
            ticks_t remainder = MIN_BUDGET - new.rAmount;
            new.rAmount += remainder;
            new.rTime -= remainder;
            REFILL_TAIL(sc).rAmount -= remainder;
            refill_add_tail(sc, new);
        } else if (new.rAmount < MIN_BUDGET || refill_full(sc)) {
            /* Merge with existing tail */
            REFILL_TAIL(sc).rTime = new.rTime - REFILL_TAIL(sc).rAmount;
            REFILL_TAIL(sc).rAmount += new.rAmount;
        } else {
            refill_add_tail(sc, new);
        }
    }

    assert(!refill_empty(sc));
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
    sc->scRefillCount = 1;
    /* update max refills */
    sc->scRefillMax = new_max_refills;
    /* update period */
    sc->scPeriod = new_period;

    if (new_period == 0) {
        REFILL_HEAD(sc).rTime = 0;
    } else if (refill_ready(sc)) {
        REFILL_HEAD(sc).rTime = NODE_STATE_ON_CORE(ksCurTime, sc->scCore);
    }

    if (REFILL_HEAD(sc).rAmount >= new_budget) {
        /* if the heads budget exceeds the new budget just trim it */
        REFILL_HEAD(sc).rAmount = new_budget;
    } else {
        /* otherwise schedule the rest for the next period */
        ticks_t unused = new_budget - REFILL_HEAD(sc).rAmount;
        ticks_t true_period = MAX(new_period, new_budget);
        refill_t new = { .rAmount = unused,
                         .rTime = REFILL_HEAD(sc).rTime + true_period - unused,
                       };
        schedule_used(sc, new);
    }

    REFILL_SANITY_CHECK(sc, new_budget);
}

void refill_budget_check_round_robin(ticks_t usage)
{
    sched_context_t *sc = NODE_STATE(ksCurSC);
    assert(isRoundRobin(sc));
    REFILL_SANITY_START(sc);

    if (usage < MIN_BUDGET && refill_single(sc)) {
        /* If a new refill is created it will be at least MIN_BUDGET. */
        usage = MIN_BUDGET;
    }

    /* If the following check is false then it has already been
     * determined that this thread will be placed at the back of its
     * ready queue by the time the kernel returns.
     *
     * Whenever a round-robin thread is moved to the back of its ready
     * queue it should have a single refill with its entire budget.
     */
    if (REFILL_HEAD(sc).rAmount >= usage + MIN_BUDGET) {
        /* The amount left in the head must be at least MIN_BUDGET. */
        REFILL_HEAD(sc).rAmount -= usage;
        /* Consume only a portion of the head refill */
        if (refill_single(sc)) {
            assert(REFILL_HEAD(sc).rTime == 0);
            refill_t new = {
                .rTime = REFILL_HEAD(sc).rAmount,
                .rAmount = usage,
            };
            refill_add_tail(sc, new);
            assert(refill_ordered(sc));
        } else {
            assert(sc->scRefillCount == 2);
            REFILL_TAIL(sc).rTime -= usage;
            REFILL_TAIL(sc).rAmount += usage;
        }
    } else if (!refill_single(sc)) {
        /* Reset to a single refill */
        REFILL_HEAD(sc).rAmount += REFILL_TAIL(sc).rAmount;
        sc->scRefillCount = 1;
    }

    assert(REFILL_HEAD(sc).rTime == 0);
    REFILL_SANITY_END(sc);
    return;
}

void refill_budget_check(ticks_t usage)
{
    sched_context_t *sc = NODE_STATE(ksCurSC);
    /* this function should only be called when the sc is out of budget */
    assert(sc->scPeriod > 0);
    REFILL_SANITY_START(sc);

    while (unlikely(REFILL_HEAD(sc).rAmount <= usage)) {
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

    if (usage > 0) {
        refill_split_check(usage);
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

    /* set up a new replenishment structure */
    refill_t new = (refill_t) {
        .rAmount = usage, .rTime = REFILL_HEAD(sc).rTime + sc->scPeriod
    };

    if (REFILL_HEAD(sc).rAmount >= MIN_BUDGET + usage) {
        /* Split the head to keep the remaining time in a refill*/
        REFILL_HEAD(sc).rTime += usage;
        REFILL_HEAD(sc).rAmount -= usage;
    } else {
        assert(REFILL_HEAD(sc).rAmount >= usage);
        /* merge the remaining time into the following refill */
        refill_t head = refill_pop_head(sc);
        ticks_t remnant = head.rAmount - usage;
        if (!refill_empty(sc)) {
            REFILL_HEAD(sc).rTime -= remnant;
            REFILL_HEAD(sc).rAmount += remnant;
        } else {
            /* used is the subsequent refill */
            new.rTime -= remnant;
            new.rAmount += remnant;
        }
    }

    /* Schedule all of the used time as a single refill. */
    schedule_used(sc, new);

    REFILL_SANITY_END(sc);
}


static bool_t refill_unblock_check_mergable(sched_context_t *sc)
{
    ticks_t amount = REFILL_HEAD(sc).rAmount;
    ticks_t tail = NODE_STATE_ON_CORE(ksCurTime, sc->scCore) + amount;
    bool_t enough_time = REFILL_INDEX(sc, refill_next(sc, sc->scRefillHead)).rTime <= tail;
    return !refill_single(sc) && enough_time;
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
        REFILL_HEAD(sc).rTime = NODE_STATE_ON_CORE(ksCurTime, sc->scCore);
        NODE_STATE(ksReprogram) = true;

        /* merge available replenishments */
        while (refill_unblock_check_mergable(sc)) {
            ticks_t amount = REFILL_HEAD(sc).rAmount;
            refill_pop_head(sc);
            REFILL_HEAD(sc).rAmount += amount;
            REFILL_HEAD(sc).rTime = NODE_STATE_ON_CORE(ksCurTime, sc->scCore);
        }

        assert(refill_sufficient(sc, 0));
    }
    REFILL_SANITY_END(sc);
}
