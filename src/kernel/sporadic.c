/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
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
        assert(REFILL_INDEX(sc, current).rTime <= REFILL_INDEX(sc, next).rTime);
        current = next;
        next = refill_next(sc, current);
    }
}

#define REFILL_SANITY_START(sc) ticks_t _sum = refill_sum(sc); refill_ordered(sc);
#define REFILL_SANITY_CHECK(sc, budget) \
    do { \
        assert(refill_sum(sc) == budget); refill_ordered(sc); \
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
static ticks_t refill_sum(sched_context_t *sc)
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
    /* cannot add an empty refill */
    assert(refill.rAmount != 0);
    /* cannot add beyond queue size */
    assert(refill_size(sc) < sc->scRefillMax);

    word_t new_tail = refill_next(sc, sc->scRefillTail);
    sc->scRefillTail = new_tail;
    REFILL_TAIL(sc) = refill;

    /* sanity */
    assert(new_tail < sc->scRefillMax);
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
    REFILL_SANITY_CHECK(sc, budget);
}

void refill_update(sched_context_t *sc, ticks_t new_period, ticks_t new_budget, word_t new_max_refills)
{

    /* refill must be initialised in order to be updated - otherwise refill_new should be used */
    assert(sc->scRefillMax > 0);

    /* figure out how much budget is available */
    ticks_t total_budget = refill_sum(sc);
    REFILL_SANITY_CHECK(sc, total_budget);

    /* first deal with a difference in max refills - merge
     * any refills that exceed the new max */
    while (new_max_refills < refill_size(sc)) {
        /* merge refills */

        assert(!refill_single(sc));
        refill_t refill = refill_pop_head(sc);
        REFILL_HEAD(sc).rAmount += refill.rAmount;
    }

    REFILL_SANITY_CHECK(sc, total_budget);

    /* move anything in the list that is beyond the old max */
    if (sc->scRefillMax > new_max_refills) {
        word_t curr = sc->scRefillHead;
        for (curr = sc->scRefillHead; curr < sc->scRefillMax; curr++) {
            word_t diff = sc->scRefillMax - new_max_refills;
            REFILL_INDEX(sc, curr - diff) = REFILL_INDEX(sc, curr);
        }
    }
    sc->scRefillMax = new_max_refills;

    /* now deal with the period change - update each refill by the difference in period */
    word_t current = refill_next(sc, sc->scRefillHead);
    while (current != sc->scRefillTail) {
        /* adjust the period of each refill by new one  (except the head) */
        REFILL_INDEX(sc, current).rTime += (new_period - sc->scPeriod);
        current = refill_next(sc, current);
    }
    sc->scPeriod = new_period;
    REFILL_SANITY_CHECK(sc, total_budget);

    /* now deal with the new budget */
    if (new_budget > total_budget) {
        /* if the budget has increased, just add it to the last refill */
        REFILL_TAIL(sc).rAmount += (new_budget - total_budget);
    } else {
        /* if the budget has decreased, iterate through from head to
         * tail until the amount decreased has been removed from the refill
         * buffer */
        ticks_t remove = total_budget - new_budget;
        while (remove >= REFILL_HEAD(sc).rAmount) {
            assert(!refill_single(sc));
            refill_t old_head = refill_pop_head(sc);
            remove -= old_head.rAmount;
        }
        REFILL_HEAD(sc).rAmount -= remove;
        if (REFILL_HEAD(sc).rAmount < MIN_BUDGET) {
            assert(!refill_single(sc));
            refill_t old_head = refill_pop_head(sc);
            REFILL_HEAD(sc).rAmount += old_head.rAmount;
        }
    }

    /* merge any overlapping refills */
    refill_unblock_check(sc);
    REFILL_SANITY_CHECK(sc, new_budget);
}

ticks_t refill_budget_check(sched_context_t *sc, ticks_t usage)
{
    /* this function should only be called when the sc is out of budget */
    assert(refill_capacity(sc, usage) == 0);
    REFILL_SANITY_START(sc);

    while (REFILL_HEAD(sc).rAmount <= usage) {
        /* exhaust and schedule replenishment */
        usage -= REFILL_HEAD(sc).rAmount;
        if (refill_single(sc)) {
            /* update in place */
            REFILL_HEAD(sc).rTime += sc->scPeriod;
        } else {
            refill_t old_head = refill_pop_head(sc);
            old_head.rTime = old_head.rTime + sc->scPeriod;
            refill_add_tail(sc, old_head);
        }
    }

    /* budget overrun */
    if (usage > 0 && sc->scPeriod > 0) {
        /* budget reduced when calculating capacity */
        /* due to overrun delay next replenishment */
        REFILL_HEAD(sc).rTime += usage;
        /* merge front two replenishments if times overlap */
        if (!refill_single(sc) &&
            REFILL_HEAD(sc).rTime + REFILL_HEAD(sc).rAmount >=
            REFILL_INDEX(sc, refill_next(sc, sc->scRefillHead)).rTime) {

            refill_t refill = refill_pop_head(sc);
            REFILL_HEAD(sc).rAmount += refill.rAmount;
        }
    }

    REFILL_SANITY_END(sc);

    /* return any usage we haven't dealt with */
    return usage;
}

void refill_split_check(sched_context_t *sc, ticks_t usage)
{
    /* invalid to call this on a NULL sc */
    assert(sc != NULL);
    /* something is seriously wrong if this is called and no
     * time has been used */
    assert(usage > 0);
    assert(usage <= REFILL_HEAD(sc).rAmount);

    REFILL_SANITY_START(sc);

    /* first deal with the remaining budget of the current replenishment */
    ticks_t remnant = REFILL_HEAD(sc).rAmount - usage;
    if (remnant < MIN_BUDGET && refill_single(sc)) {
        /* delay entire replenishment - can't merge, nothing to merge with */
        REFILL_HEAD(sc).rTime += sc->scPeriod;
        REFILL_SANITY_END(sc);
        return;
    }

    if (refill_size(sc) == sc->scRefillMax || remnant < MIN_BUDGET) {
        assert(!refill_single(sc));
        /* merge remnant with next replenishment - either it's too small
         * or we're out of space */
        refill_pop_head(sc);
        REFILL_HEAD(sc).rAmount += remnant;
    } else  {
        assert(remnant >= MIN_BUDGET);
        /* split the head refill  */
        REFILL_HEAD(sc).rAmount = remnant;
    }

    /* schedule the used amount */
    refill_t new = (refill_t) {
        .rAmount = usage, .rTime = REFILL_HEAD(sc).rTime + sc->scPeriod
    };
    refill_add_tail(sc, new);
    REFILL_SANITY_END(sc);
}

void refill_unblock_check(sched_context_t *sc)
{
    /* advance earliest activation time to now */
    REFILL_SANITY_START(sc);
    if (refill_ready(sc)) {
        REFILL_HEAD(sc).rTime = NODE_STATE(ksCurTime);

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

        /* it's possible that a refill is not bigger than min budget (if a task
         * uses less than min budget, it will still be scheduled for refill), if
         * so merge with the next refill, as it's not enough to schedule the task. */
        if (!refill_sufficient(sc, 0)) {
            assert(!refill_single(sc));
            refill_t insufficient = refill_pop_head(sc);
            REFILL_HEAD(sc).rAmount += insufficient.rAmount;
        }
    }
    REFILL_SANITY_END(sc);
}
