/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#ifndef __OBJECT_SCHED_CONTEXT_H
#define __OBJECT_SCHED_CONTEXT_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

exception_t decodeSchedContextInvocation(word_t label, cap_t cap, extra_caps_t extraCaps);

/* Bind a tcb and a scheduling context. This allows a tcb to enter the scheduler.
 * If the tcb is runnable, insert into scheduler
 *
 * @param sc the scheduling context to bind
 * @param tcb the tcb to bind
 *
 * @pre  the scheduling context must not already be bound to a tcb,
 *       tcb->tcbSchedContext == NULL && sc->scTcb == NULL
 * @post tcb->tcbSchedContext == sc && sc->scTcb == tcb
 */
void schedContext_bindTCB(sched_context_t *sc, tcb_t *tcb);

/* Unbind a specific tcb from a scheduling context. If the tcb is runnable,
 * remove from the scheduler.
 *
 * @param sc  scheduling context to unbind
 * @param tcb the tcb to unbind
 *
 * @pre   the tcb is bound to the sc,
 *        (sc->scTcb == tcb && tcb->tcbSchedContext == sc);
 * @post  (tcb->tcbSchedContext == NULL && sc->scTcb == NULL)
 */
void schedContext_unbindTCB(sched_context_t *sc, tcb_t *tcb);

/*
 * Unbind any tcb from a scheduling context. If the tcb bound to the scheduling
 * context is runnable, remove from the scheduler.
 *
 * @param sc the scheduling context to unbind
 * @post  (sc->scTcb == NULL)
 */
void schedContext_unbindAllTCBs(sched_context_t *sc);

/*
 * Resume a scheduling context. This will check if a the tcb bound to the scheduling context
 * is runnable, if so, it will then check if the budget is due to be recharged and do so.
 * If the scheduling context has insufficient budget the bound tcb is placed in the release queue.
 *
 * @pre (sc != NULL)
 */
void schedContext_resume(sched_context_t *sc);

/*
 * Donate sc to tcb.
 *
 * @pre (sc != NULL && tcb != NULL)
 * @post (sc->scTcb == tcb && tcb->tcbSchedContext == sc)
 */
void schedContext_donate(sched_context_t *sc, tcb_t *to);

#endif /* __OBJECT_SCHED_CONTEXT_H */
