/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

exception_t decodeSchedContextInvocation(word_t label, sched_context_t *sc, bool_t call);

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

/* Unbind the tcb from a scheduling context. If the tcb is runnable,
 * remove from the scheduler.
 *
 * @param sc the scheduling context whose tcb will be unbound
 *
 * @pre   the sc is not NULL and is bound to a tcb
 *        (sc != NULL && sc->scTcb != NULL);
 * @post  (sc->scTcb == NULL && sc->scTcb->tcbSchedContext == NULL)
 */
void schedContext_unbindTCB(sched_context_t *sc);

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

/* Bind scheduling context to a notification */
void schedContext_bindNtfn(sched_context_t *sc, notification_t *ntfn);
/* unbind scheduling context from a notification */
void schedContext_unbindNtfn(sched_context_t *sc);

time_t schedContext_updateConsumed(sched_context_t *sc);
void schedContext_completeYieldTo(tcb_t *yielder);
void schedContext_cancelYieldTo(tcb_t *yielder);

