/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_DEPRECATED_H
#define __LIBSEL4_DEPRECATED_H

static inline int __attribute__((deprecated("use seL4_IRQHandler_SetNotification")))
seL4_IRQHandler_SetEndpoint(seL4_CPtr irq_handler, seL4_CPtr endpoint)
{
    return seL4_IRQHandler_SetNotification(irq_handler, endpoint);
}

static inline void __attribute__((deprecated("use seL4_Signal")))
seL4_Notify(seL4_CPtr dest, __attribute__((unused)) seL4_Word msg)
{
    seL4_Signal(dest);
}

#endif // __LIBSEL4_DEPRECATED_H
