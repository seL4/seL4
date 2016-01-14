/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_TFIPC
#define __LIBSEL4_TFIPC

#include <sel4/types.h>
/**
 * This is what temporal fault ipc looks like
 * Not a standalone include! include messages.h
 */
#define SEL4_TFIPC_LABEL              seL4_TemporalFault
#define SEL4_TFIPC_LENGTH             1
#define SEL4_TFIPC_DATA_WORD          0

static inline seL4_Word seL4_TF_DataWord(void)
{
    return seL4_GetMR(SEL4_TFIPC_DATA_WORD);
}

static inline seL4_Word seL4_isTemporalFault_Tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == SEL4_TFIPC_LABEL;
}

#endif /* __LIBSEL4_TFIPC */

