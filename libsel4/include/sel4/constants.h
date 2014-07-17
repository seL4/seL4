/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __API_CONSTANTS_H
#define __API_CONSTANTS_H

#define BIT(n) (1ul<<(n))

enum priorityConstants {
    seL4_InvalidPrio = -1,
    seL4_MinPrio = 0,
    seL4_MaxPrio = 255
};

/* message_info_t defined in api/types.bf */

enum seL4_MsgLimits {
    seL4_MsgLengthBits = 7,
    seL4_MsgExtraCapBits = 2
};


#define seL4_MsgMaxLength 120
#define seL4_MsgMaxExtraCaps (BIT(seL4_MsgExtraCapBits)-1)

#endif /* __API_CONSTANTS_H */
