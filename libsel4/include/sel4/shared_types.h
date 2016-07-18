/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SHARED_TYPES_H
#define __LIBSEL4_SHARED_TYPES_H

/* this file is shared between the kernel and libsel4 */

typedef struct seL4_IPCBuffer_ {
    seL4_MessageInfo_t tag;
    seL4_Word msg[seL4_MsgMaxLength];
    seL4_Word userData;
    seL4_Word caps_or_badges[seL4_MsgMaxExtraCaps];
    seL4_CPtr receiveCNode;
    seL4_CPtr receiveIndex;
    seL4_Word receiveDepth;
} seL4_IPCBuffer __attribute__ ((__aligned__ (sizeof(struct seL4_IPCBuffer_))));

enum {
    seL4_CapFault_IP,
    seL4_CapFault_Addr,
    seL4_CapFault_InRecvPhase,
    seL4_CapFault_LookupFailureType,
    seL4_CapFault_BitsLeft,
    seL4_CapFault_DepthMismatch_BitsFound,
    seL4_CapFault_GuardMismatch_GuardFound = seL4_CapFault_DepthMismatch_BitsFound,
    seL4_CapFault_GuardMismatch_BitsFound,
    SEL4_FORCE_LONG_ENUM(seL4_CapFault_Msg),
} seL4_CapFault_Msg;

#define seL4_AllRights seL4_CapRights_new(true, true, true)
#define seL4_CanRead   seL4_CapRights_new(false, true, false)
#define seL4_CanWrite  seL4_CapRights_new(false, false, true)
#define seL4_CanGrant  seL4_CapRights_new(true, false, false)
#define seL4_NoWrite   seL4_CapRights_new(true, true, false);

#endif
