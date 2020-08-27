/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

/* this file is shared between the kernel and libsel4 */

typedef struct seL4_IPCBuffer_ {
    seL4_MessageInfo_t tag;
    seL4_Word msg[seL4_MsgMaxLength];
    seL4_Word userData;
    seL4_Word caps_or_badges[seL4_MsgMaxExtraCaps];
    seL4_CPtr receiveCNode;
    seL4_CPtr receiveIndex;
    seL4_Word receiveDepth;
} seL4_IPCBuffer __attribute__((__aligned__(sizeof(struct seL4_IPCBuffer_))));

typedef enum {
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

#define seL4_ReadWrite     seL4_CapRights_new(0, 0, 1, 1)
#define seL4_AllRights     seL4_CapRights_new(1, 1, 1, 1)
#define seL4_CanRead       seL4_CapRights_new(0, 0, 1, 0)
#define seL4_CanWrite      seL4_CapRights_new(0, 0, 0, 1)
#define seL4_CanGrant      seL4_CapRights_new(0, 1, 0, 0)
#define seL4_CanGrantReply seL4_CapRights_new(1, 0, 0, 0)
#define seL4_NoWrite       seL4_CapRights_new(1, 1, 1, 0)
#define seL4_NoRead        seL4_CapRights_new(1, 1, 0, 1)
#define seL4_NoRights      seL4_CapRights_new(0, 0, 0, 0)

