--
-- Copyright 2014, General Dynamics C4 Systems
-- Copyright 2024, Capabilities Limited
-- CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
--
-- SPDX-License-Identifier: GPL-2.0-only
--

-- Default base size: uint32_t
base 32

block null_cap {
    padding 32

    padding 28
    field capType 4

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 64
#endif
}

-- The combination of freeIndex and blockSize must match up with the
-- definitions of MIN_SIZE_BITS and MAX_SIZE_BITS
block untyped_cap(capFreeIndex, capIsDevice, capBlockSize, capPtr, capType) {
    field capFreeIndex 26
    field capIsDevice  1
    field capBlockSize 5

#if defined(__CHERI_PURE_CAPABILITY__)
    padding           28
#else
    field_high capPtr 28
#endif
    field capType     4

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capPtr  64
#endif
}

block endpoint_cap(capEPBadge, capCanGrantReply, capCanGrant, capCanSend,
                   capCanReceive, capEPPtr, capType) {
#if defined(__CHERI_PURE_CAPABILITY__)
    padding 28
#else
    field_high capEPPtr 28
#endif
    field capCanGrantReply 1
    field capCanGrant 1
    field capCanReceive 1
    field capCanSend 1

    field capEPBadge 28
    field capType 4

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capEPPtr 64
#endif
}

block notification_cap (capNtfnBadge, capNtfnCanReceive, capNtfnCanSend,
                        capNtfnPtr, capType) {
    field capNtfnBadge 28
    padding 2
    field capNtfnCanReceive 1
    field capNtfnCanSend 1

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 28
#else
    field_high capNtfnPtr 28
#endif
    field capType 4

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capNtfnPtr 64
#endif
}

#ifdef CONFIG_KERNEL_MCS
block reply_cap {
    field capReplyPtr 32

    padding 27
    field capReplyCanGrant 1
    field capType 4
}

block call_stack {
    field_high callStackPtr 28
    padding 3
    field isHead 1
}
#else
block reply_cap(capReplyCanGrant, capReplyMaster, capTCBPtr, capType) {
    padding 32

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 26
#else
    field_high capTCBPtr 26
#endif

    field capReplyCanGrant 1
    field capReplyMaster 1
    field capType 4

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capTCBPtr 64
#endif
}
#endif
-- The user-visible format of the data word is defined by cnode_capdata, below.
block cnode_cap(capCNodeRadix, capCNodeGuardSize, capCNodeGuard,
                capCNodePtr, capType) {
    padding 4
    field capCNodeGuardSize 5
    field capCNodeRadix 5
    field capCNodeGuard 18

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 28
#else
    field_high capCNodePtr 27
    padding 1
#endif
    field capType 4

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capCNodePtr 64
#endif
}

block thread_cap (capTCBPtr, capType) {
    padding              32

#if defined(__CHERI_PURE_CAPABILITY__)
    padding              28
#else
    field_high capTCBPtr 28
#endif

    field capType         4
#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capTCBPtr  64
#endif
}

block irq_control_cap {
    padding       32

    padding       24
    field capType  8
#if defined(__CHERI_PURE_CAPABILITY__)
    padding       64
#endif
}

block irq_handler_cap {
#ifdef ENABLE_SMP_SUPPORT
    field capIRQ   32
#else
    padding 24
    field capIRQ 8
#endif

    padding       24
    field capType  8
#if defined(__CHERI_PURE_CAPABILITY__)
    padding       64
#endif
}

block zombie_cap (capZombieID, capZombieType, capType) {
#if defined(__CHERI_PURE_CAPABILITY__)
    padding               32
#else
    field capZombieID     32
#endif

    padding               18
    field capZombieType   6
    field capType         8

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capZombieID 64
#endif
}

block domain_cap {
    padding 32

    padding 24
    field capType 8
#if defined(__CHERI_PURE_CAPABILITY__)
    padding 64
#endif
}

#ifdef CONFIG_KERNEL_MCS
block sched_context_cap {
    field_high capSCPtr 28
    padding              4

    padding             18
    field capSCSizeBits  6
    field capType        8
}

block sched_control_cap {
    field core    32

    padding       24
    field capType 8
}
#endif
---- Arch-independent object types

-- Endpoint: size = 16 bytes for non-CHERI builds
block endpoint {
#if !defined(__CHERI_PURE_CAPABILITY__)
    padding 64

    field_high epQueue_head 28
    padding 4

    field_high epQueue_tail 28
    padding 2
#endif
    field state 2

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                30
    cheri_cap epQueue_head 64
    cheri_cap epQueue_tail 64
#endif
}

-- Notification object: size = 16 bytes (32 bytes on mcs)
-- for non-CHERI builds
block notification {
#if !defined(__CHERI_PURE_CAPABILITY__)
#ifdef CONFIG_KERNEL_MCS
    padding 96

    field_high ntfnSchedContext 28
    padding 4
#endif

    field_high ntfnBoundTCB 28
    padding 4

    field ntfnMsgIdentifier 32

    field_high ntfnQueue_head 28
    padding 4

    field_high ntfnQueue_tail 28
    padding 2
#else
    field ntfnMsgIdentifier 32
#endif
    field state 2

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                  30
    cheri_cap ntfnQueue_head 64
    cheri_cap ntfnQueue_tail 64
    cheri_cap ntfnBoundTCB   64
#endif
}

-- Mapping database (MDB) node: size = 8 bytes for non-CHERI builds
block mdb_node (mdbNext, mdbRevocable, mdbFirstBadged, mdbPrev) {
#if !defined(__CHERI_PURE_CAPABILITY__)
    field_high mdbNext 29
    padding 1
#endif
    field mdbRevocable 1
    field mdbFirstBadged 1

#if !defined(__CHERI_PURE_CAPABILITY__)
    field_high mdbPrev 29
    padding 3
#else
    padding 30
    cheri_cap mdbNext 64
    cheri_cap mdbPrev 64
#endif
}

-- Thread state data
--
-- tsType
-- * Running
-- * Restart
-- * Inactive
-- * BlockedOnReceive
--   - Endpoint
--   - CanGrant
-- * BlockedOnSend
--   - Endpoint
--   - CanGrant
--   - CanGrantReply
--   - IsCall
--   - IPCBadge
--   - Fault
--     - faultType
--     * CapFault
--       - Address
--       - InReceivePhase
--       - LookupFailure
--         - lufType
--         * InvalidRoot
--         * MissingCapability
--           - BitsLeft
--         * DepthMismatch
--           - BitsFound
--           - BitsLeft
--         * GuardMismatch
--           - GuardFound
--           - BitsLeft
--           - GuardSize
--     * VMFault
--       - Address
--       - FSR
--       - FaultType
--     * UnknownSyscall
--       - Number
--     * UserException
--       - Number
--       - Code
-- * BlockedOnReply
-- * BlockedOnFault
--   - Fault
-- * BlockedOnNotification
--   - Notification
-- * Idle

-- Lookup fault: size = 8 bytes
block invalid_root {
    padding 62
    field lufType 2
}

block missing_capability {
    padding 56
    field bitsLeft 6
    field lufType 2
}

block depth_mismatch {
    padding 50
    field bitsFound 6
    field bitsLeft 6
    field lufType 2
}

block guard_mismatch {
    field guardFound 32
    padding 18
    field bitsLeft 6
    field bitsFound 6
    field lufType 2
}

tagged_union lookup_fault lufType {
    tag invalid_root 0
    tag missing_capability 1
    tag depth_mismatch 2
    tag guard_mismatch 3
}

-- Fault: size = 8 bytes
block NullFault {
    padding 60
    field seL4_FaultType 4
}

block CapFault {
    field address 32
    field inReceivePhase 1
    padding 27
    field seL4_FaultType 4
}

block UnknownSyscall {
    field syscallNumber 32
    padding 28
    field seL4_FaultType 4
}

block UserException {
    field number 32
    field code 28
    field seL4_FaultType 4
}

#ifdef CONFIG_HARDWARE_DEBUG_API
block DebugException {
    field breakpointAddress 32

    padding 20
    -- X86 has 4 breakpoints (DR0-3).
    -- ARM has between 2 and 16 breakpoints
    --   ( ARM Ref manual, C3.3).
    -- So we just use 4 bits to cater for both.
    field breakpointNumber 4
    field exceptionReason 4
    field seL4_FaultType 4
}
#endif

#ifdef CONFIG_KERNEL_MCS
block Timeout {
    field badge 32
    padding 28
    field seL4_FaultType 4
}
#endif

-- Thread state: size = 12 bytes for non-CHERI builds
block thread_state(blockingIPCBadge, blockingIPCCanGrant,
                   blockingIPCCanGrantReply, blockingIPCIsCall,
                   tcbQueued, blockingObject,
#ifdef CONFIG_KERNEL_MCS
                   tcbInReleaseQueue, replyObject,
#endif
                   tsType) {
    field blockingIPCBadge 28
    field blockingIPCCanGrant 1
    field blockingIPCCanGrantReply 1
    field blockingIPCIsCall 1
    padding 1

    -- this is fastpath-specific. it is useful to be able to write
    -- tsType and without changing tcbQueued or tcbInReleaseQueue
#ifdef CONFIG_KERNEL_MCS
    field_high replyObject 28
    padding 2
#else
    padding 31
#endif
    field tcbQueued 1
#ifdef CONFIG_KERNEL_MCS
    field tcbInReleaseQueue 1
#endif

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                   28
#else
    field_high blockingObject 28
#endif
    field tsType 4

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap  blockingObject 64
#endif
}
