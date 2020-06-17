--
-- Copyright 2014, General Dynamics C4 Systems
--
-- SPDX-License-Identifier: GPL-2.0-only
--

-- Default base size: uint32_t
base 32

block null_cap {
    padding 32

    padding 28
    field capType 4
}

-- The combination of freeIndex and blockSize must match up with the
-- definitions of MIN_SIZE_BITS and MAX_SIZE_BITS
block untyped_cap {
    field capFreeIndex 26
    field capIsDevice  1
    field capBlockSize 5

    field_high capPtr 28
    field capType     4
}

block endpoint_cap(capEPBadge, capCanGrantReply, capCanGrant, capCanSend,
                   capCanReceive, capEPPtr, capType) {
    field_high capEPPtr 28
    field capCanGrantReply 1
    field capCanGrant 1
    field capCanReceive 1
    field capCanSend 1

    field capEPBadge 28
    field capType 4
}

block notification_cap {
    field capNtfnBadge 28
    padding 2
    field capNtfnCanReceive 1
    field capNtfnCanSend 1

    field_high capNtfnPtr 28
    field capType 4
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

    field_high capTCBPtr 26
    field capReplyCanGrant 1
    field capReplyMaster 1
    field capType 4
}
#endif
-- The user-visible format of the data word is defined by cnode_capdata, below.
block cnode_cap(capCNodeRadix, capCNodeGuardSize, capCNodeGuard,
                capCNodePtr, capType) {
    padding 4
    field capCNodeGuardSize 5
    field capCNodeRadix 5
    field capCNodeGuard 18

    field_high capCNodePtr 27
    padding 1
    field capType 4
}

block thread_cap {
    padding              32

    field_high capTCBPtr 28
    field capType         4
}

block irq_control_cap {
    padding       32

    padding       24
    field capType  8
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
}

block zombie_cap {
    field capZombieID     32

    padding               18
    field capZombieType   6
    field capType         8
}

block domain_cap {
    padding 32

    padding 24
    field capType 8
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

-- Endpoint: size = 16 bytes
block endpoint {
    padding 64

    field_high epQueue_head 28
    padding 4

    field_high epQueue_tail 28
    padding 2
    field state 2
}

-- Notification object: size = 16 bytes (32 bytes on mcs)
block notification {
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
    field state 2
}

-- Mapping database (MDB) node: size = 8 bytes
block mdb_node {
    field_high mdbNext 29
    padding 1
    field mdbRevocable 1
    field mdbFirstBadged 1

    field_high mdbPrev 29
    padding 3
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

-- Thread state: size = 12 bytes
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

    field_high blockingObject 28
    field tsType 4
}
