--
-- Copyright 2017, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230.
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(DATA61_GPL)
--

block null_cap {
    padding 64

    field capType 5
    padding 59
}

block untyped_cap {
#if BF_CANONICAL_RANGE == 48
    field capFreeIndex 48
    padding 9
#elif BF_CANONICAL_RANGE == 39
    field capFreeIndex 39
    padding 18
#else
#error "Unspecified cannonical address range"
#endif
    field capIsDevice 1
    field capBlockSize 6

    field capType 5
#if BF_CANONICAL_RANGE == 48
    padding 11
    field_high capPtr 48
#elif BF_CANONICAL_RANGE == 39
    padding 20
    field_high capPtr 39
#else
#error "Unspecified cannonical address range"
#endif
}

block endpoint_cap(capEPBadge, capCanGrantReply, capCanGrant, capCanSend,
                   capCanReceive, capEPPtr, capType) {
    field capEPBadge 64

    field capType 5
    field capCanGrantReply 1
    field capCanGrant 1
    field capCanReceive 1
    field capCanSend 1
#if BF_CANONICAL_RANGE == 48
    padding 7
    field_high capEPPtr 48
#elif BF_CANONICAL_RANGE == 39
    padding 16
    field_high capEPPtr 39
#else
#error "Unspecified cannonical address range"
#endif

}

block notification_cap {
    field capNtfnBadge 64

    field capType 5
    field capNtfnCanReceive 1
    field capNtfnCanSend 1
#if BF_CANONICAL_RANGE == 48
    padding 9
    field_high capNtfnPtr 48
#elif BF_CANONICAL_RANGE == 39
    padding 18
    field_high capNtfnPtr 39
#else
#error "Unspecified cannonical address range"
#endif
}

block reply_cap(capReplyCanGrant, capReplyMaster, capTCBPtr, capType) {
    field capTCBPtr 64

    field capType 5
    padding 57
    field capReplyCanGrant 1
    field capReplyMaster 1
}

-- The user-visible format of the data word is defined by cnode_capdata, below.
block cnode_cap(capCNodeRadix, capCNodeGuardSize, capCNodeGuard,
                capCNodePtr, capType) {
    field capCNodeGuard 64

    field capType 5
    field capCNodeGuardSize 6
    field capCNodeRadix 6
#if BF_CANONICAL_RANGE == 48
    field_high capCNodePtr 47
#elif BF_CANONICAL_RANGE == 39
    padding 9
    field_high capCNodePtr 38
#else
#error "Unspecified cannonical address range"
#endif
}

block thread_cap {
    padding 64

    field capType 5
#if BF_CANONICAL_RANGE == 48
    padding 11
    field_high capTCBPtr 48
#elif BF_CANONICAL_RANGE == 39
    padding 20
    field_high capTCBPtr 39
#else
#error "Unspecified cannonical address range"
#endif
}

block irq_control_cap {
    padding 64

    field capType  5
    padding 59
}

block irq_handler_cap {
    padding 56
    field capIRQ 8

    field capType  5
    padding 59
}

block zombie_cap {
    field capZombieID     64

    field capType         5
    padding               52
    field capZombieType   7
}

block domain_cap {
    padding 64

    field capType 5
    padding 59
}

---- Arch-independent object types

-- Endpoint: size = 16 bytes
block endpoint {
    field epQueue_head 64

#if BF_CANONICAL_RANGE == 48
    padding 16
    field_high epQueue_tail 46
#elif BF_CANONICAL_RANGE == 39
    padding 25
    field_high epQueue_tail 37
#else
#error "Unspecified cannonical address range"
#endif
    field state 2
}

-- Async endpoint: size = 32 bytes
block notification {
#if BF_CANONICAL_RANGE == 48
    padding 16
    field_high ntfnBoundTCB 48
#elif BF_CANONICAL_RANGE == 39
    padding 25
    field_high ntfnBoundTCB 39
#else
#error "Unspecified cannonical address range"
#endif

    field ntfnMsgIdentifier 64

#if BF_CANONICAL_RANGE == 48
    padding 16
    field_high ntfnQueue_head 48
#elif BF_CANONICAL_RANGE == 39
    padding 25
    field_high ntfnQueue_head 39
#else
#error "Unspecified cannonical address range"
#endif

#if BF_CANONICAL_RANGE == 48
    field_high ntfnQueue_tail 48
    padding 14
#elif BF_CANONICAL_RANGE == 39
    field_high ntfnQueue_tail 39
    padding 23
#else
#error "Unspecified cannonical address range"
#endif
    field state 2
}

-- Mapping database (MDB) node: size = 16 bytes
block mdb_node {
#if BF_CANONICAL_RANGE == 48
    padding 16
    field_high mdbNext 46
#elif BF_CANONICAL_RANGE == 39
    padding 25
    field_high mdbNext 37
#else
#error "Unspecified cannonical address range"
#endif
    field mdbRevocable 1
    field mdbFirstBadged 1

    field mdbPrev 64
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
--     - seL4_FaultType
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

-- Lookup fault: size = 16 bytes
block invalid_root {
    padding 64

    padding 62
    field lufType 2
}

block missing_capability {
    padding 64

    padding 55
    field bitsLeft 7
    field lufType 2
}

block depth_mismatch {
    padding 64

    padding 48
    field bitsFound 7
    field bitsLeft 7
    field lufType 2
}

block guard_mismatch {
    field guardFound 64

    padding 48
    field bitsLeft 7
    field bitsFound 7
    field lufType 2
}

tagged_union lookup_fault lufType {
    tag invalid_root 0
    tag missing_capability 1
    tag depth_mismatch 2
    tag guard_mismatch 3
}

-- Fault: size = 16 bytes
block NullFault {
    padding 64

    padding 61
    field seL4_FaultType 3
}

block CapFault {
    field address 64

    field inReceivePhase 1
    padding 60
    field seL4_FaultType 3
}

block UnknownSyscall {
    field syscallNumber 64

    padding 61
    field seL4_FaultType 3
}

block UserException {
    padding 64

    field number 32
    field code 29
    field seL4_FaultType 3
}

#ifdef CONFIG_HARDWARE_DEBUG_API
block DebugException {
    field breakpointAddress 64

    padding 53
    -- X86 has 4 breakpoints (DR0-3).
    -- ARM has between 2 and 16 breakpoints
    --   ( ARM Ref manual, C3.3).
    -- So we just use 4 bits to cater for both.
    field breakpointNumber 4
    field exceptionReason 4
    field seL4_FaultType 3
}
#endif

-- Thread state: size = 24 bytes
block thread_state(blockingIPCBadge, blockingIPCCanGrant,
                   blockingIPCCanGrantReply, blockingIPCIsCall,
                   tcbQueued, blockingObject,
                   tsType) {
    field blockingIPCBadge 64

    padding 60
    field blockingIPCCanGrant 1
    field blockingIPCCanGrantReply 1
    field blockingIPCIsCall 1
    field tcbQueued 1

#if BF_CANONICAL_RANGE == 48
    padding 16
    field_high blockingObject 44
#elif BF_CANONICAL_RANGE == 39
    padding 25
    field_high blockingObject 35
#else
#error "Unspecified cannonical address range"
#endif
    field tsType 4
}
