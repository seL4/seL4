--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

-- Default base size: uint32_t
base 32

block null_cap {
    padding 32

    padding 28
    field capType 4
}

block untyped_cap {
    field capFreeIndex 27
    field capBlockSize 5

    field_high capPtr 28
    field capType     4
}

block endpoint_cap(capEPBadge, capCanGrant, capCanSend, capCanReceive,
                   capEPPtr, capType) {
    field_high capEPPtr 28
    padding 1
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

block reply_cap(capReplyMaster, capTCBPtr, capType) {
    padding 32

    field_high capTCBPtr 27
    field capReplyMaster 1
    field capType 4
}

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

block cnode_capdata {
    padding 6
    field guard 18
    field guardSize 5
    padding 3
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
    padding       24
    field capIRQ   8

    padding       24
    field capType  8
}

block zombie_cap {
    field capZombieID     32

    padding               18
    field capZombieType   6
    field capType         8
}

block sched_context_cap {
    field_high capPtr 28
    padding            4

    padding           24
    field  capType     8
}

block sched_control_cap {
    padding       32

    padding       24
    field capType 8
}

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

-- Notification object: size = 32 bytes
block notification {
    padding 96
    
    field_high ntfnSchedContext 28
    padding 4

    field_high ntfnBoundTCB 28
    field bound 1
    padding 3

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
-- * BlockedOnSend
--   - Endpoint
--   - CanGrant
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
    padding 61
    field seL4_FaultType 3
}

block CapFault {
    field address 32
    field inReceivePhase 1
    padding 28
    field seL4_FaultType 3
}

block UnknownSyscall {
    field syscallNumber 32
    padding 29
    field seL4_FaultType 3
}

block UserException {
    field number 32
    field code 29
    field seL4_FaultType 3
}

block Temporal {
    field data 32
    padding 29
    field seL4_FaultType 3
}

block NoFaultHandler {
    padding 61
    field seL4_FaultType 3
}

-- Thread state: size = 12 bytes
block thread_state(blockingIPCBadge, blockingIPCCanGrant, blockingIPCIsCall,
                   tcbQueued, blockingObject,
                   tcbInReleaseQueue, tcbCritQueued, tsType) {
    field blockingIPCBadge 28
    field blockingIPCCanGrant 1
    field blockingIPCIsCall 1
    padding 2

    -- this is fastpath-specific. it is useful to be able to write
    -- tsType and without changing tcbQueued
    -- or inReleaseQueue 
    padding 29
    field tcbCritQueued 1
    field tcbInReleaseQueue 1
    field tcbQueued 1

    field_high blockingObject 28
    field tsType 4
}

