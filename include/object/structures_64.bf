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
    field capFreeIndex 48
    padding 9
    field capIsDevice 1
    field capBlockSize 6

    field capType 5
    padding 11
    field_high capPtr 48
}

block endpoint_cap(capEPBadge, capType, capCanGrant, capCanSend, capCanReceive,
                   capEPPtr) {
    field capEPBadge 64

    field capType 5
    field capCanGrant 1
    field capCanReceive 1
    field capCanSend 1
    padding 8
    field_high capEPPtr 48

}

block notification_cap {
    field capNtfnBadge 64

    field capType 5
    field capNtfnCanReceive 1
    field capNtfnCanSend 1
    padding 9
    field_high capNtfnPtr 48
}

block reply_cap(capReplyMaster, capTCBPtr, capType) {
    field capTCBPtr 64

    field capType 5
    padding 58
    field capReplyMaster 1
}

-- The user-visible format of the data word is defined by cnode_capdata, below.
block cnode_cap(capCNodeRadix, capCNodeGuardSize, capCNodeGuard,
                capType, capCNodePtr) {
    field capCNodeGuard 64

    field capType 5
    field capCNodeGuardSize 6
    field capCNodeRadix 6
    field_high capCNodePtr 47
}

block thread_cap {
    padding 64

    field capType 5
    padding 11
    field_high capTCBPtr 48
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

    padding 16
    field_high epQueue_tail 46
    field state 2
}

-- Async endpoint: size = 32 bytes
block notification {
    padding 16
    field_high ntfnBoundTCB 48

    field ntfnMsgIdentifier 64

    padding 16
    field_high ntfnQueue_head 48

    field_high ntfnQueue_tail 48
    padding 14
    field state 2
}

-- Mapping database (MDB) node: size = 16 bytes
block mdb_node {
    padding 16
    field_high mdbNext 46
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
--   - DiminishCaps
--   - Endpoint
-- * BlockedOnSend
--   - Endpoint
--   - CanGrant
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

-- Thread state: size = 8 bytes
block thread_state(blockingIPCBadge, blockingIPCCanGrant, blockingIPCIsCall,
                   tcbQueued, blockingIPCDiminishCaps, tsType,
                   blockingObject) {
    field blockingIPCBadge 64

    padding 60
    field blockingIPCCanGrant 1
    field blockingIPCIsCall 1
    field blockingIPCDiminishCaps 1
    field tcbQueued 1

    padding 16
    field_high blockingObject 44
    field tsType 4
}
