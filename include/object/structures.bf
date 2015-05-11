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

block async_endpoint_cap {
    field capAEPBadge 28
    padding 2
    field capAEPCanReceive 1
    field capAEPCanSend 1

    field_high capAEPPtr 28
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

block domain_cap {
    padding 32

    padding 24
    field capType 8
}

