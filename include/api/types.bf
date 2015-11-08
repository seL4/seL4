--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

#if defined(__KERNEL_32__)

base 32

-- Cap rights
block cap_rights {
    padding 29
    field capAllowGrant 1
    field capAllowRead 1
    field capAllowWrite 1
}

-- Message info word
block message_info {
    field msgLabel 20
    field msgCapsUnwrapped 3
    field msgExtraCaps 2
    field msgLength 7
}

#elif defined(__KERNEL_64__)

base 64

-- Cap rights
block cap_rights {
    padding 32

    padding 29
    field capAllowGrant 1
    field capAllowRead 1
    field capAllowWrite 1
}

-- Message info word
block message_info {
    padding         32

    field msgLabel 20
    field msgCapsUnwrapped 3
    field msgExtraCaps 2
    field msgLength 7
}
#else

#error "typesbf unsupported kernel word size"
#endif
