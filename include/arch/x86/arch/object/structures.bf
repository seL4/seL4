--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

#include <config.h>

#if defined(X86_32)
#include <arch/object/structures_32.bf>
#else
#error Unknown architecture
#endif

base 32

block ia32_pat_msr {
    padding     5
    field pa7   3
    padding     5
    field pa6   3
    padding     5
    field pa5   3
    padding     5
    field pa4   3
    padding     5
    field pa3   3
    padding     5
    field pa2   3
    padding     5
    field pa1   3
    padding     5
    field pa0   3
}

-- Local APIC

block apic_base_msr {
    field_high  base_addr           20
    field       enabled             1
    padding                         2
    field       is_bsp              1
    padding                         8
}

block apic_version {
    padding                         8
    field       max_lvt_entry       8
    padding                         8
    field       version             8
}

block apic_svr {
    padding                         22
    field       focus_processor_chk 1
    field       enabled             1
    field       spurious_vector     8
}

block apic_lvt {
    padding                         13
    field       timer_mode          2
    field       masked              1
    field       trigger_mode        1
    field       remote_irr          1
    field       pin_polarity        1
    field       delivery_status     1
    padding                         1
    field       delivery_mode       3
    field       vector              8
}

block apic_icr1 {
    padding                         12
    field       dest_shorthand      2
    padding                         2
    field       trigger_mode        1
    field       level               1
    padding                         1
    field       delivery_status     1
    field       dest_mode           1
    field       delivery_mode       3
    field       vector              8
}

block apic_icr2 {
    field       dest                8
    padding                         24
}
