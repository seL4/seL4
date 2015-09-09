--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

#include <autoconf.h>

#if CONFIG_WORD_SIZE == 32
#include "types_32.bf"
#else
#error Unsupported word size
#endif

