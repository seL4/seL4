/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/functions.h>
#include <sel4/sel4_arch/syscalls.h>
#include <sel4/types.h>

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Poll(seL4_CPtr src, seL4_Word *sender)
{
    return seL4_NBWait(src, sender);
}
#else /* CONFIG_KERNEL_MCS */
LIBSEL4_INLINE_FUNC void seL4_Wait(seL4_CPtr src, seL4_Word *sender)
{
    seL4_Recv(src, sender);
}
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Poll(seL4_CPtr src, seL4_Word *sender)
{
    return seL4_NBRecv(src, sender);
}
#endif /* !CONFIG_KERNEL_MCS */
