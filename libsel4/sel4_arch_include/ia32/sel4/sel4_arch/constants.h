/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_SEL4_ARCH_CONSTANTS_H

#include <autoconf.h>

#define TLS_GDT_ENTRY 6
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY 7
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#define seL4_PageBits        12
#define seL4_SlotBits         4
#define seL4_TCBBits         10
#define seL4_EndpointBits     4
#define seL4_NotificationBits 4
#define seL4_PageTableBits   12
#define seL4_PageDirBits     12
#define seL4_IOPageTableBits 12
#define seL4_ASIDPoolBits    12

#ifdef CONFIG_PAE_PAGING
#define seL4_PDPTBits         5
#define seL4_LargePageBits    21
#else
#define seL4_PDPTBits         0
#define seL4_LargePageBits    22
#endif

/* Previously large frames were explicitly assumed to be 4M. If not using
 * PAE assuming a legacy environment and leave the old definition */
#ifndef CONFIG_PAE_PAGING
#define seL4_4MBits           seL4_LargePageBits
#endif

#define seL4_WordBits (sizeof(seL4_Word) * 8)

#endif
