/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(D61_BSD)
 */

#pragma once

#include <autoconf.h>

#define TLS_GDT_ENTRY   7
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY    8
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#define seL4_DataFault 0
#define seL4_InstructionFault 1

/* for x86-64, the large page size is 2 MiB and huge page size is 1 GiB */
#define seL4_WordBits           64
#define seL4_PageBits           12
#define seL4_SlotBits           5
#define seL4_TCBBits            11
#define seL4_EndpointBits       4
#define seL4_NotificationBits   5
#define seL4_PageTableBits      12
#define seL4_PageDirBits        12
#define seL4_PDPTBits           12
#define seL4_PML4Bits           12
#define seL4_IOPageTableBits    12
#define seL4_LargePageBits      21
#define seL4_HugePageBits       30
#define seL4_ASIDPoolBits       12
