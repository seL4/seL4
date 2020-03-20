/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

/* Currently MSIs do not go through a vt-d translation by
 * the kernel, therefore when the user programs an MSI they
 * need to know how the 'vector' they allocated relates to
 * the actual vector table. In this case if they allocate
 * vector X they need to program their MSI to interrupt
 * vector X + IRQ_OFFSET */
#define IRQ_OFFSET (0x20 + 16)

/* When allocating vectors for IOAPIC or MSI interrupts,
 * this represent the valid range */
#define VECTOR_MIN (0)
#define VECTOR_MAX (109)

/* Legacy definitions */
#define MSI_MIN VECTOR_MIN
#define MSI_MAX VECTOR_MAX

#define seL4_VCPUBits 14
#define seL4_X86_VCPUBits    seL4_VCPUBits

#define seL4_X86_EPTPML4EntryBits 3
#define seL4_X86_EPTPML4IndexBits 9
#define seL4_X86_EPTPML4Bits (seL4_X86_EPTPML4EntryBits + seL4_X86_EPTPML4IndexBits)

#define seL4_X86_EPTPDPTEntryBits 3
#define seL4_X86_EPTPDPTIndexBits 9
#define seL4_X86_EPTPDPTBits (seL4_X86_EPTPDPTEntryBits + seL4_X86_EPTPDPTIndexBits)

#define seL4_X86_EPTPDEntryBits   3
#define seL4_X86_EPTPDIndexBits   9
#define seL4_X86_EPTPDBits   (seL4_X86_EPTPDEntryBits + seL4_X86_EPTPDIndexBits)

#define seL4_X86_EPTPTEntryBits   3
#define seL4_X86_EPTPTIndexBits   9
#define seL4_X86_EPTPTBits   (seL4_X86_EPTPTEntryBits + seL4_X86_EPTPTIndexBits)