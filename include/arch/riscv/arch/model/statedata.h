/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * Copyright 2018, DornerWorks
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_DORNERWORKS_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __ARCH_MODEL_STATEDATA_H
#define __ARCH_MODEL_STATEDATA_H

#include <config.h>
#include <types.h>
#include <util.h>
#include <object/structures.h>
#include <arch/types.h>
#include <plat/machine/devices.h>


NODE_STATE_BEGIN(archNodeState)
/* TODO: add RISCV-dependent fields here */
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);
NODE_STATE_END(archNodeState);

extern asid_pool_t *riscvKSASIDTable[BIT(asidHighBits)];

/* Kernel Page Tables */
extern pte_t kernel_root_pageTable[BIT(PT_INDEX_BITS)] VISIBLE;

#if CONFIG_PT_LEVELS == 3

/* RISC-V has n-level page tables (depending on configured paging mode),
 *  As of RISC-V priv 1.10,
 *  - level 1 are entries for 1GiB pages
 *  - level 2 are entries for 2MiB pages
 *  - level 3 are entries for 4KiB pages
 */
/* This consists of a one dimensional page table for the first level and a
   two dimensional page table for the second level. The number of
   second level page tables are bound by the macro: NUM_2MB_ENTRIES.
   This allows a page table entry in the first level to be either a leaf or
   point to one of the second level page tables.
*/

/*
        Level 1 Page Table              Level 2 Page Tables
        +----------------+              +---------------------------+
        | Leaf PTE       |              | +-----------------------+ |
        +----------------+    +------->[0]|    |    |    |    |   | |
        | Next Level PTE +----+         | +-----------------------+ |
        +----------------+              | +-----------------------+ |
        | Next Level PTE +------------>[1]|    |    |    |    |   | |
        +----------------+              | +-----------------------+ |
        |                |              | +-----------------------+ |
        +----------------+           [...]|    |    |    |    |   | |
        |                |              | +-----------------------+ |
        +----------------+              | +-----------------------+ |
                      [NUM_2MiB_ENTRIES-1]|    |    |    |    |   | |
                                        | +-----------------------+ |
                                        +---------------------------+
 */

/* Kernel has Upper 2GiB of Virtual Memory. 512 Entries per PTE * 2MiB = 1GiB, so 2 of these are needed */
#define NUM_2MB_ENTRIES 2

extern pte_t kernel_level2_Tables[NUM_2MB_ENTRIES][BIT(PT_INDEX_BITS)] VISIBLE;

#endif
#endif
