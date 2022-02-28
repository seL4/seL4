/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#define seL4_ARM_PageDirectory seL4_ARM_PageTable;
#define seL4_ARM_PageDirectory_Map seL4_ARM_PageTable_Map
#define seL4_ARM_PageDirectory_Unmap seL4_ARM_PageTable_Unmap
#define ARMPageDirectoryMap ARMPageTableMap
#define ARMPageDirectoryUnmap ARMPageTableUnmap

#define seL4_PageDirBits seL4_PageTableBits
#define seL4_PageDirEntryBits seL4_PageTableEntryBits
#define seL4_PageDirIndexBits seL4_PageTableIndexBits
#define seL4_ARM_PageDirectoryObject seL4_ARM_PageTableObject
