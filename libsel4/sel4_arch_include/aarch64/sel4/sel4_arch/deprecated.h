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

#define seL4_PUDEntryBits 3

#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined (CONFIG_ARM_PA_SIZE_BITS_40)

#define seL4_PGDBits 0
#define seL4_PGDEntryBits 0
#define seL4_PGDIndexBits    0

#define seL4_PUDBits 13
#define seL4_PUDIndexBits 10
#define seL4_ARM_PageUpperDirectoryObject seL4_ARM_VSpaceObject

#else

#define seL4_PGDBits 12
#define seL4_PGDEntryBits 3
#define seL4_PGDIndexBits    9

#define seL4_PUDBits 12
#define seL4_PUDIndexBits 9
#define seL4_ARM_PageGlobalDirectoryObject seL4_ARM_VSpaceObject
#define seL4_ARM_PageUpperDirectoryObject seL4_ARM_PageTableObject
#define seL4_ARM_PageUpperDirectory seL4_ARM_PageTable;
#define seL4_ARM_PageUpperDirectory_Map seL4_ARM_PageTable_Map
#define seL4_ARM_PageUpperDirectory_Unmap seL4_ARM_PageTable_Unmap
#endif
