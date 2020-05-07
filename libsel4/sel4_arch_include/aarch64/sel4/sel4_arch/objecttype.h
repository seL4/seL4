/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

typedef enum _mode_object {
    seL4_ARM_HugePageObject = seL4_NonArchObjectTypeCount,
    seL4_ARM_PageUpperDirectoryObject,
    seL4_ARM_PageGlobalDirectoryObject,
    seL4_ModeObjectTypeCount
} seL4_ModeObjectType;
