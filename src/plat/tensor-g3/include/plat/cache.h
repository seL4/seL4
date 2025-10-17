/*
 * Copyright 2025, Millpex
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/*
 * Platform-specific cache maintenance routines.
 * On Tensor G3, these are wrappers around generic ARM operations,
 * as the DynamIQ architecture handles coherency.
 */

void plat_cleanL2Range(paddr_t start, paddr_t end);
void plat_invalidateL2Range(paddr_t start, paddr_t end);
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);
