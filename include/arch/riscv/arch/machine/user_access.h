/*
 * Copyright 2025
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <arch/api/types.h>
#include <arch/types.h>
#include <arch/machine/hardware.h>

/*
 * Safe user memory access routines for RISC-V.
 *
 * Uses S-mode SUM (Supervisor User Memory) and a temporary SATP switch
 * to perform bounded load/store operations on user-space memory.
 * Addresses are validated to ensure they reside strictly within the
 * user window [0, USER_TOP).
 */

bool_t riscv_load_word_user(vptr_t user_addr, word_t *out_val,
                            paddr_t vspace_root_paddr, asid_t asid);

bool_t riscv_store_word_user(vptr_t user_addr, word_t val,
                             paddr_t vspace_root_paddr, asid_t asid);

/* Handles S-mode faults generated during memory access. Returns true if recovered. */
bool_t riscv_user_access_handle_fault(word_t scause);

#ifdef CONFIG_DEBUG_BUILD
exception_t handle_SysRiscvDebugReadUserWord(void);
#endif
