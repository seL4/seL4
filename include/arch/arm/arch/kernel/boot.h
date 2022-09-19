/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>

cap_t create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large);
cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large,
                                 bool_t executable);

/* This is called from assembly code and thus there are no specific types in
 * the signature.
 */
void init_kernel(
    word_t ui_p_reg_start,
    word_t ui_p_reg_end,
    word_t pv_offset,
    word_t v_entry,
    word_t dtb_addr_p,
    word_t dtb_size
);

