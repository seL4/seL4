/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <arch/kernel/vspace.h>

struct lookupPDPTSlot_ret {
    exception_t status;
    pdpte_t     *pdptSlot;
};
typedef struct lookupPDPTSlot_ret lookupPDPTSlot_ret_t;

static inline pte_t x86_make_device_pte(paddr_t phys)
{
    return pte_new(
               0,      /* xd */
               phys,   /* page_base_address    */
               1,      /* global               */
               0,      /* pat                  */
               0,      /* dirty                */
               0,      /* accessed             */
               1,      /* cache_disabled       */
               1,      /* write_through        */
               0,      /* super_user           */
               1,      /* read_write           */
               1       /* present              */
           );
}

static inline pte_t x86_make_empty_pte(void)
{
    return pte_new(
               0,      /* xd */
               0,      /* page_base_address    */
               0,      /* global               */
               0,      /* pat                  */
               0,      /* dirty                */
               0,      /* accessed             */
               0,      /* cache_disabled       */
               0,      /* write_through        */
               0,      /* super_user           */
               0,      /* read_write           */
               0       /* present              */
           );
}

static inline CONST pml4e_t x86_make_empty_root_mapping(void)
{
    return pml4e_new(
               0,                  /* xd               */
               0,                  /* pdpt_base_addr   */
               0,                  /* accessed         */
               0,                  /* cache_disabled   */
               0,                  /* write through    */
               0,                  /* super user       */
               0,                  /* read_write       */
               0                   /* present          */
           );
}

