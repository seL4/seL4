/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __MODE_KERNEL_VSPACE_H
#define __MODE_KERNEL_VSPACE_H

#include <arch/kernel/vspace.h>

struct lookupPDPTSlot_ret {
    exception_t status;
    pdpte_t     *pdptSlot;
};
typedef struct lookupPDPTSlot_ret lookupPDPTSlot_ret_t;

static inline pte_t
x86_make_device_pte(paddr_t phys)
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

static inline pte_t
x86_make_empty_pte(void)
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

static inline CONST pml4e_t
x86_make_empty_root_mapping(void)
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

#endif /* __MODE_KERNEL_VSPACE_H */
