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

static inline pte_t
x86_make_device_pte(paddr_t phys)
{
    return pte_new(
               phys,   /* page_base_address    */
               0,      /* avl                  */
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

static inline CONST pte_t
x86_make_empty_pte(void)
{
    return makeUserPTEInvalid();
}

static inline CONST pde_t
x86_make_empty_root_mapping(void)
{
    return makeUserPDEInvalid();
}

#endif /* __MODE_KERNEL_VSPACE_H */
