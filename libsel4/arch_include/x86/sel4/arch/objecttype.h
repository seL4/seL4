/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __ARCH_OBJECT_TYPE_H
#define __ARCH_OBJECT_TYPE_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _object {
    seL4_IA32_4K = seL4_ModeObjectTypeCount,
    seL4_IA32_LargePage,
    seL4_IA32_PageTableObject,
    seL4_IA32_PageDirectoryObject,
#ifdef CONFIG_IOMMU
    seL4_IA32_IOPageTableObject,
#endif
    seL4_ObjectTypeCount
} seL4_ArchObjectType;
typedef seL4_Word object_t;

#ifndef CONFIG_PAE_PAGING
#define seL4_IA32_4M seL4_IA32_LargePage
#endif

#ifndef CONFIG_IOMMU
#define seL4_IA32_IOPageTableObject 0xffffff
#endif

#endif
