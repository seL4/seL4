/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __ARCH_OBJECTTYPE_H
#define __ARCH_OBJECTTYPE_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _object {
    seL4_ARM_SmallPageObject = seL4_ModeObjectTypeCount,
    seL4_ARM_LargePageObject,
#ifdef CONFIG_ARCH_AARCH32
    seL4_ARM_SectionObject,
    seL4_ARM_SuperSectionObject,
#endif
    seL4_ARM_PageTableObject,
    seL4_ARM_PageDirectoryObject,
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    seL4_ARM_VCPUObject,
#endif
#ifdef CONFIG_ARM_SMMU
    seL4_ARM_IOPageTableObject,
#endif
    seL4_ObjectTypeCount
} seL4_ArchObjectType;

typedef seL4_Word object_t;

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
#define seL4_ARM_VCPUObject 0xfffe
#endif

#ifndef CONFIG_ARM_SMMU
#define seL4_ARM_IOPageTableObject 0xffff
#endif

#endif /* __ARCH_OBJECTTYPE_H */

