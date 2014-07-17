/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __ARCH_OBJECTTYPE_H
#define __ARCH_OBJECTTYPE_H

typedef enum _object {
    seL4_ARM_SmallPageObject = seL4_NonArchObjectTypeCount,
    seL4_ARM_LargePageObject,
    seL4_ARM_SectionObject,
    seL4_ARM_SuperSectionObject,
    seL4_ARM_PageTableObject,
    seL4_ARM_PageDirectoryObject,
    seL4_ObjectTypeCount
} seL4_ArchObjectType;

typedef uint32_t object_t;

#endif /* __ARCH_OBJECTTYPE_H */

