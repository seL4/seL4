/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __LIBSEL4_SEL4_SEL4_ARCH_OBJECTTYPE_H_
#define __LIBSEL4_SEL4_SEL4_ARCH_OBJECTTYPE_H_

typedef enum _mode_object {
    seL4_ARM_HugePageObject = seL4_NonArchObjectTypeCount,
    seL4_ARM_PageUpperDirectoryObject,
    seL4_ARM_PageGlobalDirectoryObject,
    seL4_ModeObjectTypeCount
} seL4_ModeObjectType;

#endif /* __LIBSEL4_SEL4_SEL4_ARCH_OBJECTTYPE_H_ */
