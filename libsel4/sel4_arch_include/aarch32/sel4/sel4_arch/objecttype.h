/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __SEL4_ARCH_OBJECT_TYPE_H
#define __SEL4_ARCH_OBJECT_TYPE_H

typedef enum _mode_object {
    seL4_ARM_SectionObject = seL4_NonArchObjectTypeCount,
    seL4_ARM_SuperSectionObject,
    seL4_ModeObjectTypeCount
} seL4_ModeObjectType;

#endif
