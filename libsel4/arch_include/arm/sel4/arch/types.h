/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_TYPES_H
#define __LIBSEL4_ARCH_TYPES_H

#include <sel4/simple_types.h>
#include <sel4/sel4_arch/types.h>
#ifdef ARM_HYP
#define seL4_PageTableBits 12
#else  /* ARM_HYP */
#endif /* ARM_HYP */
#define seL4_ARM_VCPUBits 12

typedef seL4_CPtr seL4_ARM_Page;
typedef seL4_CPtr seL4_ARM_PageTable;
typedef seL4_CPtr seL4_ARM_PageDirectory;
typedef seL4_CPtr seL4_ARM_ASIDControl;
typedef seL4_CPtr seL4_ARM_ASIDPool;
typedef seL4_CPtr seL4_ARM_VCPU;

#endif /* __ARCH_SEL4TYPES_H__ */
