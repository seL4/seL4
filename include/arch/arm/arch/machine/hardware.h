/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_HARDWARE_H
#define __ARCH_MACHINE_HARDWARE_H

#include <config.h>
#include <mode/machine/hardware.h>

#ifndef __ASSEMBLER__
enum vm_fault_type {
    ARMDataAbort = seL4_DataFault,
    ARMPrefetchAbort = seL4_InstructionFault
};
typedef word_t vm_fault_type_t;

#define PAGE_BASE(_p, _s)        ((_p) & ~MASK(pageBitsForSize((_s))))
#define PAGE_OFFSET(_p, _s)      ((_p) & MASK(pageBitsForSize((_s))))
#define IS_PAGE_ALIGNED(_p, _s)  (((_p) & MASK(pageBitsForSize((_s)))) == 0)

#define IPI_MEM_BARRIER \
  do { \
     dmb(); \
  } while (0)

#endif /* __ASSEMBLER__ */
#endif /* !__ARCH_MACHINE_HARDWARE_H */
