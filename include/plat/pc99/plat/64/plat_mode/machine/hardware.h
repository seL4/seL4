/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#ifndef __PLAT_MODE_MACHINE_HARDWARE_H_
#define __PLAT_MODE_MACHINE_HARDWARE_H_

#include <config.h>
#include <basic_types.h>
#include <plat/machine.h>
#include <plat_mode/machine/hardware_gen.h>

/* WARNING: some of these constants are also defined in linker.lds
 * These constants are written out in full instead of using bit arithmetic
 * because they need to defined like this in linker.lds
 */
#define PADDR_BASE  0x00000000ul
#define PADDR_LOAD  0x00100000ul
/* our kernel window is 2^39 bits (2^9 * 1gb) and the virtual address
 * range is 48 bits. Therefore our base is 2^48 - 2^39
 */
#define PPTR_BASE   0xffffff8000000000ul

/* The kernel binary itself is placed in the bottom 1gb of the top
 * 2gb of virtual address space. This is so we can use the 'kernel'
 * memory model of GCC, which requires all symbols to be linked
 * within the top 2GiB of memory. This is (2^48 - 2 ^ 31) */
#define KERNEL_BASE 0xffffffff80000000ul

/* Put the kernel devices at the very beginning of the top
 * 1GB. This means they are precisely after the kernel binary
 * region. This is 2^48 - 2^30
 */
#define PPTR_KDEV 0xffffffffc0000000ul

/* PADDR_TOP is the end of our larger kernel window, just before the
 * kernel image itself */
#define PADDR_TOP (KERNEL_BASE - PPTR_BASE)

/* Define the top of our static 'kernel window', which is the top 1GiB of memory */
#define PADDR_HIGH_TOP (PPTR_KDEV - KERNEL_BASE)

/* Below the main kernel window we have any slots for the TLB bitmap */
#define TLBBITMAP_PML4_RESERVED (TLBBITMAP_ROOT_ENTRIES * BIT(PML4_INDEX_OFFSET))
#define TLBBITMAP_PPTR (PPTR_BASE - TLBBITMAP_PML4_RESERVED)

/* The start of the this TLB bitmap becomes the highest valid user address */
#define PPTR_USER_TOP TLBBITMAP_PPTR
#define KERNEL_BASE_OFFSET (KERNEL_BASE - PADDR_BASE)
#define kernelBase KERNEL_BASE

/* The maximum physical address for device untypeds that we export to
 * the user */
#define PADDR_USER_DEVICE_TOP BIT(47)

#define BASE_OFFSET PPTR_BASE

/* since we have two kernel VM windows, we have two pptr to paddr
 * conversion functions.
 * paddr_to_kpptr converts physical address to the second small kernel
 * window which locates at the top 2GiB.
 */
static inline void* CONST
paddr_to_kpptr(paddr_t paddr)
{
    assert(paddr < PADDR_HIGH_TOP);
    return (void*)(paddr + KERNEL_BASE_OFFSET);
}

static inline paddr_t CONST
kpptr_to_paddr(void *pptr)
{
    assert((word_t)pptr >= KERNEL_BASE);
    return (paddr_t)pptr - KERNEL_BASE_OFFSET;
}

#endif /* __PLAT_MODE_MACHINE_HARDWARE_H_ */
