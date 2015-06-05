/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_64_H
#define __PLAT_MACHINE_HARDWARE_64_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <plat/machine/hardware_gen.h>

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

#define PPTR_USER_TOP KERNEL_BASE
#define KERNEL_BASE_OFFSET (KERNEL_BASE - PADDR_BASE)

/* since we have two kernel VM windows, we have two pptr to paddr
 * conversion functions.
 * paddr_to_kpptr converts physical address to the second small kernel
 * window which locates at the top 2GiB.
 */
static inline void* CONST
paddr_to_kpptr(paddr_t paddr)
{
    if (paddr < PADDR_HIGH_TOP) {
        return (void*)(paddr + KERNEL_BASE_OFFSET);
    } else {
        printf("Possible kernel window violation. Assume translation of device frame\n");
        return (void*)(paddr + KERNEL_BASE_OFFSET);
    }
}

static inline paddr_t CONST
kpptr_to_paddr(void *pptr)
{
    if ((word_t)pptr >= KERNEL_BASE) {
        return (paddr_t)pptr - KERNEL_BASE_OFFSET;
    } else {
        printf("Possible kernel window violation. Assume translation of device frame\n");
        return (paddr_t)pptr - KERNEL_BASE_OFFSET;
    }
}

/* paddr_to_pptr converts a physical address to the large kernel
 * window which starts at PPTR_BASE and directly maps all physical
 * memory.
 */
static inline void* CONST
paddr_to_pptr(paddr_t paddr)
{
    return (void *)(paddr + PPTR_BASE);
}

static inline paddr_t CONST
pptr_to_paddr(void* pptr)
{
    return (paddr_t)pptr - PPTR_BASE;
}

static inline region_t CONST
paddr_to_pptr_reg(p_region_t p_reg)
{
    return (region_t) {
        (word_t)paddr_to_pptr(p_reg.start), (word_t)paddr_to_pptr(p_reg.end)
    };
}

static inline p_region_t CONST
pptr_to_paddr_reg(region_t reg)
{
    return (p_region_t) {
        pptr_to_paddr((void*)reg.start), pptr_to_paddr((void*)reg.end)
    };
}

void handleReservedIRQ(irq_t irq);
void maskInterrupt(bool_t mask, irq_t irq);
void ackInterrupt(irq_t irq);
irq_t getActiveIRQ(void);
bool_t isIRQPending(void);
void resetTimer(void);
void platAddDevices(void);

void handleSpuriousIRQ(void);

#endif
