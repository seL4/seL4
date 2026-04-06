/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <arch/machine/hardware.h>
#include <sel4/plat/api/constants.h>

/*
 * For information on the architectural details of the Armv8-A address translation,
 * refer to the ARM "Armv8-A Address Translation", document ID 100940.
 * https://developer.arm.com/documentation/100940/latest/
 *
 * !defined(CONFIG_ARM_HYPERVISOR_SUPPORT)
 *
 *    Per ARM ARM DDI 0487 (version L.b), in D8.2.4, VA bit[55] selects between
 *    TTBR1_EL1 and TTBR0_EL1, the two Translation Table base registers.
 *    seL4 follows the convention to use the "higher VA" (TTBR1_EL1) for kernel
 *    memory and the "lower VA" (TTBR0_EL1) for userspace memory.
 *    seL4 also does not enable the ARM "Address tagging" (D8.8) feature (
 *    sometimes known as Top Byte Ignore) so virtual addresses need to be sign
 *    extended in VA[63:56].
 *
 *    seL4 chooses to map only a single entry of the PGD translation base,
 *    using a singular PUD that covers 2^39 bits of virtual address space.
 *    Note that 2^39, 2^30, 2^21 corresponds to the memory ranges covered by
 *    a PUD, PD, and PT respectively.
 *
 *                             (PGD)
 *                               |
 *                               v
 *                2^64 +-------------------+                --+
 *                     |                   |                  |
 *                     | Kernel Mem Region | --->  (PUD)      |
 *                     |                   |                  |
 *         2^64 - 2^39 +-------------------+                  | Kernel Space (TTBR1)
 *                     |                   |                  |
 *                     |     Unmapped      |                  |
 *                     |                   |                  |
 *         2^64 - 2^48 +-------------------+                --+
 *                     |                   |
 *                     |                   |
 *                     |   Unaddressable   |
 *                     |                   |
 *                     |                   |
 *                2^48 +-------------------+                --+
 *                     |     Unmapped      |                  |
 *                     +-------------------+ USER_TOP         |
 *                     |                   |                  | User Space (TTBR0)
 *                     |       User        |                  |
 *                     |                   |                  |
 *                 0x0 +-------------------+                --+
 *
 *
 *    The Kernel's PUD (`armKSGlobalKernelPUD`) is then split into several parts.
 *
 *    -  The physical memory window starts from the 0th PUD entry and extends
 *       up to the second-to-last PUD entry. This covers 255GiB and contains the
 *       virtual address for which the kernel ELF is mapped in to.
 *
 *       The physical memory window is mapped using Large Pages.
 *
 *    -  The top entry of the PUD then contains the "kernel mappings" (more details below).
 *
 *                                (PUD)
 *                                  |
 *                                  v
 *                   2^64 +-------------------+
 *                        |                   |
 *                        |  Kernel Mappings  | --> (PD)
 *                        |                   |
 *            2^64 - 2^30 +-------------------+ PPTR_TOP
 *                        |                   |
 *                        |                   |
 *                        |  Physical Memory  |
 *                        |      Window       |
 *                        |                   |
 *                        |                   |
 *                        | ----------------- | KERNEL_ELF_TOP
 *                        |     Kernel ELF    |
 *                        | ----------------- | KERNEL_ELF_BASE (at `physBase()`)
 *                        |                   |
 *                        |                   |
 *            2^64 - 2^39 +-------------------+ PPTR_BASE
 *
 *
 *    The kernel mappings PD (code: `armKSGlobalKernelPDs[kernel_mappings_pud_idx]`)
 *    then contains the:
 *
 *    -  Kernel Devices page table in the last entry of the PD. This points to the
 *       `armKSGlobalKernelPT` which points to 4K device pages.
 *    -  Depending on configuration, the benchmarking log buffer.
 *
 *                             (PD )
 *                               |
 *                               v
 *                 2^64 +------------------+
 *                      |                  |
 *                      |  Kernel Devices  | --> (PT)
 *                      |                  |
 *          2^64 - 2^21 +------------------+ KDEV_BASE / KERNEL_PT_BASE
 *                      |                  |
 *                      |    Log Buffer    |
 *                      |                  |
 *      2^64 - 2 * 2^21 +------------------+ KS_LOG_PPTR
 *                      |                  |
 *                      |                  |
 *                      |     Unmapped     |
 *                      |                  |
 *                      |                  |
 *          2^64 - 2^30 +------------------+
 */

/*
 * defined(CONFIG_ARM_HYPERVISOR_SUPPORT)
 *
 *    EL2 kernel address map:
 *
 *    The EL2 mode kernel uses TTBR0_EL2 which covers the range of
 *    0x0 - 0x0000ffffffffffff (48 bits of vaddrspace).
 *
 *    The layout of the kernel's virtual address space is very similar to
 *    non-hyp, but the kernel address space is distinct to userland (EL1/EL0).
 *    In EL2 there is only a TTBR0_EL2 register (unlike EL1 with TTBR0 and TTBR1)
 *    so the kernel is linked in the low 2^48 bits of the virtual address space,
 *    or "lower VA" in EL1 parlance.
 *
 *    The major difference to non-hyp mode, is that the kernel mem region is
 *    located in the second entry of the TTBR0_EL2 page table (PGD) rather than
 *    the topmost. This is because:
 *
 *     -  The kernel expects to be able to create untypeds from 0 to
 *        KernelPaddrUserTop (CONFIG_PADDR_USER_TOP) which can be up to 2^44 in
 *        the current kernel. The kernel (proofs) expect addresses in these
 *        untypeds to be valid (i.e. canonical, not dereferencable) virtual
 *        addresses, but allows device UT addresses to exceed ("overflow") the
 *        bounds of the physical memory window (PPTR_BASE...PPTR_TOP) as it will
 *        not dereference them itself. In the EL1 translation regime, as we use
 *        the "upper VA" these kind of address overflows remain canonical as
 *        they wraparound into the "lower VA", which still consists of valid
 *        addresses. In the EL2 translation regime, which uses TTBR0_EL2 and
 *        virtual addresses are always between [0, 2^48), when we exceed the
 *        top of this address range the virtual addresses are non-canonical,
 *        and therefore no longer valid.
 *
 *        The solution was to move the physical memory window (and thus the
 *        offset between physical and virtual addresses) down such that the full
 *        UT paddr range of 2^44 can fit within [0, 2^48). This likely will not
 *        work when the kernel implements adds support for 48-bit PA.
 *
 *     -  We use the second entry, not the first, as the lowest 512GiB of
 *        virtual addresses conflicted elfloader's direct mapping during boot.
 *
 *     -  Also see commit a94d90598fec484636b14e1b8d3643156e1e27b1, which
 *        introduced this behaviour.
 *
 *    This gives us the address range from 2^39 to 2^40 (2^39 + 2^39).
 *
 *    For clarity, the diagrams are replicated below with adjusted
 *    virtual addresses.
 *
 *
 *           2^64 +-------------------+
 *                |                   |
 *                |                   |
 *                |   Unaddressable   |
 *                |                   |
 *                |                   |
 *           2^48 +-------------------+
 *                |                   |
 *                |                   |
 *                |     Unmapped      |
 *                |                   |
 *                |                   |
 *           2^40 +-------------------+
 *                |                   |
 *                | Kernel Mem Region | ---+ (PUD)
 *                |                   |
 *           2^39 +-------------------+
 *                |                   |
 *                |     Unmapped      |
 *                |                   |
 *            0x0 +-------------------+
 *
 *
 *                                (PUD)
 *                                  |
 *                                  v
 *                   2^40 +-------------------+
 *                        |                   |
 *                        |  Kernel Mappings  | --> (PD)
 *                        |                   |
 *            2^40 - 2^30 +-------------------+ PPTR_TOP
 *                        |                   |
 *                        |                   |
 *                        |                   |
 *                        |  Physical Memory  |
 *                        |      Window       |
 *                        |                   |
 *                        |                   |
 *                        | ----------------- | KERNEL_ELF_TOP
 *                        |     Kernel ELF    |
 *                        | ----------------- | KERNEL_ELF_BASE (at `physBase()`)
 *                        |                   |
 *                        |                   |
 *            2^40 - 2^39 +-------------------+ PPTR_BASE
 *
 *
 *                             (PD )
 *                               |
 *                               v
 *                 2^40 +------------------+
 *                      |                  |
 *                      |  Kernel Devices  | --> (PT)
 *                      |                  |
 *          2^40 - 2^21 +------------------+ KDEV_BASE / KERNEL_PT_BASE
 *                      |                  |
 *                      |    Log Buffer    |
 *                      |                  |
 *      2^40 - 2 * 2^21 +------------------+ KS_LOG_PPTR
 *                      |                  |
 *                      |                  |
 *                      |     Unmapped     |
 *                      |                  |
 *                      |                  |
 *          2^40 - 2^30 +------------------+
 *
 */

/* last accessible virtual address in user space */
#define USER_TOP seL4_UserTop

/* The first physical address to map into the kernel's physical memory
 * window */
#define PADDR_BASE UL_CONST(0x0)

/* The base address in virtual memory to use for the 1:1 physical memory
 * mapping */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PPTR_BASE UL_CONST(0x0000008000000000)  /* 2^40 - 2^39 */
#else
#define PPTR_BASE UL_CONST(0xffffff8000000000)  /* 2^64 - 2^39 */
#endif

/* Top of the physical memory window, exclusive */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PPTR_TOP UL_CONST(0x000000ffc0000000)  /* 2^40 - 2^30 */
#else
#define PPTR_TOP UL_CONST(0xffffffffc0000000)  /* 2^64 - 2^30 */
#endif

/* The physical memory address to use for mapping the kernel ELF */
#define KERNEL_ELF_PADDR_BASE physBase()
/* For use by the linker (only integer constants allowed) */
#define KERNEL_ELF_PADDR_BASE_RAW PHYS_BASE_RAW

/* The base address in virtual memory to use for the kernel ELF mapping */
#define KERNEL_ELF_BASE (PPTR_BASE_OFFSET + KERNEL_ELF_PADDR_BASE)
/* For use by the linker (only integer constants allowed) */
#define KERNEL_ELF_BASE_RAW (PPTR_BASE_OFFSET + KERNEL_ELF_PADDR_BASE_RAW)

/* This is a page table mapping at the end of the virtual address space
 * to map objects with 4KiB pages rather than 2MiB large pages. */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define KERNEL_PT_BASE UL_CONST(0x000000ffffe00000)  /* 2^40 - 2^21 */
#else
#define KERNEL_PT_BASE UL_CONST(0xffffffffffe00000)  /* 2^64 - 2^21 */
#endif

/* The base address in virtual memory to use for the kernel device
 * mapping region. These are mapped in the kernel page table. */
#define KDEV_BASE KERNEL_PT_BASE

/* The log buffer is placed before the device region */
#define KS_LOG_PPTR (KDEV_BASE - UL_CONST(0x200000))

#ifndef __ASSEMBLER__
/* All PPTR addresses must be canonical to be able to be stored in caps or objects.
   Check that all UTs that are created will have valid address in the PPTR space.
   For non-hyp, PPTR_BASE is in the top part of the address space and device untyped
   addresses are allowed to be large enough to overflow and be in the bottom half of
   the address space.  However, when the kernel is in EL2 it is not possible to safely
   overflow without going into address ranges that are non-canonical.  These static
   asserts check that the kernel config won't lead to UTs being created that aren't
   representable. */
compile_assert(ut_max_less_than_canonical, CONFIG_PADDR_USER_DEVICE_TOP <= BIT(47));
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
compile_assert(ut_max_is_canonical, (PPTR_BASE + CONFIG_PADDR_USER_DEVICE_TOP) <= BIT(48));
#endif
#endif
