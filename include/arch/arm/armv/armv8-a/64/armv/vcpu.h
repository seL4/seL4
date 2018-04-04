/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_ARMV_VCPU_H_
#define __ARCH_ARMV_VCPU_H_

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <arch/object/vcpu.h>

/* Note that the HCR_DC for ARMv8 disables S1 translation if enabled */
/* Trap WFI/WFE/SMC and override CPSR.AIF */
#define HCR_COMMON ( HCR_TWI | HCR_TWE | HCR_VM | HCR_RW | HCR_AMO | HCR_IMO | HCR_FMO )

/* Allow native tasks to run at EL0, but restrict access */
#define HCR_NATIVE ( HCR_COMMON | HCR_TGE | HCR_TVM | HCR_TTLB | HCR_DC \
                   | HCR_TAC | HCR_SWIO |  HCR_TSC | HCR_IMO | HCR_FMO | HCR_AMO)
#define HCR_VCPU   ( HCR_COMMON)

#define SCTLR_EL1_UCI   BIT(26)     /* Enable EL0 access to DC CVAU, DC CIVAC, DC CVAC,
                                       and IC IVAU in AArch64 state   */
#define SCTLR_EL1_C     BIT(2)      /* Enable data and unified caches */
#define SCTLR_EL1_I     BIT(12)     /* Enable instruction cache       */
/* Disable MMU, SP alignment check, and alignment check */
/* A57 default value */
#define SCTLR_EL1_NATIVE   (0x34d58820 | SCTLR_EL1_C | SCTLR_EL1_I | SCTLR_EL1_UCI)
#define SCTLR_EL1_VM       0x34d58820
#define SCTLR_DEFAULT      SCTLR_EL1_NATIVE

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */

#endif

