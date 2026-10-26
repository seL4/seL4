/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define CR0_PE              BIT(0)  /* Protected Mode Enable */
#define CR0_MONITOR_COPROC  BIT(1)  /* Trap on FPU "WAIT" commands. */
#define CR0_EMULATION       BIT(2)  /* Enable OS emulation of FPU. */
#define CR0_TASK_SWITCH     BIT(3)  /* Trap on any FPU usage, for lazy FPU. */
#define CR0_NUMERIC_ERROR   BIT(5)  /* Internally handle FPU problems. */
#define CR0_WRITE_PROTECT   BIT(16) /* Write protection in supervisor mode. */
#define CR0_PG              BIT(31) /* Paging On */
#define CR4_PCE             BIT(8)  /* Performance-Monitoring Counter enable. */
#define CR4_OSFXSR          BIT(9)  /* Enable SSE et. al. features. */
#define CR4_OSXMMEXCPT      BIT(10) /* Enable SSE exceptions. */
#define CR4_OSXSAVE         BIT(18) /* Enavle XSAVE feature set */
#define CR4_VMXE            BIT(13) /* Enable VMX mode. */
#define CR4_SMEP            BIT(20) /* Supervisor Mode Execution Prevention. */
#define CR4_SMAP            BIT(21) /* Supervisor Mode Access Prevention. */

#define FLAGS_CF            BIT(0)  /* Carry Flag */
#define FLAGS_PF            BIT(2)  /* Parity Flag */
#define FLAGS_AF            BIT(4)  /* Auxiliary carry Flag */
#define FLAGS_ZF            BIT(6)  /* Zero Flag */
#define FLAGS_SF            BIT(7)  /* Sign Flag */
#define FLAGS_TF            BIT(8)  /* Trap Flag */
#define FLAGS_IF            BIT(9)  /* Interrupt enable Flag */
#define FLAGS_DF            BIT(10) /* Direction Flag */
#define FLAGS_OF            BIT(11) /* Overflow Flag */
#define FLAGS_IOPL          (BIT(12) | BIT(13)) /* I/O Privilege Level */
#define FLAGS_NT            BIT(14) /* Nested Task */
#define FLAGS_RF            BIT(16) /* Resume Flag */
#define FLAGS_AC            BIT(18) /* Alignment Check / Access Control (SMAP override) */
#define FLAGS_ID            BIT(21) /* CPUID available */
#define FLAGS_HIGH          BIT(1)  /* Bits in the FLAGS register that must be high */
#define FLAGS_LOW           (BIT(3) | BIT(5)) /* Bits in the FLAGS register that must be low */
#define FLAGS_MASK          MASK(12)/* Only the first 12 bits of the FLAGS are used, rest should be zero */
#define FLAGS_USER_DEFAULT  FLAGS_IF | FLAGS_HIGH

/* Clear all available flags on kernel entry via SYSCALL (IA32_FMASK), so that
 * flag state is well defined when the kernel runs. This is the same set Linux
 * uses. User flags are saved in R11 by SYSCALL. */
#define FLAGS_SYSCALL_MASK  (FLAGS_CF | FLAGS_PF | FLAGS_AF | FLAGS_ZF | FLAGS_SF | FLAGS_TF | FLAGS_IF | \
                             FLAGS_DF | FLAGS_OF | FLAGS_IOPL | FLAGS_NT | FLAGS_RF | FLAGS_AC | FLAGS_ID)

/* We use a dummy variable to synchronize reads and writes to the control registers.
 * this allows us to write inline asm blocks that do not have enforced memory
 * clobbers for ordering. */
static unsigned long control_reg_order;

#include <mode/machine/cpu_registers.h>

static inline void xsetbv(uint32_t reg, uint64_t value)
{
    asm volatile("xsetbv" :: "d"((uint32_t)(value >> 32)), "a"((uint32_t)(value & 0xffffffff)), "c"(reg), "m"(control_reg_order));
}

static inline void write_xcr0(uint64_t value)
{
    xsetbv(0, value);
}

