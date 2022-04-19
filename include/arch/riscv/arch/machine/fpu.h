/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <arch/machine/registerset.h>
#include <arch/machine/hardware.h>
#include <arch/smp/ipi_inline.h>

static inline void set_fs_off(void)
{
    asm volatile("csrc sstatus, %0" :: "rK"(SSTATUS_FS));
}

#ifdef CONFIG_HAVE_FPU
#if defined(CONFIG_RISCV_EXT_D)

#define FL "fld"
#define FS "fsd"
#define FP_REG_BYTES "8"

#elif defined(CONFIG_RISCV_EXT_F)

#define FL "flw"
#define FS "fsw"
#define FP_REG_BYTES "4"

#endif

extern bool_t isFPUEnabledCached[CONFIG_MAX_NUM_NODES];

static inline void set_fs_clean(void)
{
    asm volatile("csrs sstatus, %0" :: "rK"(SSTATUS_FS_CLEAN));
}

static inline void set_fs_initial(void)
{
    asm volatile("csrs sstatus, %0" :: "rK"(SSTATUS_FS_INITIAL));
}

static inline void set_fs_dirty(void)
{
    asm volatile("csrs sstatus, %0" :: "rK"(SSTATUS_FS_DIRTY));
}

static inline word_t read_sstatus_fs(void)
{
    return (read_sstatus() & SSTATUS_FS);
}

/* We unconditionally enable FPU accesses in kernel
 * mode for save and store functions. The field
 * we be set again before returning to user-mode
 * to actually enable/disable FPU accesses in
 * user mode.
 */
static inline void saveFpuState(user_fpu_state_t *dest)
{
    set_fs_clean();

    asm volatile(
        FS " f0,  0*"  FP_REG_BYTES "(%0)\n\t"
        FS " f1,  1*"  FP_REG_BYTES "(%0)\n\t"
        FS " f2,  2*"  FP_REG_BYTES "(%0)\n\t"
        FS " f3,  3*"  FP_REG_BYTES "(%0)\n\t"
        FS " f4,  4*"  FP_REG_BYTES "(%0)\n\t"
        FS " f5,  5*"  FP_REG_BYTES "(%0)\n\t"
        FS " f6,  6*"  FP_REG_BYTES "(%0)\n\t"
        FS " f7,  7*"  FP_REG_BYTES "(%0)\n\t"
        FS " f8,  8*"  FP_REG_BYTES "(%0)\n\t"
        FS " f9,  9*"  FP_REG_BYTES "(%0)\n\t"
        FS " f10, 10*" FP_REG_BYTES "(%0)\n\t"
        FS " f11, 11*" FP_REG_BYTES "(%0)\n\t"
        FS " f12, 12*" FP_REG_BYTES "(%0)\n\t"
        FS " f13, 13*" FP_REG_BYTES "(%0)\n\t"
        FS " f14, 14*" FP_REG_BYTES "(%0)\n\t"
        FS " f15, 15*" FP_REG_BYTES "(%0)\n\t"
        FS " f16, 16*" FP_REG_BYTES "(%0)\n\t"
        FS " f17, 17*" FP_REG_BYTES "(%0)\n\t"
        FS " f18, 18*" FP_REG_BYTES "(%0)\n\t"
        FS " f19, 19*" FP_REG_BYTES "(%0)\n\t"
        FS " f20, 20*" FP_REG_BYTES "(%0)\n\t"
        FS " f21, 21*" FP_REG_BYTES "(%0)\n\t"
        FS " f22, 22*" FP_REG_BYTES "(%0)\n\t"
        FS " f23, 23*" FP_REG_BYTES "(%0)\n\t"
        FS " f24, 24*" FP_REG_BYTES "(%0)\n\t"
        FS " f25, 25*" FP_REG_BYTES "(%0)\n\t"
        FS " f26, 26*" FP_REG_BYTES "(%0)\n\t"
        FS " f27, 27*" FP_REG_BYTES "(%0)\n\t"
        FS " f28, 28*" FP_REG_BYTES "(%0)\n\t"
        FS " f29, 29*" FP_REG_BYTES "(%0)\n\t"
        FS " f30, 30*" FP_REG_BYTES "(%0)\n\t"
        FS " f31, 31*" FP_REG_BYTES "(%0)\n\t"
        :
        : "r"(&dest->regs[0])
        : "memory"
    );

    dest->fcsr = read_fcsr();
}

static inline void loadFpuState(user_fpu_state_t *src)
{
    set_fs_clean();

    asm volatile(
        FL " f0,  0*"  FP_REG_BYTES "(%0)\n\t"
        FL " f1,  1*"  FP_REG_BYTES "(%0)\n\t"
        FL " f2,  2*"  FP_REG_BYTES "(%0)\n\t"
        FL " f3,  3*"  FP_REG_BYTES "(%0)\n\t"
        FL " f4,  4*"  FP_REG_BYTES "(%0)\n\t"
        FL " f5,  5*"  FP_REG_BYTES "(%0)\n\t"
        FL " f6,  6*"  FP_REG_BYTES "(%0)\n\t"
        FL " f7,  7*"  FP_REG_BYTES "(%0)\n\t"
        FL " f8,  8*"  FP_REG_BYTES "(%0)\n\t"
        FL " f9,  9*"  FP_REG_BYTES "(%0)\n\t"
        FL " f10, 10*" FP_REG_BYTES "(%0)\n\t"
        FL " f11, 11*" FP_REG_BYTES "(%0)\n\t"
        FL " f12, 12*" FP_REG_BYTES "(%0)\n\t"
        FL " f13, 13*" FP_REG_BYTES "(%0)\n\t"
        FL " f14, 14*" FP_REG_BYTES "(%0)\n\t"
        FL " f15, 15*" FP_REG_BYTES "(%0)\n\t"
        FL " f16, 16*" FP_REG_BYTES "(%0)\n\t"
        FL " f17, 17*" FP_REG_BYTES "(%0)\n\t"
        FL " f18, 18*" FP_REG_BYTES "(%0)\n\t"
        FL " f19, 19*" FP_REG_BYTES "(%0)\n\t"
        FL " f20, 20*" FP_REG_BYTES "(%0)\n\t"
        FL " f21, 21*" FP_REG_BYTES "(%0)\n\t"
        FL " f22, 22*" FP_REG_BYTES "(%0)\n\t"
        FL " f23, 23*" FP_REG_BYTES "(%0)\n\t"
        FL " f24, 24*" FP_REG_BYTES "(%0)\n\t"
        FL " f25, 25*" FP_REG_BYTES "(%0)\n\t"
        FL " f26, 26*" FP_REG_BYTES "(%0)\n\t"
        FL " f27, 27*" FP_REG_BYTES "(%0)\n\t"
        FL " f28, 28*" FP_REG_BYTES "(%0)\n\t"
        FL " f29, 29*" FP_REG_BYTES "(%0)\n\t"
        FL " f30, 30*" FP_REG_BYTES "(%0)\n\t"
        FL " f31, 31*" FP_REG_BYTES "(%0)\n\t"
        :
        : "r"(&src->regs[0])
    );

    write_fcsr(src->fcsr);
}

static inline void enableFpu(void)
{
    isFPUEnabledCached[CURRENT_CPU_INDEX()] = true;
}

static inline void disableFpu(void)
{
    isFPUEnabledCached[CURRENT_CPU_INDEX()] = false;
}

static inline bool_t isFpuEnable(void)
{
    return isFPUEnabledCached[CURRENT_CPU_INDEX()];
}

static inline void set_tcb_fs_state(tcb_t *tcb, bool_t enabled)
{
    word_t sstatus = getRegister(tcb, SSTATUS);
    sstatus &= ~SSTATUS_FS;
    if (enabled) {
        sstatus |= SSTATUS_FS_CLEAN;
    }
    setRegister(tcb, SSTATUS, sstatus);
}

#endif /* end of CONFIG_HAVE_FPU */
