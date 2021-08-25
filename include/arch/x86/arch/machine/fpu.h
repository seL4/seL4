/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <arch/object/vcpu.h>
#include <object/structures.h>
#include <model/statedata.h>
#include <arch/machine/cpu_registers.h>

#define MXCSR_INIT_VALUE               0x1f80
#define XCOMP_BV_COMPACTED_FORMAT      (1ull << 63)

/* The state format, as saved by FXSAVE and restored by FXRSTOR instructions. */
typedef struct i387_state {
    uint16_t cwd;               /* control word */
    uint16_t swd;               /* status word */
    uint16_t twd;               /* tag word */
    uint16_t fop;               /* last instruction opcode */
    uint32_t reserved[4];       /* instruction and data pointers */
    uint32_t mxcsr;             /* MXCSR register state */
    uint32_t mxcsr_mask;        /* MXCSR mask */
    uint32_t st_space[32];      /* FPU registers */
    uint32_t xmm_space[64];     /* XMM registers */
    uint32_t padding[13];
} PACKED i387_state_t;

/* The state format, as saved by XSAVE and restored by XRSTOR instructions. */
typedef struct xsave_state {
    i387_state_t i387;
    struct {
        uint64_t xfeatures;
        uint64_t xcomp_bv;      /* state-component bitmap */
        uint64_t reserved[6];
    } header;
} PACKED xsave_state_t;

/* Initialise the FPU. */
bool_t Arch_initFpu(void);

/* Initialise the FPU state of the given user context. */
void Arch_initFpuContext(user_context_t *context);

static inline uint32_t xsave_features_high(void)
{
    uint64_t features = config_ternary(CONFIG_XSAVE, CONFIG_XSAVE_FEATURE_SET, 1);
    return (uint32_t)(features >> 32);
}

static inline uint32_t xsave_features_low(void)
{
    uint64_t features = config_ternary(CONFIG_XSAVE, CONFIG_XSAVE_FEATURE_SET, 1);
    return (uint32_t)(features & 0xffffffff);
}

/* Store state in the FPU registers into memory. */
static inline void saveFpuState(user_fpu_state_t *dest)
{
    if (config_set(CONFIG_FXSAVE)) {
        asm volatile("fxsave %[dest]" : [dest] "=m"(*dest));
    } else if (config_set(CONFIG_XSAVE_XSAVEOPT)) {
        asm volatile("xsaveopt %[dest]" : [dest] "=m"(*dest) : "d"(xsave_features_high()), "a"(xsave_features_low()));
    } else if (config_set(CONFIG_XSAVE_XSAVE)) {
        asm volatile("xsave %[dest]" : [dest] "=m"(*dest) : "d"(xsave_features_high()), "a"(xsave_features_low()));
    } else if (config_set(CONFIG_XSAVE_XSAVEC)) {
        asm volatile("xsavec %[dest]" : [dest] "=m"(*dest) : "d"(xsave_features_high()), "a"(xsave_features_low()));
    } else if (config_set(CONFIG_XSAVE_XSAVES)) {
        asm volatile("xsaves %[dest]" : [dest] "=m"(*dest) : "d"(xsave_features_high()), "a"(xsave_features_low()));
    }
}

/* Load FPU state from memory into the FPU registers. */
static inline void loadFpuState(user_fpu_state_t *src)
{
    if (config_set(CONFIG_FXSAVE)) {
        asm volatile("fxrstor %[src]" :: [src] "m"(*src));
    } else if (config_set(CONFIG_XSAVE)) {
        if (config_set(CONFIG_XSAVE_XSAVES)) {
            asm volatile("xrstors %[src]" :: [src] "m"(*src), "d"(xsave_features_high()), "a"(xsave_features_low()));
        } else {
            asm volatile("xrstor %[src]" :: [src] "m"(*src), "d"(xsave_features_high()), "a"(xsave_features_low()));
        }
    }
}

/* Reset the FPU registers into their initial blank state. */
static inline void finit(void)
{
    asm volatile("finit" :: "m"(control_reg_order));
}

/*
 * Enable the FPU to be used without faulting.
 * Required even if the kernel attempts to use the FPU.
 */
static inline void enableFpu(void)
{
    asm volatile("clts" :: "m"(control_reg_order));
}

/*
 * Disable the FPU so that usage of it causes a fault
 */
static inline void disableFpu(void)
{
    write_cr0(read_cr0() | CR0_TASK_SWITCH);
}

#ifdef CONFIG_VTX
static inline bool_t vcpuThreadUsingFPU(tcb_t *thread)
{
    return thread->tcbArch.tcbVCPU && &thread->tcbArch.tcbVCPU->fpuState == NODE_STATE(ksActiveFPUState);
}
#endif /* CONFIG_VTX */

