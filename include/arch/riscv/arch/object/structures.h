/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifndef __ASSEMBLER__
#include <config.h>
#include <assert.h>
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>
#include <mode/object/structures.h>

#define tcbArchCNodeEntries tcbCNodeEntries

struct asid_pool {
    pte_t *array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)
#define ASID_BITS           (asidHighBits + asidLowBits)
#define nASIDPools          BIT(asidHighBits)
#define ASID_LOW(a)         (a & MASK(asidLowBits))
#define ASID_HIGH(a)        ((a >> asidLowBits) & MASK(asidHighBits))

#ifdef CONFIG_HAVE_FPU

#define RISCV_NUM_FP_REGS   32

#if defined(CONFIG_RISCV_EXT_D)
typedef uint64_t fp_reg_t;
#elif defined(CONFIG_RISCV_EXT_F)
typedef uint32_t fp_reg_t;
#else
#error Unknown RISCV FPU extension
#endif

typedef struct fpu {
    /* 
     * For EXT_D and EXT_Q we can remove a single register,
     * but for EXT_F we'll need to either remove 1 (RISCV32) or 2 (RISCV64).
     * 
     * There is space for optimization here but it's skipped for cleanness.
     */
#if defined(CONFIG_RISCV_EXT_F) && defined(CONFIG_ARCH_RISCV64)
    fp_reg_t regs[RISCV_NUM_FP_REGS-2];
#else
    fp_reg_t regs[RISCV_NUM_FP_REGS-1];
#endif

    /* Backlink from fpu to TCB */
    struct tcb *fpuBoundTCB;
} fpu_t;

compile_assert(fpu_object_size_correct, sizeof(fpu_t) == BIT(seL4_FPUBits));

typedef struct tcb_fpu {
    /* Object created from retyping an untyped */
    fpu_t *tcbBoundFpu;
    
    /* Control status register */
    uint32_t fcsr;

    /* Last fp register(s) in the fpu */
#if defined(CONFIG_RISCV_EXT_F) && defined(CONFIG_ARCH_RISCV64)
    fp_reg_t regs[2];
#else
    fp_reg_t regs[1];
#endif
} tcb_fpu_t;
#endif

typedef struct arch_tcb {
    user_context_t tcbContext;

#ifdef CONFIG_HAVE_FPU
    tcb_fpu_t tcbFpu;
#endif
} arch_tcb_t;

enum vm_rights {
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef word_t vm_rights_t;

typedef pte_t vspace_root_t;

/* Generic fastpath.c code expects pde_t for stored_hw_asid
 * that's a workaround in the time being.
 */
typedef pte_t pde_t;

#define PTE_PTR(r) ((pte_t *)(r))
#define PTE_REF(p) ((word_t)(p))

#define PT_SIZE_BITS 12
#define PT_PTR(r) ((pte_t *)(r))
#define PT_REF(p) ((word_t)(p))

#define PTE_SIZE_BITS   seL4_PageTableEntryBits
#define PT_INDEX_BITS   seL4_PageTableIndexBits

#define WORD_BITS   (8 * sizeof(word_t))
#define WORD_PTR(r) ((word_t *)(r))

#define FPU_PTR(r)  ((fpu_t *)(r))
#define FPU_REF(p)  ((word_t)(p))

static inline bool_t CONST cap_get_archCapIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return true;

    case cap_page_table_cap:
        return true;

    case cap_asid_control_cap:
        return false;

    case cap_asid_pool_cap:
        return true;

    default:
        /* unreachable */
        return false;
    }
}

static inline word_t CONST cap_get_archCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return PT_SIZE_BITS;

    case cap_asid_control_cap:
        return 0;

    case cap_asid_pool_cap:
        return seL4_ASIDPoolBits;

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline void *CONST cap_get_archCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return (void *)(cap_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return NULL;
    }
}

static inline bool_t CONST Arch_isCapRevocable(cap_t derivedCap, cap_t srcCap)
{
    return false;
}

#endif /* !__ASSEMBLER__  */
