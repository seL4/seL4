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

/*
 *
 */

#ifndef __ARCH_MACHINE_FPU_H
#define __ARCH_MACHINE_FPU_H

#include <machine/registerset.h>
#include <util.h>

#ifdef CONFIG_HAVE_FPU
static inline bool_t isDirtyFPU(void)
{
    word_t sstatus = NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS];
    /* Check SD Flag in sstatus */
    // RVTODO: Give SD Flag a definition. sstatus in bitfield perhaps?
    return !!(sstatus & BIT(CONFIG_WORD_SIZE - 1));
}

static inline bool_t cleanFPUState(void)
{
    word_t sstatus = NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS];
    sstatus = (sstatus & ~SSTATUS_FS) | SSTATUS_FS_CLEAN;
    NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS] = sstatus;
}

/* Store state in the FPU registers into memory. */
static inline void saveFpuState(user_fpu_state_t *dest)
{
    word_t temp_fcsr = 0;

    if (isDirtyFPU()) {
        /* Save FPU state */
        asm volatile(
            "csrr %[temp_fcsr], fcsr\n"
            STORE_S " %[temp_fcsr], 0(%[fcsr])\n"

            FSTORE_S "  f0, (0*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f1, (1*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f2, (2*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f3, (3*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f4, (4*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f5, (5*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f6, (6*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f7, (7*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f8, (8*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f9, (9*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f10, (10*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f11, (11*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f12, (12*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f13, (13*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f14, (14*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f15, (15*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f16, (16*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f17, (17*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f18, (18*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f19, (19*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f20, (20*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f21, (21*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f22, (22*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f23, (23*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f24, (24*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f25, (25*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f26, (26*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f27, (27*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f28, (28*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f29, (29*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f30, (30*%[FREGSIZE])(%[fpregs])\n"
            FSTORE_S "  f31, (31*%[FREGSIZE])(%[fpregs])\n"

            : [temp_fcsr] "+r" (temp_fcsr)
            : [fpregs] "r" (&dest->fpregs[0]),
            [fcsr] "r" (&dest->fcsr),
            [FREGSIZE] "i" (sizeof(dest->fpregs[0]))
            : "memory"
        );

        cleanFPUState();
    }

}

/* Load FPU state from memory into the FPU registers. */
static inline void loadFpuState(user_fpu_state_t *src)
{

    // RVTODO: This comment or function name is wrong as 'dirty' and 'on'
    // are not the same thing. Overall something is very wrong with the
    // logic in this function
    /* FPU is not off */
    if (!isDirtyFPU()) {
        word_t temp_fcsr = 0;

        asm volatile(
            FLOAD_S "  f0, (0*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f1, (1*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f2, (2*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f3, (3*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f4, (4*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f5, (5*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f6, (6*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f7, (7*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f8, (8*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f9, (9*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f10, (10*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f11, (11*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f12, (12*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f13, (13*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f14, (14*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f15, (15*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f16, (16*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f17, (17*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f18, (18*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f19, (19*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f20, (20*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f21, (21*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f22, (22*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f23, (23*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f24, (24*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f25, (25*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f26, (26*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f27, (27*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f28, (28*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f29, (29*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f30, (30*%[FREGSIZE])(%[fpregs])\n"
            FLOAD_S "  f31, (31*%[FREGSIZE])(%[fpregs])\n"

            LOAD_S " %[temp_fcsr], 0(%[fcsr])\n"
            "csrw fcsr, %[temp_fcsr]\n"

            : [temp_fcsr] "+r" (temp_fcsr)
            : [fpregs] "r" (&src->fpregs[0]),
            [fcsr] "r" (&src->fcsr),
            [FREGSIZE] "i" (sizeof(src->fpregs[0]))
        );

        // RVTODO: should this be here?
        /* set fpu to clean state */
        //cleanFPUState();
    } else {
        // RVTODO: should this be a panic then?
        printf("FPU restore should never be dirty\n");
    }
}

/* Check if FPU is enable */
static inline bool_t isFpuEnable(void)
{
    word_t sstatus = NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS];
    return !!(sstatus & SSTATUS_FS);
}

/* Enable the FPU to be used without faulting.
 * Required even if the kernel attempts to use the FPU. */
static inline void enableFpu(void)
{
    word_t sstatus = NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS];
    // RVTODO: why do enableFpu and disableFpu fiddle with different bits?
    sstatus |=  SSTATUS_FS_INITIAL;
    NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS] = sstatus;

    write_csr(sstatus, sstatus);
}
#endif /* CONFIG_HAVE_FPU */

/* Disable the FPU so that usage of it causes a fault */
static inline void disableFpu(void)
{
    word_t sstatus = NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS];
    sstatus &=  ~SSTATUS_FS;
    NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[SSTATUS] = sstatus;
}
#endif /* __ARCH_MACHINE_FPU_H */
