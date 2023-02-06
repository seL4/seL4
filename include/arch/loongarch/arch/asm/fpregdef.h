/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Definitions for the FPU register names
 *
 * Copyright (C) 2020-2021 Loongson Technology Corporation Limited
 */
#ifndef _ASM_FPREGDEF_H
#define _ASM_FPREGDEF_H

#include <arch/asm/abidefs.h>

#if _LOONGARCH_SIM == _LOONGARCH_SIM_ABILP64

#define fv0	$f0	/* return value */
#define fv1	$f2
#define fa0	$f12	/* argument registers */
#define fa1	$f13
#define fa2	$f14
#define fa3	$f15
#define fa4	$f16
#define fa5	$f17
#define fa6	$f18
#define fa7	$f19
#define ft0	$f4	/* caller saved */
#define ft1	$f5
#define ft2	$f6
#define ft3	$f7
#define ft4	$f8
#define ft5	$f9
#define ft6	$f10
#define ft7	$f11
#define ft8	$f20
#define ft9	$f21
#define ft10	$f22
#define ft11	$f23
#define ft12	$f1
#define ft13	$f3
#define fs0	$f24	/* callee saved */
#define fs1	$f25
#define fs2	$f26
#define fs3	$f27
#define fs4	$f28
#define fs5	$f29
#define fs6	$f30
#define fs7	$f31

#endif /* _LOONGARCH_SIM == _LOONGARCH_SIM_ABILP64 */

#define fcsr0	$r0
#define fcsr1	$r1
#define fcsr2	$r2
#define fcsr3	$r3
#define vcsr16	$r16

#endif /* _ASM_FPREGDEF_H */
