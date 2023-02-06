/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Some useful macros for LoongArch assembler code
 *
 * Copyright (C) 2020-2021 Loongson Technology Corporation Limited
 *
 * Derived from MIPS:
 * Copyright (C) 1995, 1996, 1997, 1999, 2001 by Ralf Baechle
 * Copyright (C) 1999 by Silicon Graphics, Inc.
 * Copyright (C) 2001 MIPS Technologies, Inc.
 * Copyright (C) 2002  Maciej W. Rozycki
 */
#ifndef __ASM_ASM_H
#define __ASM_ASM_H

#include <arch/asm/abidefs.h>

/* LoongArch pref instruction. */
#ifdef CONFIG_CPU_HAS_PREFETCH

#define PREF(hint, addr, offs)				\
		preld	hint, addr, offs;		\

#define PREFX(hint, addr, index)			\
		preldx	hint, addr, index;		\

#else /* !CONFIG_CPU_HAS_PREFETCH */

#define PREF(hint, addr, offs)
#define PREFX(hint, addr, index)

#endif /* !CONFIG_CPU_HAS_PREFETCH */

/*
 * Stack alignment
 */
#if (_LOONGARCH_SIM == _LOONGARCH_SIM_ABILP64)
#define ALSZ	15
#define ALMASK	~15
#endif

/*
 * Macros to handle different pointer/register sizes for 32/64-bit code
 */

/*
 * Size of a register
 */
#ifdef __loongarch64
#define SZREG	8
#else
#define SZREG	4
#endif

/*
 * Use the following macros in assemblercode to load/store registers,
 * pointers etc.
 */
#if (_LOONGARCH_SIM == _LOONGARCH_SIM_ABILP64)
#define REG_S		st.d
#define REG_L		ld.d
#define REG_SUBU	sub.d
#define REG_ADDU	add.d
#endif

/*
 * How to add/sub/load/store/shift C int variables.
 */
#if (_LOONGARCH_SZINT == 32)
#define INT_ADDU	add.w
#define INT_ADDIU	addi.w
#define INT_SUBU	sub.w
#define INT_L		ld.w
#define INT_S		st.w
#define INT_SLL		slli.w
#define INT_SLLV	sll.w
#define INT_SRL		srli.w
#define INT_SRLV	srl.w
#define INT_SRA		srai.w
#define INT_SRAV	sra.w
#endif

#if (_LOONGARCH_SZINT == 64)
#define INT_ADDU	add.d
#define INT_ADDIU	addi.d
#define INT_SUBU	sub.d
#define INT_L		ld.d
#define INT_S		st.d
#define INT_SLL		slli.d
#define INT_SLLV	sll.d
#define INT_SRL		srli.d
#define INT_SRLV	srl.d
#define INT_SRA		sra.w
#define INT_SRAV	sra.d
#endif

/*
 * How to add/sub/load/store/shift C long variables.
 */
// #if (_LOONGARCH_SZLONG == 32)
// #define LONG_ADDU	add.w
// #define LONG_ADDIU	addi.w
// #define LONG_SUBU	sub.w
// #define LONG_L		ld.w
// #define LONG_S		st.w
// #define LONG_SP		swp
// #define LONG_SLL	slli.w
// #define LONG_SLLV	sll.w
// #define LONG_SRL	srli.w
// #define LONG_SRLV	srl.w
// #define LONG_SRA	srai.w
// #define LONG_SRAV	sra.w

// #ifdef __ASSEMBLER__
// #define LONG		.word
// #endif
// #define LONGSIZE	4
// #define LONGMASK	3
// #define LONGLOG		2
// #endif

#if (_LOONGARCH_SZLONG == 64)
#define LONG_ADDU	add.d
#define LONG_ADDIU	addi.d
#define LONG_SUBU	sub.d
#define LONG_L		ld.d
#define LONG_S		st.d
#define LONG_SP		sdp
#define LONG_SLL	slli.d
#define LONG_SLLV	sll.d
#define LONG_SRL	srli.d
#define LONG_SRLV	srl.d
#define LONG_SRA	sra.w
#define LONG_SRAV	sra.d

#ifdef __ASSEMBLER__
#define LONG		.dword
#endif
#define LONGSIZE	8
#define LONGMASK	7
#define LONGLOG		3
#endif

/*
 * How to add/sub/load/store/shift pointers.
 */
#if (_LOONGARCH_SZPTR == 32)
#define PTR_ADDU	add.w
#define PTR_ADDIU	addi.w
#define PTR_SUBU	sub.w
#define PTR_L		ld.w
#define PTR_S		st.w
#define PTR_LI		li.w
#define PTR_SLL		slli.w
#define PTR_SLLV	sll.w
#define PTR_SRL		srli.w
#define PTR_SRLV	srl.w
#define PTR_SRA		srai.w
#define PTR_SRAV	sra.w

#define PTR_SCALESHIFT	2

#define PTR		.word
#define PTRSIZE		4
#define PTRLOG		2
#endif

#if (_LOONGARCH_SZPTR == 64)
#define PTR_ADDU	add.d
#define PTR_ADDIU	addi.d
#define PTR_SUBU	sub.d
#define PTR_L		ld.d
#define PTR_S		st.d
#define PTR_LI		li.d
#define PTR_SLL		slli.d
#define PTR_SLLV	sll.d
#define PTR_SRL		srli.d
#define PTR_SRLV	srl.d
#define PTR_SRA		srai.d
#define PTR_SRAV	sra.d

#define PTR_SCALESHIFT	3

#define PTR		.dword
#define PTRSIZE		8
#define PTRLOG		3
#endif

#endif /* __ASM_ASM_H */
