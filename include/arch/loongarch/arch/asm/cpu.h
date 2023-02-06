/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * cpu.h: Values of the PRId register used to match up
 *	  various LoongArch cpu types.
 *
 * Copyright (C) 2020-2021 Loongson Technology Corporation Limited
 */
#ifndef _ASM_CPU_H
#define _ASM_CPU_H

/*
 * As of the LoongArch specs from Loongson Technology, the PRId register
 * (CPUCFG.00) is defined in this (backwards compatible) way:
 *
 * +----------------+----------------+----------------+----------------+
 * | Reserved       | Company ID	    | Processor ID   | Revision	      |
 * +----------------+----------------+----------------+----------------+
 *  31		 24 23		  16 15		    8 7              0
 *
 */

/*
 * Assigned Company values for bits 23:16 of the PRId register.
 */

#define PRID_COMP_MASK		0xff0000

#define PRID_COMP_LOONGSON	0x140000

/*
 * Assigned Processor ID (implementation) values for bits 15:8 of the PRId
 * register.  In order to detect a certain CPU type exactly eventually
 * additional registers may need to be examined.
 */

#define PRID_IMP_MASK		0xff00

#define PRID_IMP_LOONGSON_32	0x4200  /* Loongson 32bit */
#define PRID_IMP_LOONGSON_64R	0x6100  /* Reduced Loongson 64bit */
#define PRID_IMP_LOONGSON_64C	0x6300  /* Classic Loongson 64bit */
#define PRID_IMP_LOONGSON_64G	0xc000  /* Generic Loongson 64bit */
#define PRID_IMP_UNKNOWN	0xff00

/*
 * Particular Revision values for bits 7:0 of the PRId register.
 */

#define PRID_REV_MASK		0x00ff

#if !defined(__ASSEMBLY__)

enum cpu_type_enum {
	CPU_UNKNOWN,
	CPU_LOONGSON32,
	CPU_LOONGSON64,
	CPU_LAST
};

#endif /* !__ASSEMBLY */

/*
 * ISA Level encodings
 *
 */

#define LOONGARCH_CPU_ISA_LA32R 0x00000001
#define LOONGARCH_CPU_ISA_LA32S 0x00000002
#define LOONGARCH_CPU_ISA_LA64  0x00000004

#define LOONGARCH_CPU_ISA_32BIT (LOONGARCH_CPU_ISA_LA32R | LOONGARCH_CPU_ISA_LA32S)
#define LOONGARCH_CPU_ISA_64BIT LOONGARCH_CPU_ISA_LA64

/*
 * CPU Option encodings
 */
#define CPU_FEATURE_CPUCFG		0	/* CPU has CPUCFG */
#define CPU_FEATURE_LAM			1	/* CPU has Atomic instructions */
#define CPU_FEATURE_UAL			2	/* CPU has Unaligned Access support */
#define CPU_FEATURE_FPU			3	/* CPU has FPU */
#define CPU_FEATURE_LSX			4	/* CPU has 128bit SIMD instructions */
#define CPU_FEATURE_LASX		5	/* CPU has 256bit SIMD instructions */
#define CPU_FEATURE_COMPLEX		6	/* CPU has Complex instructions */
#define CPU_FEATURE_CRYPTO		7	/* CPU has Crypto instructions */
#define CPU_FEATURE_LVZ			8	/* CPU has Virtualization extension */
#define CPU_FEATURE_LBT_X86		9	/* CPU has X86 Binary Translation */
#define CPU_FEATURE_LBT_ARM		10	/* CPU has ARM Binary Translation */
#define CPU_FEATURE_LBT_MIPS		11	/* CPU has MIPS Binary Translation */
#define CPU_FEATURE_TLB			12	/* CPU has TLB */
#define CPU_FEATURE_CSR			13	/* CPU has CSR feature */
#define CPU_FEATURE_WATCH		14	/* CPU has watchpoint registers */
#define CPU_FEATURE_VINT		15	/* CPU has vectored interrupts */
#define CPU_FEATURE_CSRIPI		16	/* CPU has CSR-IPI */
#define CPU_FEATURE_EXTIOI		17	/* CPU has EXT-IOI */
#define CPU_FEATURE_PREFETCH		18	/* CPU has prefetch instructions */
#define CPU_FEATURE_PMP			19	/* CPU has perfermance counter */
#define CPU_FEATURE_SCALEFREQ		20	/* CPU support scale cpufreq */
#define CPU_FEATURE_FLATMODE		21	/* CPU has flatmode */
#define CPU_FEATURE_EIODECODE		22	/* CPU has extioi int pin decode mode */
#define CPU_FEATURE_GUESTID		23	/* CPU has GuestID feature */
#define CPU_FEATURE_HYPERVISOR		24	/* CPU has hypervisor (run in VM) */

#define LOONGARCH_CPU_CPUCFG		BIT_ULL(CPU_FEATURE_CPUCFG)
#define LOONGARCH_CPU_LAM		BIT_ULL(CPU_FEATURE_LAM)
#define LOONGARCH_CPU_UAL		BIT_ULL(CPU_FEATURE_UAL)
#define LOONGARCH_CPU_FPU		BIT_ULL(CPU_FEATURE_FPU)
#define LOONGARCH_CPU_LSX		BIT_ULL(CPU_FEATURE_LSX)
#define LOONGARCH_CPU_LASX		BIT_ULL(CPU_FEATURE_LASX)
#define LOONGARCH_CPU_COMPLEX		BIT_ULL(CPU_FEATURE_COMPLEX)
#define LOONGARCH_CPU_CRYPTO		BIT_ULL(CPU_FEATURE_CRYPTO)
#define LOONGARCH_CPU_LVZ		BIT_ULL(CPU_FEATURE_LVZ)
#define LOONGARCH_CPU_LBT_X86		BIT_ULL(CPU_FEATURE_LBT_X86)
#define LOONGARCH_CPU_LBT_ARM		BIT_ULL(CPU_FEATURE_LBT_ARM)
#define LOONGARCH_CPU_LBT_MIPS		BIT_ULL(CPU_FEATURE_LBT_MIPS)
#define LOONGARCH_CPU_TLB		BIT_ULL(CPU_FEATURE_TLB)
#define LOONGARCH_CPU_CSR		BIT_ULL(CPU_FEATURE_CSR)
#define LOONGARCH_CPU_WATCH		BIT_ULL(CPU_FEATURE_WATCH)
#define LOONGARCH_CPU_VINT		BIT_ULL(CPU_FEATURE_VINT)
#define LOONGARCH_CPU_CSRIPI		BIT_ULL(CPU_FEATURE_CSRIPI)
#define LOONGARCH_CPU_EXTIOI		BIT_ULL(CPU_FEATURE_EXTIOI)
#define LOONGARCH_CPU_PREFETCH		BIT_ULL(CPU_FEATURE_PREFETCH)
#define LOONGARCH_CPU_PMP		BIT_ULL(CPU_FEATURE_PMP)
#define LOONGARCH_CPU_SCALEFREQ		BIT_ULL(CPU_FEATURE_SCALEFREQ)
#define LOONGARCH_CPU_FLATMODE		BIT_ULL(CPU_FEATURE_FLATMODE)
#define LOONGARCH_CPU_EIODECODE		BIT_ULL(CPU_FEATURE_EIODECODE)
#define LOONGARCH_CPU_GUESTID		BIT_ULL(CPU_FEATURE_GUESTID)
#define LOONGARCH_CPU_HYPERVISOR	BIT_ULL(CPU_FEATURE_HYPERVISOR)
#endif /* _ASM_CPU_H */
