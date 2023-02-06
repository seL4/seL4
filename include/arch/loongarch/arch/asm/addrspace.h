/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Copyright (C) 2020-2021 Loongson Technology Corporation Limited
 */
#ifndef _ASM_ADDRSPACE_H
#define _ASM_ADDRSPACE_H

#include <arch/machine.h>

/*
 * This gives the physical RAM offset.
 */
#ifndef __ASSEMBLER__
#ifndef PHYS_OFFSET
#define PHYS_OFFSET	UL_CONST(0, UL)
#endif
extern unsigned long vm_map_base;
#endif /* __ASSEMBLER__ */

#ifndef IO_BASE
#define IO_BASE			CSR_DMW0_BASE
#endif

#ifndef CAC_BASE
#define CAC_BASE		CSR_DMW1_BASE
#endif

#ifndef UNCAC_BASE
#define UNCAC_BASE		CSR_DMW0_BASE
#endif

#define DMW_PABITS	48
#define TO_PHYS_MASK	((1ULL << DMW_PABITS) - 1)

/*
 * Memory above this physical address will be considered highmem.
 */
#ifndef HIGHMEM_START
#define HIGHMEM_START		(UL_CONST(1, UL) << UL_CONST(DMW_PABITS, UL))
#endif

#define TO_PHYS(x)		(	      ((x) & TO_PHYS_MASK))
#define TO_CAC(x)		(CAC_BASE   | ((x) & TO_PHYS_MASK))
#define TO_UNCAC(x)		(UNCAC_BASE | ((x) & TO_PHYS_MASK))

/*
 * This handles the memory map.
 */
#ifndef PAGE_OFFSET
#define PAGE_OFFSET		(CAC_BASE + PHYS_OFFSET)
#endif

#ifndef FIXADDR_TOP
#define FIXADDR_TOP		((unsigned long)(long)(int)0xfffe0000)
#endif

/*
 *  Configure language
 */
#ifdef __ASSEMBLER__
#define _ATYPE_
#define _ATYPE32_
#define _ATYPE64_
#define _CONST64_(x)	x
#else
#define _ATYPE_		__PTRDIFF_TYPE__
#define _ATYPE32_	int
#define _ATYPE64_	__s64
#ifdef CONFIG_64BIT
#define _CONST64_(x)	x ## L
#else
#define _CONST64_(x)	x ## LL
#endif
#endif

/*
 *  32/64-bit LoongArch address spaces
 */
#ifdef __ASSEMBLER__
#define _ACAST32_
#define _ACAST64_
#else
#define _ACAST32_		(_ATYPE_)(_ATYPE32_)	/* widen if necessary */
#define _ACAST64_		(_ATYPE64_)		/* do _not_ narrow */
#endif

#ifdef CONFIG_32BIT

#define UVRANGE			0x00000000
#define KPRANGE0		0x80000000
#define KPRANGE1		0xa0000000
#define KVRANGE			0xc0000000

#else

#define XUVRANGE		ULL_CONST(0x0000000000000000)
#define XSPRANGE		ULL_CONST(0x4000000000000000)
#define XKPRANGE		ULL_CONST(0x8000000000000000)
#define XKVRANGE		ULL_CONST(0xc000000000000000)

#endif

/*
 * Returns the physical address of a KPRANGEx / XKPRANGE address
 */
#define PHYSADDR(a)		((_ACAST64_(a)) & TO_PHYS_MASK)

#endif /* _ASM_ADDRSPACE_H */
