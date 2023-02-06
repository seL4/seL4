/* SPDX-License-Identifier: GPL-2.0-only */
/*Copyright (C) 2020-2021 Loongson Technology Corporation Limited*/
#ifndef __ASM_LINKAGE_H
#define __ASM_LINKAGE_H

#define ALIGN __ALIGN
#define __ALIGN		.align 2
#define __ALIGN_STR	".align 2"
#define SYM_L_GLOBAL(name)			.globl name
#define SYM_A_ALIGN				ALIGN

#ifndef ASM_NL
#define ASM_NL		 ;
#endif
#define SYM_T_FUNC				STT_FUNC

#ifdef __ASSEMBLER__
#define _ULCAST_
#else
#define _ULCAST_ (unsigned long)
#endif


#define STT_NOTYPE  0
#define STT_OBJECT  1
#define STT_FUNC    2
#define STT_SECTION 3
#define STT_FILE    4
#define STT_COMMON  5
#define STT_TLS     6
#define PGDIR_SHIFT		28
#define PGD_ORDER		0
#define PUD_ORDER		0
#define PMD_ORDER		0
#define PTE_ORDER		0
#define PAGE_SHIFT	14
#define	_PAGE_HUGE_SHIFT	6  /* HUGE is a PMD bit */
#define	_PAGE_GLOBAL_SHIFT	6
#define	_PAGE_HGLOBAL_SHIFT	12 /* HGlobal is a PMD bit */
#define	_PAGE_PRESENT_SHIFT	7
#define	_PAGE_VALID_SHIFT	0
#define	_PAGE_DIRTY_SHIFT	1
#define	_PAGE_PRESENT_SHIFT	7
#define	_PAGE_WRITE_SHIFT	8
#define	_PAGE_MODIFIED_SHIFT	9
/* Default huge tlb size for a given kernel configuration */
#define PS_HUGE_SIZE   PS_16M


#define PTRS_PER_PMD	((PAGE_SIZE << PMD_ORDER) >> 3)
#define PTRS_PER_PGD	((PAGE_SIZE << PGD_ORDER) >> 3)
#define PMD_SHIFT	(PAGE_SHIFT + (PAGE_SHIFT + PTE_ORDER - 3))
#define PTRS_PER_PTE	((PAGE_SIZE << PTE_ORDER) >> 3)
#define _PAGE_PRESENT		(_ULCAST_(1) << _PAGE_PRESENT_SHIFT)
#define _PAGE_WRITE		(_ULCAST_(1) << _PAGE_WRITE_SHIFT)
#define _PAGE_DIRTY		(_ULCAST_(1) << _PAGE_DIRTY_SHIFT)
#define _PAGE_MODIFIED		(_ULCAST_(1) << _PAGE_MODIFIED_SHIFT)
#define HPAGE_SHIFT	(PAGE_SHIFT + PAGE_SHIFT - 3)
#define PAGE_SIZE	(UL_CONST(1) << PAGE_SHIFT)
#define _PAGE_VALID		(_ULCAST_(1) << _PAGE_VALID_SHIFT)
#define _PAGE_HUGE		(_ULCAST_(1) << _PAGE_HUGE_SHIFT)
#define _PAGE_HGLOBAL		(_ULCAST_(1) << _PAGE_HGLOBAL_SHIFT)


#define SYM_END(name, sym_type)				\
	.type name sym_type ASM_NL			\
	.size name, .-name

#define SYM_ENTRY(name, linkage, align...)		\
	linkage(name) ASM_NL				\
	align ASM_NL					\
	name:

#define SYM_START(name, linkage, align...)		\
	SYM_ENTRY(name, linkage, align)

#define SYM_FUNC_START(name)				\
	SYM_START(name, SYM_L_GLOBAL, SYM_A_ALIGN)	\
	.cfi_startproc;

#define SYM_FUNC_START_NOALIGN(name)			\
	SYM_START(name, SYM_L_GLOBAL, SYM_A_NONE)	\
	.cfi_startproc;

#define SYM_FUNC_START_LOCAL(name)			\
	SYM_START(name, SYM_L_LOCAL, SYM_A_ALIGN)	\
	.cfi_startproc;

#define SYM_FUNC_START_LOCAL_NOALIGN(name)		\
	SYM_START(name, SYM_L_LOCAL, SYM_A_NONE)	\
	.cfi_startproc;

#define SYM_FUNC_START_WEAK(name)			\
	SYM_START(name, SYM_L_WEAK, SYM_A_ALIGN)	\
	.cfi_startproc;

#define SYM_FUNC_START_WEAK_NOALIGN(name)		\
	SYM_START(name, SYM_L_WEAK, SYM_A_NONE)		\
	.cfi_startproc;

#define SYM_FUNC_END(name)				\
	.cfi_endproc;					\
	SYM_END(name, SYM_T_FUNC)

#endif
