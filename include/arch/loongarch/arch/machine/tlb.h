/*
 * Copyright 2022, tyyteam
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#pragma once

#include <arch/machine.h>

extern void init_tlb(void);

void local_flush_tlb_all(void);
void setup_tlb_handler(void);

/*
 * TLB Invalidate Flush
 */
static inline void tlbclr(void)
{
	__asm__ volatile("tlbclr");
}

static inline void tlbflush(void)
{
	__asm__ volatile("tlbflush");
}

/*
 * TLB R/W operations.
 */
static inline void tlb_probe(void)
{
	__asm__ volatile("tlbsrch");
}

static inline void tlb_read(void)
{
	__asm__ volatile("tlbrd");
}

static inline void tlb_write_indexed(void)
{
	__asm__ volatile("tlbwr");
}

static inline void tlb_write_random(void)
{
	__asm__ volatile("tlbfill");
}

/*
 * Guest TLB Invalidate Flush
 */
static inline void guest_tlbflush(void)
{
	__asm__ volatile(
		".word 0x6482401\n\t");
}

/*
 * Guest TLB R/W operations.
 */
static inline void guest_tlb_probe(void)
{
	__asm__ volatile(
		".word 0x6482801\n\t");
}

static inline void guest_tlb_read(void)
{
	__asm__ volatile(
		".word 0x6482c01\n\t");
}

static inline void guest_tlb_write_indexed(void)
{
	__asm__ volatile(
		".word 0x6483001\n\t");
}

static inline void guest_tlb_write_random(void)
{
	__asm__ volatile(
		".word 0x6483401\n\t");
}

enum invtlb_ops {
	/* Invalid all tlb */
	INVTLB_ALL = 0x0,
	/* Invalid current tlb */
	INVTLB_CURRENT_ALL = 0x1,
	/* Invalid all global=1 lines in current tlb */
	INVTLB_CURRENT_GTRUE = 0x2,
	/* Invalid all global=0 lines in current tlb */
	INVTLB_CURRENT_GFALSE = 0x3,
	/* Invalid global=0 and matched asid lines in current tlb */
	INVTLB_GFALSE_AND_ASID = 0x4,
	/* Invalid addr with global=0 and matched asid in current tlb */
	INVTLB_ADDR_GFALSE_AND_ASID = 0x5,
	/* Invalid addr with global=1 or matched asid in current tlb */
	INVTLB_ADDR_GTRUE_OR_ASID = 0x6,
	/* Invalid matched gid in guest tlb */
	INVGTLB_GID = 0x9,
	/* Invalid global=1, matched gid in guest tlb */
	INVGTLB_GID_GTRUE = 0xa,
	/* Invalid global=0, matched gid in guest tlb */
	INVGTLB_GID_GFALSE = 0xb,
	/* Invalid global=0, matched gid and asid in guest tlb */
	INVGTLB_GID_GFALSE_ASID = 0xc,
	/* Invalid global=0 , matched gid, asid and addr in guest tlb */
	INVGTLB_GID_GFALSE_ASID_ADDR = 0xd,
	/* Invalid global=1 , matched gid, asid and addr in guest tlb */
	INVGTLB_GID_GTRUE_ASID_ADDR = 0xe,
	/* Invalid all gid gva-->gpa guest tlb */
	INVGTLB_ALLGID_GVA_TO_GPA = 0x10,
	/* Invalid all gid gpa-->hpa tlb */
	INVTLB_ALLGID_GPA_TO_HPA = 0x11,
	/* Invalid all gid tlb, including  gva-->gpa and gpa-->hpa */
	INVTLB_ALLGID = 0x12,
	/* Invalid matched gid gva-->gpa guest tlb */
	INVGTLB_GID_GVA_TO_GPA = 0x13,
	/* Invalid matched gid gpa-->hpa tlb */
	INVTLB_GID_GPA_TO_HPA = 0x14,
	/* Invalid matched gid tlb,including gva-->gpa and gpa-->hpa */
	INVTLB_GID_ALL = 0x15,
	/* Invalid matched gid and addr gpa-->hpa tlb */
	INVTLB_GID_ADDR = 0x16,
};

/*
 * invtlb op info addr
 * (0x1 << 26) | (0x24 << 20) | (0x13 << 15) |
 * (addr << 10) | (info << 5) | op
 */
static inline void invtlb(uint32_t op, uint32_t info, uint64_t addr)
{
	__asm__ volatile(
		"parse_r addr,%0\n\t"
		"parse_r info,%1\n\t"
		".word ((0x6498000) | (addr << 10) | (info << 5) | %2)\n\t"
		:
		: "r"(addr), "r"(info), "i"(op)
		);
}

static inline void invtlb_addr(uint32_t op, uint32_t info, uint64_t addr)
{
	__asm__ volatile(
		"parse_r addr,%0\n\t"
		".word ((0x6498000) | (addr << 10) | (0 << 5) | %1)\n\t"
		:
		: "r"(addr), "i"(op)
		);
}

static inline void invtlb_info(uint32_t op, uint32_t info, uint64_t addr)
{
	__asm__ volatile(
		"parse_r info,%0\n\t"
		".word ((0x6498000) | (0 << 10) | (info << 5) | %1)\n\t"
		:
		: "r"(info), "i"(op)
		);
}

static inline void invtlb_all(uint32_t op, uint32_t info, uint64_t addr)
{
	__asm__ volatile(
		".word ((0x6498000) | (0 << 10) | (0 << 5) | %0)\n\t"
		:
		: "i"(op)
		);
}

/*
 * LoongArch doesn't need any special per-pte or per-vma handling, except
 * we need to flush cache for area to be unmapped.
 */
#define tlb_start_vma(tlb, vma)					\
	do {							\
		if (!(tlb)->fullmm)				\
			flush_cache_range(vma, vma->vm_start, vma->vm_end); \
	}  while (0)
#define tlb_end_vma(tlb, vma) do { } while (0)
#define __tlb_remove_tlb_entry(tlb, ptep, address) do { } while (0)

extern void handle_tlb_load(void);
extern int handle_tlb_store(void);
extern void handle_tlb_modify(void);
extern void handle_tlb_refill(void);
extern void handle_tlb_protect(void);

extern void dump_tlb_all(void);
extern void dump_tlb_regs(void);
