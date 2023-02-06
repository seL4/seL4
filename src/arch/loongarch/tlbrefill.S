/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 * 
 * Derived from:
 * Copyright (C) 2020-2021 Loongson Technology Corporation Limited
 *
 * SPDX-License-Identifier: GPL-2.0
 */

 #include <arch/machine.h>
 #include <arch/machine/hardware.h>
 #include <hardware.h>

.section kernel_tlbrefillentry
.global handle_tlb_refill
handle_tlb_refill:
	csrwr	$t0, LOONGARCH_CSR_TLBRSAVE
	csrrd	$t0, LOONGARCH_CSR_ASID
	csrrd	$t0, LOONGARCH_CSR_PGD
	lddir	$t0, $t0, 3
	beq     $t0, $r0, refill_unmap
	lddir	$t0, $t0, 1
	beq     $t0, $r0, refill_unmap
	ldpte	$t0, 0
	ldpte	$t0, 1
	tlbfill
	csrrd   $t0, LOONGARCH_CSR_TLBREHI
	csrrd   $t0, LOONGARCH_CSR_TLBRELO0
	csrrd   $t0, LOONGARCH_CSR_TLBRELO1
	csrrd	$t0, LOONGARCH_CSR_TLBRSAVE
	ertn
refill_unmap:
	csrwr	$r0, LOONGARCH_CSR_TLBRELO0
	csrwr	$r0, LOONGARCH_CSR_TLBRELO1
	tlbfill
	csrrd	$t0, LOONGARCH_CSR_TLBRSAVE
	ertn
	
