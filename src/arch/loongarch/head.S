/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 * 
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <util.h>

#include <arch/asm/asm.h>
#include <arch/asm/regdef.h>
#include <arch/asm/addrspace.h>
#include <arch/machine.h>


.section .boot.text, "ax"
.global _start
.extern kernel_stack_alloc


_start:                   # kernel entry point
	la.abs		t0, 0f
	jirl		zero, t0, 0

#	li.d    	t0, 0x5000000000000009 # no cache for uart, PLV0|PLV3, for hardware platforms
#	csrwr   	t0, LOONGARCH_CSR_DMWIN2
0:
	la			t0, __bss_start		# clear .bss
	st.d		zero, t0, 0
	la 			t1, __bss_stop
	li.d		t2, LONGSIZE
	sub.d		t1, t1, t2
1:
	addi.d		t0, t0, LONGSIZE
	stptr.d		zero, t0, 0
	bne			t0, t1, 1b

	la sp, kernel_stack_alloc
	addi.w t0, zero, 1
	slli.w t0, t0, CONFIG_KERNEL_STACK_BITS
	add.d sp, sp, t0
	csrwr zero, LOONGARCH_CSR_KS3 /* zero LOONGARCH_CSR_KS3 for the init task */
	bl		init_kernel

	la ra, restore_user_context
	jirl zero, ra, 0
