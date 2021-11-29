/* SPDX-License-Identifier: BSD-3-Clause */

/* Copyright (c) 2010-2017, The Regents of the University of California
 * (Regents).  All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the Regents nor the
 * names of its contributors may be used to endorse or promote products
 * derived from this software without specific prior written permission.
 *
 * IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING
 * OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF REGENTS HAS
 * BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
 * HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION TO PROVIDE
 * MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* This file is copied from RISC-V tools/linux project, it might change for
 * new spec releases.
 */

#pragma once

#include <config.h>
#include <stdint.h>

/* See https://github.com/riscv/riscv-sbi-doc/blob/master/riscv-sbi.adoc for
 * details about these command codes, they are the "legacy extensions"
 * introduced by BBL and supported by OpenSBI.
 */
#define SBI_SET_TIMER 0
#define SBI_CONSOLE_PUTCHAR 1
#define SBI_CONSOLE_GETCHAR 2
#define SBI_CLEAR_IPI 3
#define SBI_SEND_IPI 4
#define SBI_REMOTE_FENCE_I 5
#define SBI_REMOTE_SFENCE_VMA 6
#define SBI_REMOTE_SFENCE_VMA_ASID 7
#define SBI_SHUTDOWN 8
/* The values 9 - 15 are reserved. */

static inline word_t sbi_call(word_t cmd,
                              word_t arg_0,
                              word_t arg_1,
                              word_t arg_2)
{
    register word_t a0 asm("a0") = arg_0;
    register word_t a1 asm("a1") = arg_1;
    register word_t a2 asm("a2") = arg_2;
    register word_t a7 asm("a7") = cmd;
    register word_t result asm("a0");
    asm volatile("ecall"
                 : "=r"(result)
                 : "r"(a0), "r"(a1), "r"(a2), "r"(a7)
                 : "memory");
    return result;
}

/* Lazy implementations until SBI is finalized */
#define SBI_CALL_0(which) sbi_call(which, 0, 0, 0)
#define SBI_CALL_1(which, arg0) sbi_call(which, arg0, 0, 0)
#define SBI_CALL_2(which, arg0, arg1) sbi_call(which, arg0, arg1, 0)

static inline void sbi_console_putchar(int ch)
{
    SBI_CALL_1(SBI_CONSOLE_PUTCHAR, ch);
}

static inline int sbi_console_getchar(void)
{
    return (int)(SBI_CALL_0(SBI_CONSOLE_GETCHAR));
}

static inline void sbi_set_timer(unsigned long long stime_value)
{
#if __riscv_xlen == 32
    SBI_CALL_2(SBI_SET_TIMER, stime_value, stime_value >> 32);
#else
    SBI_CALL_1(SBI_SET_TIMER, stime_value);
#endif
}

static inline void sbi_shutdown(void)
{
    SBI_CALL_0(SBI_SHUTDOWN);
}

#ifdef ENABLE_SMP_SUPPORT

static inline void sbi_clear_ipi(void)
{
    SBI_CALL_0(SBI_CLEAR_IPI);
}

static inline void sbi_send_ipi(word_t hart_mask)
{
    /* ToDo: In the legacy SBI API the hart mask parameter is not a value, but
     *       the virtual address of a bit vector. This was intended to allow
     *       passing an arbitrary number of harts without being limited by the
     *       architecture's word size. We hide this feature here because:
     *       - All systems currently supported by the kernel have less harts
     *           than bits in a word. Support for more harts would requires
     *           reworking parts of the kernel.
     *       - The legacy SBI interface is deprecated. Passing pointers with
     *           virtual addresses has several practical drawbacks or corner
     *           cases, these outweight the gain of being able to address all
     *           harts in one call. The new interface uses a window concept and
     *           passes plain values.
     *       - Using pointers to local variables is perfectly fine in C, but
     *           verification does not support this, because passing pointers to
     *           stack objects is not allowed. However, verification does not
     *           run for the RISC-V SMP configuration at the moment, thus
     *           keeping this detail hidden here allows postponing finding a
     *           solution and higher layers are kept agnostic of this.
     */
    word_t virt_addr_hart_mask = (word_t)&hart_mask;
    SBI_CALL_1(SBI_SEND_IPI, virt_addr_hart_mask);
}

static inline void sbi_remote_fence_i(word_t hart_mask)
{
    /* See comment at sbi_send_ipi() about the pointer parameter. */
    word_t virt_addr_hart_mask = (word_t)&hart_mask;
    SBI_CALL_1(SBI_REMOTE_FENCE_I, virt_addr_hart_mask);
}

static inline void sbi_remote_sfence_vma(word_t hart_mask,
                                         unsigned long start,
                                         unsigned long size)
{
    /* See comment at sbi_send_ipi() about the pointer parameter. */
    word_t virt_addr_hart_mask = (word_t)&hart_mask;
    SBI_CALL_1(SBI_REMOTE_SFENCE_VMA, virt_addr_hart_mask);
}

static inline void sbi_remote_sfence_vma_asid(word_t hart_mask,
                                              unsigned long start,
                                              unsigned long size,
                                              unsigned long asid)
{
    /* See comment at sbi_send_ipi() about the pointer parameter. */
    word_t virt_addr_hart_mask = (word_t)&hart_mask;
    SBI_CALL_1(SBI_REMOTE_SFENCE_VMA_ASID, virt_addr_hart_mask);
}

#endif /* ENABLE_SMP_SUPPORT */
