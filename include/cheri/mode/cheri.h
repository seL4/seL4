/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2019 Hesham Almatary
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/**
 * @file
 *
 * @brief CHERI utility functions
 */

#ifndef _CHERI_UTILITY_H
#define _CHERI_UTILITY_H

#include <types.h>
#include <linker.h>

void *cheri_seal_cap(void *unsealed_cap, size_t otype);
void *cheri_unseal_cap(void *unsealed_cap);
void *cheri_build_data_cap(ptraddr_t address, size_t size, size_t perms);
void *cheri_build_device_cap(ptraddr_t address, size_t size);
void *cheri_build_user_cap(ptraddr_t address, size_t size, size_t perms);
void *cheri_build_code_cap(ptraddr_t address, size_t size, size_t perms);
void *cheri_build_code_cap_unbounded(ptraddr_t address, size_t perms);
void *cheri_derive_data_cap(void *src, ptraddr_t address, size_t size, size_t perms);
void *cheri_derive_code_cap(void *src, ptraddr_t address, size_t size, size_t perms);
void cheri_print_cap(const void *cap);

extern __uintcap_t KernelVirtOffsetCap;
void _start_purecap(void);
#endif /* ifndef __CHERI__UTILITY_H */
