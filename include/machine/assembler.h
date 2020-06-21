/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/* This file contains useful macros for assembly code. */

#ifdef __ASSEMBLER__

/*
 * Use BEGIN_FUNC(), END_FUNC() around assembly functions to annotate them
 * correctly to the assembler.
 */
#define BEGIN_FUNC(_name) \
    .global _name ; \
    .type _name, %function ; \
_name:

#define END_FUNC(_name) \
    .size _name, .-_name

/*
 * BEGIN_FUNC_STATIC() and END_FUNC_STATIC() do as above, but without making a
 * global declaration. (c.f. static functions in C).
 */
#define BEGIN_FUNC_STATIC(_name) \
    .type _name, %function ; \
_name:

#define END_FUNC_STATIC(_name) \
    .size _name, .-_name

#else /* !__ASSEMBLER__ */
#warning "Including assembly-specific header in C code"
#endif



