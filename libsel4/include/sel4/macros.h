/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_MACROS_H
#define __LIBSEL4_MACROS_H

/*
 * Some compilers attempt to pack enums into the smallest possible type.
 * For ABI compatability with the kernel, we need to ensure they remain
 * the same size as an 'int'.
 */
#define SEL4_FORCE_LONG_ENUM(type) \
        _enum_pad_ ## type = (1U << ((sizeof(int)*8) - 1))

#ifndef CONST
#define CONST        __attribute__((__const__))
#endif

#ifndef PURE
#define PURE         __attribute__((__pure__))
#endif

#endif
