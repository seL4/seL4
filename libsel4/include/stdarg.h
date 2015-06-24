/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __STDARG_H
#define __STDARG_H

#ifndef va_start
#define va_start(v,l) __builtin_va_start(v,l)
#endif

#ifndef va_end
#define va_end(v) __builtin_va_end(v)
#endif

#ifndef va_arg
#define va_arg(v,l) __builtin_va_arg(v,l)
#endif

#ifndef va_list
typedef __builtin_va_list va_list;
#endif

#endif
